/* Copyright 2012-2019 Dustin DeWeese
   This file is part of PoprC.

    PoprC is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PoprC is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PoprC.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "rt_types.h"
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "startle/error.h"
#include "startle/log.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "byte_compile.h"
#include "parse.h"
#include "print.h"
#include "cgen.h"
#include "vlgen.h"
#include "eval.h"
#include "lex.h"
#include "user_func.h"
#include "trace.h"

// table of corresponding Verilog types
static
const char *vltype(type_t t) {
  static const char *table[] = {
    [T_ANY]      = "any",
    [T_INT]      = "int",
    [T_LIST]     = "stream",
    [T_SYMBOL]   = "sym",
    [T_MAP]      = "map",
    [T_STRING]   = "string",
    [T_FLOAT]    = "real",
    [T_OPAQUE]   = "opaque",
    [T_BOTTOM]   = "void",
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

static
void gen_module_interface(const cell_t *e, const type_t *rtypes) {
  const cell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  bool sync = FLAG(*e, entry, SYNC);

  printf("module %s_%s (\n", e->module_name, e->word_name);
  char *sep = "";
  if(sync) {
    printf("  `sync_ports");
    sep = ",\n";
  }
  COUNTDOWN(i, e->entry.in) {
    const cell_t *a = &p[i];
    printf("%s  `input(%s, %d)", sep, vltype(trace_type(a)), (int)(e->entry.in - 1 - i));
    sep = ",\n";
  }

  COUNTUP(i, out_n) {
    printf("%s  `output(%s, %d)", sep, vltype(rtypes[i]), (int)i);
  }

  printf("\n);\n");
}

static
void gen_value_rhs(const cell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("%d", (int)c->value.integer);
    break;
  case T_STRING: {
    seg_t str = value_seg(c);
    printf("\"");
    print_escaped_string(str);
    printf("\"");
    break;
  }
  default:
    assert_error(false); // TODO add more types
  }
}

static bool is_sync(const cell_t *c) {
  if(c->op == OP_ap) return true;
  cell_t *entry = get_entry(c);
  return entry && FLAG(*entry, entry, SYNC);
}

static
void gen_decls(cell_t *e) {
  FOR_TRACE_CONST(c, e) {
    int i = c - e;
    type_t t = trace_type(c);
    if(ONEOF(t, T_BOTTOM, T_RETURN)) continue;
    if(is_var(c)) {
      const char *decl = e->entry.rec ? "variable" : "alias";
      printf("  `%s(%s, %s%d, in%d);\n", decl, vltype(t), cname(t), i, e->entry.in - i);
    } else {
      if(c->op == OP_value) {
        printf("  localparam `%sT %s%d = ", vltype(t), cname(t), i);
        gen_value_rhs(c);
        printf(";\n");
      } else if(NOT_FLAG(*c, trace, TAIL_CALL)) {
        printf("  `wire(%s, %s%d);\n", vltype(t), cname(t), i);
      }
    }
  }
}

static
void gen_instance(const cell_t *e, const cell_t *c, int *sync_chain) {
  int inst = c - e;
  char *sep = "";
  const char *module_name, *word_name;
  bool sync = is_sync(c);
  csize_t
    in = closure_in(c),
    n = closure_args(c),
    start_out = n - closure_out(c);

  trace_get_name(c, &module_name, &word_name);
  type_t t = trace_type(c);

  printf("    `inst%s(", sync ? "_sync" : "");
  print_function_name(e, c);
  printf(", inst%d)(", inst);

  if(sync) printf("\n      `sync, ");

  COUNTUP(i, in) {
    int a = cgen_index(e, c->expr.arg[i]);
    type_t at = trace_type(&e[a]);
    printf("%s\n      `in(%s, %d, %s%d)",
           sep, vltype(at), (int)i, cname(at), a);
    sep = ", ";
  };
  printf("%s\n      `out(%s, 0, %s%d)",
         sep, vltype(t), cname(t), inst);
  RANGEUP(i, start_out, n) {
    int a = cgen_index(e, c->expr.arg[i]);
    if(a > 0) {
      type_t at = trace_type(&e[a]);
      printf(",\n      `out(%s, %d, %s%d)",
             vltype(at), (int)(i - start_out + 1), cname(at), a);
    }
  }

  printf("\n    );\n");
  if(sync) *sync_chain = inst;
}

#if LOCAL_INTERFACE
typedef enum reduction_type {
  RT_VALID = 0,
  RT_READY = 1
} reduction_type;
#endif

static
void gen_reduction(cell_t *e, int block, reduction_type type) {
  const char *stype = type == RT_VALID ? "valid" : "ready";
  printf("    wire block%d_%s = ", block, stype);
  const char *sep = "";
  FOR_TRACE_CONST(c, e, block) {
    if(is_return(c)) break;
    if((is_sync(c) && NOT_FLAG(*c, trace, TAIL_CALL)) || (type == RT_VALID && c->op == OP_assert)) {
      printf("%sblock%d_inst%d_%s", sep, block, (int)(c - e), stype);
      sep = " & ";
    }
  }
  if(!sep[0]) printf("`true");
  printf(";\n");
}

static
void gen_body(cell_t *e) {
  bool block_start = true;
  int block = 1;
  int block_sync_chain = 0;
  FOR_TRACE_CONST(c, e) {
    if(!is_value(c)) {
      if(!ONEOF(c->op, OP_dep, OP_seq, OP_unless)) {
        if(get_entry(c) == e) {
          assert_error(last_call(e, c));
          continue;
        }
        if(block_start) {
          printf("\n  `define block block%d\n", block);
          block_start = false;
          block_sync_chain = 0;
        }
        if(c->op == OP_assert) {
          int ai = cgen_index(e, c->expr.arg[1]);
          printf("    `assert(inst%d, sym%d);\n", (int)(c - e), ai);
        } else {
          gen_instance(e, c, &block_sync_chain);
        }
      }
    } else if(is_return(c)) {
      if(block_start) {
        printf("\n  `define block block%d\n", block);
      }
      gen_reduction(e, block, RT_VALID);
      gen_reduction(e, block, RT_READY);
      printf("  `undef block\n");
      block = (c - e) + calculate_cells(c->size);
      block_start = true;
    }
  }
}

// TODO clean up
static
void gen_outputs(const cell_t *e, const cell_t *r0, const type_t *rtypes) {
  csize_t out = e->entry.out;
  COUNTUP(i, out) {
    type_t t = rtypes[i];
    printf("  assign out%d = ", (int)i);
    const cell_t *r = r0;
    const cell_t *r_prev = NULL;
    int block = 1;
    do {
      if(NOT_FLAG(*r, trace, TAIL_CALL)) {
        if(r_prev) {
          printf("\n      block%d_valid ? %s%d : ",
                 block, cname(t), cgen_index(e, r_prev->value.ptr[REVI(i)]));
          block = (r - e) + calculate_cells(r->size);
        }
        r_prev = r;
      }
      r = &e[tr_index(r->alt)];
    } while(r > e);
    assert_error(r_prev);
    printf("%s%d;\n", cname(t), cgen_index(e, r_prev->value.ptr[REVI(i)]));
  }
}

static
void gen_valid_ready(const cell_t *e, const cell_t *r0) {
  printf("  reg active = `false;\n");

  // valid
  printf("  wire valid = ");
  const char *sep = "";
  int block = 1;
  for(const cell_t *r = r0; r > e; r = &e[tr_index(r->alt)]) {
    if(FLAG(*r, trace, TAIL_CALL)) {
      printf("%s!block%d_valid", sep, block);
      sep = " & ";
    }
    block = (r - e) + calculate_cells(r->size);
  }
  if(!sep[0]) printf("`true");
  printf(";\n"
         "  assign out_valid = active & valid;\n");

  // ready
  printf("  assign in_ready = ");
  sep = "";
  block = 1;
  for(const cell_t *r = r0; r > e; r = &e[tr_index(r->alt)]) {
    printf("%sblock%d_ready", sep, block);
    sep = " & ";
    block = (r - e) + calculate_cells(r->size);
  }
  if(!sep[0]) printf("`true");
  printf(";\n");
}

static
void gen_loop(const cell_t *e, int block, const cell_t *tail_call) {
  csize_t in = e->entry.in;
  printf("      if(block%d_valid) begin\n", block);
  COUNTUP(i, in) {
    int a = cgen_index(e, tail_call->expr.arg[i]);
    type_t t = trace_type(&e[a]);
    assert_error(trace_type(&e[in - i]) == t);
    if(t != T_LIST) {
      printf("        %s%d <= %s%d;\n",
             cname(t), (int)(in - i),
             cname(t), a);
    }
  }
  printf("      end\n");
}

static
void gen_loops(cell_t *e) {
  printf("  always @(posedge clk) begin\n"
         "    if(in_valid) begin\n");
  FOR_TRACE_CONST(c, e) {
    if(!is_var(c)) break;
    int i = c - e;
    type_t t = trace_type(c);
    if(t != T_LIST) printf("      %s%d <= in%d;\n", cname(t), i, e->entry.in - i);
  }
  printf("      `set(active);\n"
         "    end\n"
         "    else if(valid) begin\n"
         "       if(out_ready) `reset(active);\n"
         "    end\n"
         "    else begin\n");
  int start = 1;
  const cell_t *tail_call = NULL;
  FOR_TRACE_CONST(c, e) {
    if(is_return(c)) {
      if(tail_call) gen_loop(e, start, tail_call);
      start = (c - e) + calculate_cells(c->size);
      tail_call = NULL;
    } else if(get_entry(c) == e) {
      tail_call = c;
    }
  }
  printf("    end\n"
         "  end\n");
}

static
void gen_stream_loops(cell_t *e) {
  const csize_t in = e->entry.in;
  const cell_t *tail_call;
  const char *sep;
  int start;
  COUNTUP(i, in) {
    if(trace_type(&e[in - i]) != T_LIST) continue;
    sep = "";
    tail_call = NULL;
    start = 1;
    printf("  assign lst%d_loop = ", (int)(in - i));
    FOR_TRACE_CONST(c, e) {
      if(is_return(c)) {
        if(tail_call) {
          int a = cgen_index(e, tail_call->expr.arg[i]);
          assert_error(trace_type(&e[a]) == T_LIST);
          printf("%slst%d_valid", sep, a);
          sep = " | ";
        }
        start = (c - e) + calculate_cells(c->size);
        tail_call = NULL;
      } else if(get_entry(c) == e) {
        tail_call = c;
      }
    }
    if(!sep[0]) printf("`false");
    printf(";\n");
  }
}

static
void gen_module(cell_t *e) {
  e->op = OP_value;
  const cell_t *r0 = NULL;
  FOR_TRACE_CONST(c, e) {
    if(is_return(c)) {
      r0 = c;
      break;
    }
  }
  type_t rtypes[e->entry.out];
  resolve_types(e, rtypes);
  gen_module_interface(e, rtypes);
  printf("\n");
  gen_decls(e);
  gen_body(e);
  printf("\n");
  if(e->entry.rec) {
    gen_valid_ready(e, r0);
    printf("\n");
    gen_loops(e);
    printf("\n");
    gen_stream_loops(e);
    printf("\n");
  }
  gen_outputs(e, r0, rtypes);
  printf("\nendmodule\n");

  FOR_TRACE(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      cell_t *x = get_entry(c);
      if(x != e && x->op == OP_null) {
        printf("\n");
        gen_module(x);
      }
    }
  }
}

COMMAND(cv, "print Verilog code for given function") {
  if(rest) {
    command_define(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) {
      gen_module(e);
      clear_ops(e);
    }
  }
  if(command_line) quit = true;
}
