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
    printf("  input clk");
    sep = ",\n";
  }
  COUNTDOWN(i, e->entry.in) {
    const cell_t *a = &p[i];
    type_t t = trace_type(a);
    printf("%s  input `%sT %s%d_in", sep, vltype(t), cname(t), (int)i + 1);
    sep = ",\n";
  }

  COUNTUP(i, out_n) {
    printf("%s  output `%sT %s%d_out", sep, vltype(rtypes[i]), cname(rtypes[i]), (int)i);
  }

  printf("\n);\n");
}

static
void gen_value_rhs(const cell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("`read(`%sN, %d);\n", vltype(t), (int)c->value.integer);
    break;
  case T_STRING: {
    seg_t str = value_seg(c);
    printf("\"");
    print_escaped_string(str);
    printf("\";\n");
    break;
  }
  default:
    assert_error(false); // TODO add more types
  }
}

static
void gen_decls(cell_t *e) {
  if(e->entry.rec) {
    printf("  reg active = 1'b0;\n");
    const cell_t *p = e + 1;
    printf("  wire read = ");
    const char *sep = "";
    COUNTDOWN(i, e->entry.in) {
      const cell_t *a = &p[i];
      type_t t = trace_type(a);
      printf("%s%s%d_in`%sR", sep, cname(t), (int)i + 1, vltype(t));
      sep = "\n    & ";
    }
    printf(";\n\n");
  }
  FOR_TRACE_CONST(c, e) {
    int i = c - e;
    type_t t = trace_type(c);
    if(ONEOF(t, T_BOTTOM, T_RETURN)) continue;
    if(is_var(c)) {
      if(e->entry.rec) {
        printf("  reg `%sT %s%d;\n", vltype(t), cname(t), (int)i);
      } else {
        printf("  wire `%sT %s%d = %s%d_in;\n", vltype(t), cname(t), (int)i, cname(t), (int)i);
      }
    } else {
      if(c->op == OP_value) {
        printf("  localparam `%sT %s%d = ", vltype(t), cname(t), (int)i);
        gen_value_rhs(c);
      } else {
        printf("  wire `%sT %s%d;\n", vltype(t), cname(t), i);
      }
    }
  }
}

static
void gen_instance(const cell_t *e, const cell_t *c) {
  int inst = c - e;
  char *sep = "";
  const char *module_name, *word_name;
  cell_t *entry = get_entry(c);
  bool sync = entry && FLAG(*entry, entry, SYNC);
  csize_t
    in = closure_in(c),
    n = closure_args(c),
    start_out = n - closure_out(c);

  trace_get_name(c, &module_name, &word_name);
  type_t t = trace_type(c);
  printf("  %s_%s", module_name, word_name);
  if(c->op != OP_external) print_type_suffix(e, c);
  printf(" inst%d(", inst);

  if(sync) printf("clk, ");

  COUNTUP(i, in) {
    int a = cgen_index(e, c->expr.arg[i]);
    printf("%s%s%d", sep, cname(trace_type(&e[a])), a);
    sep = ", ";
  };
  printf("%s%s%d", sep, cname(t), inst);
  RANGEUP(i, start_out, n) {
    int a = cgen_index(e, c->expr.arg[i]);
    if(a <= 0) {
      printf(", /*nc*/");
    } else {
      printf(", %s%d", cname(trace_type(&e[a])), a);
    }
  }

  printf(");\n");
}

static
void gen_body(cell_t *e) {
  int block_start = 1;
  FOR_TRACE_CONST(c, e) {
    if(!is_value(c)) {
      if(!ONEOF(c->op, OP_assert, OP_dep, OP_seq, OP_unless)) {
        if(get_entry(c) == e) {
          assert_error(last_call(e, c));
          continue;
        }
        if(block_start > 0) {
          printf("\n  // block%d:\n", block_start);
          block_start = 0;
        }
        gen_instance(e, c);
      }
    } else if(is_return(c)) {
      block_start = (c - e) + calculate_cells(c->size);
    }
  }
}

static
void gen_conditions(const cell_t *e) {
  const char *sep = "";
  bool start = true;
  int prev_block = 0;
  FOR_TRACE_CONST(c, e) {
    if(start) {
      printf("  wire block%d_cond = ", (int)(c - e));
        if(prev_block) {
          printf("!block%d_cond", prev_block);
          sep = " && ";
        } else {
          sep = "";
        }
        start = false;
        prev_block = c - e;
    }
    if(is_return(c)) {
      if(!*sep) printf("1'b1");
      printf(";\n");
      start = true;
    } else if(c->op == OP_assert) {
      int ai = cgen_index(e, c->expr.arg[1]);
      printf("%s%s%d", sep, cname(trace_type(&e[ai])), ai);
      sep = " && ";
    }
  }
}

static
void gen_outputs(const cell_t *e, const cell_t *r0, const type_t *rtypes) {
  const cell_t *r = r0;
  csize_t out = e->entry.out;
  COUNTUP(i, out) {
    type_t t = rtypes[i];
    printf("  assign %s%d_out = ", cname(t), (int)i);
    if(e->entry.rec) {
      printf("`on_write(`%sN, ", vltype(t));
    } else {
      printf("(");
    }
    const cell_t *r_prev = NULL;
    int block = 1;
    do {
      if(NOT_FLAG(*r, trace, TAIL_CALL)) {
        if(r_prev) {
          printf("\n      block%d_cond ? %s%d : ",
                 block, cname(t), cgen_index(e, r_prev->value.ptr[i]));
          block = (r - e) + calculate_cells(r->size);
        }
        r_prev = r;
      }
      r = &e[tr_index(r->alt)];
    } while(r > e);
    assert_error(r_prev);
    printf("%s%d);\n", cname(t), cgen_index(e, r_prev->value.ptr[i]));
  }
}

static
void gen_ready(const cell_t *e, const cell_t *r0) {
  const char *sep = "";
  printf("  wire ready = ");
  const cell_t *r = r0;
  int block = 1;
  while(NOT_FLAG(*r, trace, TAIL_CALL)) {
    printf("%sblock%d_cond", sep, block);
    block = (r - e) + calculate_cells(r->size);
    sep = " || ";
    if(!r->alt) break;
    r = &e[tr_index(r->alt)];
  }
  printf(";\n"
         "  wire write = active && ready;\n");
}

static
void gen_loop(const cell_t *e, const cell_t *l, int start, const cell_t *tail_call) {
  csize_t in = e->entry.in;
  printf("      if(");
  const char *sep = "";
  FOR_TRACE_CONST(c, e, start) {
    if(c->op == OP_assert) {
      int ai = cgen_index(e, c->expr.arg[1]);
      printf("%s%s%d", sep, cname(trace_type(&e[ai])), ai);
      sep = " && ";
    }
    if(c >= l) break;
  }
  printf(") begin\n");
  COUNTUP(i, in) {
    int a = cgen_index(e, tail_call->expr.arg[i]);
    printf("        %s%d <= %s%d;\n",
           cname(trace_type(&e[in - i])), (int)(in - i),
           cname(trace_type(&e[a])), a);
  }
  printf("      end\n");
}

static
void gen_loops(cell_t *e) {
  printf("  always @(posedge clk) begin\n"
         "    if(read) begin\n");
  FOR_TRACE_CONST(c, e) {
    if(!is_var(c)) break;
    int i = c - e;
    type_t t = trace_type(c);
    printf("      %s%d <= %s%d_in;\n", cname(t), i, cname(t), i);
  }
  printf("      `set(active);\n"
         "    end\n"
         "    else if(ready) begin\n"
         "       `reset(active);\n"
         "    end\n"
         "    else begin\n");
  int start = 1;
  const cell_t *tail_call = NULL;
  FOR_TRACE_CONST(c, e) {
    if(is_return(c)) {
      if(tail_call) gen_loop(e, c, start, tail_call);
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
  gen_conditions(e);
  printf("\n");
  if(e->entry.rec) {
    gen_ready(e, r0);
    printf("\n");
    gen_loops(e);
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
