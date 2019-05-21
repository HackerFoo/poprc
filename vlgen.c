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
    [T_ANY]      = "`anyT ",
    [T_INT]      = "`intT ",
    [T_LIST]     = "`arrayT ",
    [T_SYMBOL]   = "`symT ",
    [T_MAP]      = "`mapT ",
    [T_STRING]   = "`stringT ",
    [T_FLOAT]    = "`realT ",
    [T_OPAQUE]   = "`opaqueT ",
    [T_BOTTOM]   = "`voidT ",
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

static
void gen_module_interface(const cell_t *e) {
  const cell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  type_t rtypes[out_n];
  bool sync = FLAG(*e, entry, SYNC);
  resolve_types(e, rtypes);

  printf("module %s_%s (\n", e->module_name, e->word_name);
  char *sep = "";
  if(sync) {
    printf("  input clk,\n  input reset");
    sep = ",\n";
  }
  COUNTDOWN(i, e->entry.in) {
    const cell_t *a = &p[i];
    type_t t = trace_type(a);
    printf("%s  input %s%s%d_in", sep, vltype(t), cname(t), (int)i + 1);
    sep = ",\n";
  }

  COUNTUP(i, out_n) {
    printf("%s  output reg %s%s%d_out", sep, vltype(rtypes[i]), cname(rtypes[i]), (int)i);
  }
  if(sync) {
    printf(",\n  output%s busy", e->entry.rec ? " reg" : "");
  }

  printf("\n);\n");
}

static
void gen_value_rhs(const cell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("%d;\n", (int)c->value.integer);
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
    printf("  reg start;\n\n");
  }
  FOR_TRACE_CONST(c, e) {
    int i = c - e;
    type_t t = trace_type(c);
    if(ONEOF(t, T_BOTTOM, T_RETURN)) continue;
    if(is_var(c)) {
      if(e->entry.rec) {
        printf("  reg %s%s%d;\n", vltype(t), cname(t), (int)i);
      } else {
        printf("  wire %s%s%d = %s%d_in;\n", vltype(t), cname(t), (int)i, cname(t), (int)i);
      }
    } else {
      if(c->op == OP_value) {
        printf("  localparam %s%s%d = ", vltype(t), cname(t), (int)i);
        gen_value_rhs(c);
      } else {
        printf("  wire %s%s%d;\n", vltype(t), cname(t), i);
      }
    }
  }
}

static
void gen_initial(cell_t *e) {
  if(e->entry.rec) {
    printf("\n"
           "  initial begin\n"
           "    `reset(start);\n"
           "    `reset(busy);\n"
           "  end\n");
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

  if(sync) printf("clk, reset, "); // TODO, connect reset and busy

  COUNTUP(i, in) {
    int a = cgen_index(e, c->expr.arg[i]);
    printf("%s%s%d", sep, cname(trace_type(&e[a])), a);
    sep = ", ";
  };
  printf("%s%s%d", sep, cname(t), inst);
  if(sync) printf(", busy");
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
  int last_return = 0;
  FOR_TRACE_CONST(c, e) {
    if(!is_value(c)) {
      if(!ONEOF(c->op, OP_assert, OP_dep, OP_seq, OP_unless)) {
        if(get_entry(c) == e) {
          assert_error(last_call(e, c));
          continue;
        }
        if(last_return >= 0) {
          printf("\n  // block%d:\n", last_return);
          last_return = -1;
        }
        gen_instance(e, c);
      }
    } else if(is_return(c)) {
      last_return = (c - e) + calculate_cells(c->size);
    }
  }
}

static
void gen_return(const cell_t *e, const cell_t *l, int start, const cell_t *tail_call, int indent) {
  const char *assign = e->entry.rec ? "<=" : "=";
  gen_indent(indent); printf("// %s %d\n", tail_call ? "loop" : "output", (int)(l - e));
  if(e->entry.alts > 1) {
    gen_indent(indent); printf("if(");
    const char *sep = "";
    FOR_TRACE_CONST(c, e, start) {
      type_t t = trace_type(c);
      if(c->op == OP_assert) {
        int ai = cgen_index(e, c->expr.arg[1]);
        printf("%s%s%d", sep, cname(trace_type(&e[ai])), ai);
        sep = " && ";
      }
      if(c >= l) break;
    }
    printf(") begin\n");
    indent++;
  }
  if(tail_call) {
    csize_t in = closure_in(tail_call);
    COUNTUP(i, in) {
      int a = cgen_index(e, tail_call->expr.arg[i]);
      gen_indent(indent ); printf("%s%d %s %s%d;\n",
                                  cname(trace_type(&e[in - i])), (int)(in - i),
                                  assign,
                                  cname(trace_type(&e[a])), a);
    }
  } else {
    csize_t out_n = list_size(l);
    COUNTDOWN(i, out_n) {
      int ai = cgen_index(e, l->value.ptr[i]);
      const cell_t *a = &e[ai];
      type_t t = trace_type(a);
      const char *n = cname(t);
      int output = REVI(i);
      gen_indent(indent); printf("%s%d_out %s %s%d;\n",
                                 n, output,
                                 assign,
                                 n, ai);
    }
    if(e->entry.rec) {
      gen_indent(indent); printf("`reset(busy);\n");
    }
  }
  if(e->entry.alts > 1) {
    indent--;
    gen_indent(indent); printf("end\n");
  }
}

static
void gen_join(cell_t *e) {
  if(e->entry.rec) {
    printf("  always @(posedge clk) begin\n"
           "    if(reset) begin\n"
           "        `set(start);\n"
           "        `set(busy);\n"
           "    end\n"
           "    else if(start) begin\n");
    FOR_TRACE_CONST(c, e) {
      if(!is_var(c)) break;
      int i = c - e;
      type_t t = trace_type(c);
      printf("      %s%d <= %s%d_in;\n", cname(t), i, cname(t), i);
    }
    printf("      `reset(start);\n"
           "    end\n"
           "    else if(busy) begin\n");
    int start = 1;
    const cell_t *tail_call = NULL;
    FOR_TRACE_CONST(c, e) {
      if(is_return(c)) {
        if(start > 1) printf("\n");
        gen_return(e, c, start, tail_call, 3);
        start = (c - e) + calculate_cells(c->size);
        tail_call = NULL;
      } else if(get_entry(c) == e) {
        tail_call = c;
      }
    }
    printf("    end\n"
           "  end\n");
  } else {
    printf("  always @(*) begin\n");
    int start = 1;
    FOR_TRACE_CONST(c, e) {
      if(is_return(c)) {
        if(start > 1) printf("\n");
        gen_return(e, c, start, NULL, 2);
        start = (c - e) + calculate_cells(c->size);
      }
    }
    printf("  end\n");
  }
}

static
void gen_module(cell_t *e) {
  e->op = OP_value;
  gen_module_interface(e);
  printf("\n");
  gen_decls(e);
  gen_initial(e);
  gen_body(e);
  printf("\n");
  gen_join(e);
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
