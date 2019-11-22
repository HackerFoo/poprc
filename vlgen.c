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
#include "list.h"

#define MAX_DEGREE 31

// table of corresponding Verilog types
static
const char *vltype(type_t t) {
  return t == T_LIST ? "stream" : "simple";
}

static
const char *vltype_full(const tcell_t *e, const tcell_t *c) {
  type_t t = trace_type(c);
  if(t == T_OPAQUE) {
    val_t sym = trace_get_opaque_symbol(e, c);
    return symbol_string(sym);
  } else {
    return vltype(t);
  }
}

#define SEP(str) \
  const char *sep = ""; \
  const char *const sep_next = str

#define printf_sep(fmt, ...)                    \
  do {                                          \
    printf("%s" fmt, sep, ##__VA_ARGS__);       \
    sep = sep_next;                             \
  } while(0)

static
void gen_module_interface(const tcell_t *e) {
  const tcell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  trace_t tr;

  printf("module %s_%s (\n", e->module_name, e->word_name);
  SEP(",\n");
  if(FLAG(*e, entry, SYNC)) {
    printf_sep("  `sync_ports");
  }
  COUNTDOWN(i, e->entry.in) {
    const tcell_t *a = &p[i];
    type_t t = trace_type(a);
    if(t != T_OPAQUE) {
      printf_sep("  `input(%s, %d, %d)",
                 vltype_full(e, a),
                 a->trace.bit_width,
                 (int)(e->entry.in - 1 - i));
    } else {
      printf_sep("  `interface(%s, `addrN, `intN, %d)", // *** addr/data width isn't in trace yet
                 vltype_full(e, a),
                 (int)(e->entry.in - 1 - i));
    }
  }

  COUNTUP(i, out_n) {
    get_trace_info_for_output(&tr, e, i);
    if(tr.type != T_OPAQUE) {
      printf_sep("  `output(%s, %d, %d)", vltype(tr.type), tr.bit_width, (int)i);
    }
  }

  printf("\n);\n");
}

static
void gen_value_rhs(const tcell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
    printf("%d", (int)c->value.integer);
    break;
  case T_SYMBOL:
    printf("%d", (int)c->value.symbol);
    break;
  case T_STRING: {
    seg_t str = value_seg(&c->c);
    printf("\"");
    print_escaped_string(str);
    printf("\"");
    break;
  }
  case T_LIST: {
    assert_eq(list_size(c), 0);
    printf("`nil");
    break;
  }
  default:
    assert_error(false); // TODO add more types
  }
}

static bool is_sync(const tcell_t *c) {
  tcell_t *entry;
  return
    !is_value(c) &&
    (ONEOF(trace_type(c), T_LIST, T_OPAQUE) ||
     ((entry = get_entry(c)) &&
      FLAG(*entry, entry, SYNC)));
}

static
bool update_block(const tcell_t *e, const tcell_t *c, int *block) {
  if(is_return(c) ||
     (is_self_call(e, c) && NOT_FLAG(*c, trace, JUMP))) {
    *block = (c - e) + calculate_cells(c->size);
    return true;
  } else {
    return false;
  }
}

static
void gen_stack(const tcell_t *e) {
  int ra_bits = 0;
  int nt_self_calls = 1;
  trace_t tr;
  if(FLAG(*e, entry, RETURN_ADDR)) {
    nt_self_calls = 0;
    FOR_TRACE_CONST(c, e) {
      if(is_self_call(e, c) &&
         NOT_FLAG(*c, trace, JUMP)) {
        nt_self_calls++;
      }
    }
    ra_bits = int_log2(nt_self_calls);
  }
  printf("\n"
         "  // stack\n"
         "  `define RB %d\n", ra_bits);
  if(ra_bits) {
    int i = 0;
    int block = 1;
    FOR_TRACE_CONST(c, e) {
      if(update_block(e, c, &block) &&
         is_self_call(e, c) &&
         NOT_FLAG(*c, trace, JUMP)) {
        printf("  localparam label_block%d = `RB'd%d;\n", block, i++);
      }
    }
  }
  printf("  localparam STACK_WIDTH = `RB + %d * `intN;\n" // ***
         "  reg [STACK_WIDTH-1:0] stack[0:255];\n"
         "  reg [7:0] sp = 0;\n"
         "  reg returned = `false;\n", e->entry.in + nt_self_calls - 1);
  if(ra_bits) {
    printf("  reg [`RB-1:0] return_addr = `RB'd0;\n");
  }
  COUNTUP(i, e->entry.out) {
    get_trace_info_for_output(&tr, e, i);
    printf("  reg [%d-1:0] return%d = 0;\n", tr.bit_width, (int)i);
  }
}

static
void gen_decls(tcell_t *e) {
  FOR_TRACE_CONST(tc, e) {
    int i = tc - e;
    const cell_t *c = &tc->c;
    type_t t = trace_type(tc);
    if(ONEOF(t, T_BOTTOM, T_RETURN)) continue;
    if(is_var(c)) {
      if(t != T_OPAQUE) {
        const char *decl = FLAG(*e, entry, RECURSIVE) ? "variable" : "alias";
        printf("  `%s(%s, %d, %s%d, in%d);\n", decl, vltype(t), tc->trace.bit_width, cname(t), i, e->entry.in - i);
      }
    } else {
      if(c->op == OP_value) {
        if(t == T_LIST) {
          printf("  `const_nil(%d, %s%d);\n", tc->trace.bit_width, cname(t), i);
        } else {
          printf("  `const(%s, %d, %s%d, ", vltype(t), tc->trace.bit_width, cname(t), i);
          gen_value_rhs(tc);
          printf(");\n");
        }
      } else if(!is_tail_call(e, tc)) {
        if(is_sync(tc)) {
          printf("  wire inst%d_in_ready;\n", i);
        }
        if(t == T_OPAQUE) {
          // handled below
        } else if(is_self_call(e, tc)) {
          printf("  `reg(%s, %d, %s%d) = 0;\n", vltype(t), tc->trace.bit_width, cname(t), i); // ***
        } else {
          printf("  `wire(%s, %d, %s%d);\n", vltype(t), tc->trace.bit_width, cname(t), i);
        }
        if(!is_dep(c)) {
          TRAVERSE(c, const, in) {
            if(!*p) continue;
            tcell_t *a = &e[tr_index(*p)];
            if(trace_type(a) == T_OPAQUE) {
              const tcell_t *v = trace_get_linear_var(e, a);
              printf("  `bus(%s, %d, inst%d);\n", vltype_full(e, a), e->entry.in - (int)(v-e), i);
            }
          }
        }
      } else if(t == T_LIST) {
        printf("  `const_nil(%d, %s%d);\n", tc->trace.bit_width, cname(t), i);
      }
    }
  }
}

static
void find_sync_inputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, int depth) {
  while(ONEOF(c->op, OP_assert, OP_seq, OP_unless)) {
    if(c->op == OP_assert && !depth) set_insert(c-e, set, size);
    c = &e[tr_index(c->expr.arg[0])];
  }
  if(!depth && is_sync(c) && !is_tail_call(e, c)) {
    set_insert(c-e, set, size);
  } else if(is_var(c)) {
    if(!depth) set_insert(e->entry.len+1, set, size);
  } else {
    TRAVERSE(c, const, in, ptrs) {
      find_sync_inputs(e, &e[tr_index(*p)], set, size, max(0, depth-1));
    }
  }
}

static
void gen_sync_disjoint_inputs(const tcell_t *e, const tcell_t *c) {
  uintptr_t set[MAX_DEGREE] = {0};
  uintptr_t input_set[MAX_DEGREE] = {0};
  LOG("gen_sync_disjoint_inputs %E %d", e, c-e);
  find_sync_inputs(e, c, set, LENGTH(set), 1);
  FOREACH(i, set) {
    if(INRANGE(set[i], 1, e->entry.len)) {
      find_sync_inputs(e, &e[set[i]], input_set, LENGTH(input_set), 1);
    }
  }
  FOREACH(i, input_set) {
    if(input_set[i]) {
      set_remove(input_set[i], set, LENGTH(set));
    }
  }
  size_t inputs = squeeze(set, LENGTH(set));
  if(inputs) {
    COUNTUP(i, inputs) {
      if(i) printf(" & ");
      if(set[i] <= e->entry.len) {
        printf("inst%d_out_valid", (int)set[i]);
      } else {
        if(FLAG(*e, entry, RECURSIVE)) {
          printf("active");
        } else {
          printf("in_valid");
        }
      }
    }
  } else {
    printf("`true");
  }
}

static
void gen_sync_block(const tcell_t *e, const tcell_t *c, int block, bool ret) {
  SEP(" & ");
  if(ret) {
    if(FLAG(*e, entry, RETURN_ADDR)) {
      printf_sep("`returned_to(block%d)", block);
    } else {
      printf_sep("returned");
    }
  }
  FOR_TRACE_CONST(c, e, block) {
    if(is_return(c)) break;
    if(is_user_func(c)) {
      tcell_t *ce = get_entry(c);
      if(ce == e) break;
    }
    if(c->op == OP_unless) {
      int b = 1;
      FOR_TRACE_CONST(r, e) {
        if(is_return(r) &&
           b != block) {
          printf_sep("!block%d_valid", b);
        }
        update_block(e, r, &b);
      }
    }
  }
  printf_sep("");
  gen_sync_disjoint_inputs(e, c);
}

static
void find_sync_outputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, uintptr_t const *const *backrefs) {
  if(is_return(c)) return;
  const uintptr_t *outs;
  size_t n = get_outputs(e, c, backrefs, &outs);
  COUNTUP(i, n) {
    int oi = outs[i];
    const tcell_t *out = &e[oi];
    if(is_tail_call(e, out)) {
      // stop
    } else if(is_return(out) || (is_sync(out))) {
      set_insert(oi, set, size);
    } else if(ONEOF(out->op, OP_unless, OP_assert, OP_seq) && cgen_lookup(e, out) != c - e) {
      // stop
    } else {
      find_sync_outputs(e, out, set, size, backrefs);
    }
  }
}

static
void gen_sync_disjoint_outputs(const tcell_t *e, const tcell_t *c, uintptr_t const *const *backrefs) {
  uintptr_t set[MAX_DEGREE] = {0};
  uintptr_t output_set[MAX_DEGREE] = {0};
  LOG("gen_sync_disjoint_outputs %E %d", e, c-e);
  if(c > e) {
    find_sync_outputs(e, c, set, LENGTH(set), backrefs);
  } else {
    COUNTUP(i, e->entry.in) {
      find_sync_outputs(e, &e[i+1], set, LENGTH(set), backrefs);
    }
  }
  FOREACH(i, set) {
    if(INRANGE(set[i], 1, e->entry.len)) {
      find_sync_outputs(e, &e[set[i]], output_set, LENGTH(output_set), backrefs);
    }
  }
  FOREACH(i, output_set) {
    if(output_set[i]) {
      set_remove(output_set[i], set, LENGTH(set));
    }
  }
  size_t outputs = squeeze(set, LENGTH(set));
  if(outputs) {
    bool top = false;
    COUNTUP(i, outputs) {
      if(i) printf(" & ");
      if(!is_return(&e[set[i]])) {
        printf("inst%d_in_ready", (int)set[i]);
      } else if(!top) {
        printf("out_ready");
        top = true;
      }
    }
  } else {
    printf("`true");
  }
}

static
void print_var(const tcell_t *e, const tcell_t *c, int block) {
  int next = (c - e) + calculate_cells(c->size);
  type_t t = trace_type(c);
  if(t != T_LIST && is_self_call(e, c) && next >= block) {
    assert_eq(e->entry.out, 1); // TODO
    printf("return0");
  } else {
    if(t != T_OPAQUE) {
      printf("%s%d", cname(t), (int)(c - e));
    } else {
      printf("intf%d", (int)(e->entry.in - (c - e)));
    }
  }
}

static
void gen_width_params(const tcell_t *e, const tcell_t *c) {
  printf("#(");
  if(!is_user_func(c)) {
    csize_t
      in = closure_in(c),
      n = closure_args(c),
      start_out = n - closure_out(c);
    type_t t = trace_type(c);
    SEP(", ");
    COUNTUP(i, in) {
      const tcell_t *a = &e[cgen_index(e, c->expr.arg[i])];
      if(trace_type(a) == T_OPAQUE) continue;
      if(range_bounded(a->trace.range) && !range_empty(a->trace.range)) {
        assert_gt(bits_needed(a->trace.range), 0);
        printf_sep(".in%dN(%d)", (int)i, bits_needed(a->trace.range));
      }
    }
    if(t != T_OPAQUE) {
      if(range_bounded(c->trace.range) && !range_empty(c->trace.range)) {
        printf_sep(".out0N(%d)", bits_needed(c->trace.range));
      }
    }
    RANGEUP(i, start_out, n) {
      int a = cgen_index(e, c->expr.arg[i]);
      if(a > 0) {
        type_t at = trace_type(&e[a]);
        if(at == T_OPAQUE) continue;
        if(range_bounded(e[a].trace.range) && !range_empty(e[a].trace.range)) {
          printf_sep(".out%dN(%d)", (int)(i - start_out + 1), bits_needed(e[a].trace.range));
        }
      }
    }
  }
  printf(")");
}

static
void gen_instance(const tcell_t *e, const tcell_t *c, int *sync_chain, uintptr_t const *const *backrefs, int block) {
  int inst = c - e;
  const char *module_name, *word_name;
  bool sync = is_sync(c);
  csize_t
    in = closure_in(c),
    n = closure_args(c),
    start_out = n - closure_out(c);

  trace_get_name(c, &module_name, &word_name);
  type_t t = trace_type(c);

  if(sync) {
    printf("    `inst_sync(");
    print_function_name(e, c);
    printf(", inst%d, ", inst);
    gen_width_params(e, c);
    printf(")(\n      `sync(");
    gen_sync_disjoint_inputs(e, c);
    printf(", ");
    gen_sync_disjoint_outputs(e, c, backrefs);
    printf("),");
  } else {
    printf("    `inst(");
    print_function_name(e, c);
    printf(", inst%d, ", inst);
    gen_width_params(e, c);
    printf(")(");
  }

  SEP(",");
  COUNTUP(i, in) {
    const tcell_t *a = &e[cgen_index(e, c->expr.arg[i])];
    type_t t = trace_type(a);
    if(t == T_OPAQUE) {
      a = trace_get_linear_var(e, a);
    }
    printf_sep("\n      `%s(%s, %d, ",
           trace_type(a) == T_OPAQUE ? "intf" : "in",
           vltype_full(e, a), (int)i); // ***
    print_var(e, a, block);
    printf(")");
  }
  if(t != T_OPAQUE) {
    printf_sep("\n      `out(%s, 0, %s%d)",
               vltype(t), cname(t), inst);
  }
  RANGEUP(i, start_out, n) {
    int a = cgen_index(e, c->expr.arg[i]);
    if(a > 0) {
      type_t at = trace_type(&e[a]);
      if(at == T_OPAQUE) continue;
      printf_sep("\n      `out(%s, %d, %s%d)",
                 vltype(at), (int)(i - start_out + 1), cname(at), a);
    }
  }

  printf("\n    );\n");
  if(sync) *sync_chain = inst;
}

static
void next_block(const tcell_t *e, const tcell_t *r, int block,
                bool block_start, int return_block) {
  if(block_start) {
    printf("\n  `start_block(block%d)\n", block);
  }
  printf("    wire block%d_valid = ", block);
  gen_sync_block(e, r, block, block == return_block);
  printf(";\n");
  printf("  `end_block(block%d)\n", block);
}

static
void gen_body(tcell_t *e) {
  size_t backrefs_n = backrefs_size(e);
  assert_le(backrefs_n, 1024);
  uintptr_t const *backrefs[backrefs_n];
  build_backrefs(e, (uintptr_t **)backrefs, backrefs_n);
  bool block_start = true;
  int block = 1;
  int return_block = 0;
  int block_sync_chain = 0;
  if(FLAG(*e, entry, RECURSIVE)) {
    printf("\n  `loop_sync(");
    gen_sync_disjoint_outputs(e, e, backrefs);
    printf(");\n");
  } else if(FLAG(*e, entry, SYNC)) {
    printf("\n  `top_sync(");
    gen_sync_disjoint_outputs(e, e, backrefs);
    printf(");\n");
  }
  FOR_TRACE_CONST(c, e) {
    if(!is_value(c)) {
      if(!ONEOF(c->op, OP_dep, OP_seq, OP_unless)) {
        if(get_entry(c) == e) {
          if(FLAG(*c, trace, JUMP)) {
            TRAVERSE(c, const, in) {
              int i = tr_index(*p);
              const tcell_t *a = &e[i];
              if(trace_type(a) == T_LIST && direct_refs(&a->c) <= 1) { // *** HACK
                printf("    assign %s%d_ready = `true;\n", cname(T_LIST), i);
              }
            }
          } else {
            next_block(e, c, block, block_start, return_block);
            update_block(e, c, &block);
            return_block = block;
          }
          continue;
        }
        if(block_start) {
          printf("\n  `start_block(block%d)\n", block);
          block_start = false;
          block_sync_chain = 0;
        }
        if(c->op == OP_assert) {
          int ai = cgen_index(e, c->expr.arg[1]);
          printf("    `assert(inst%d, sym%d);\n", (int)(c - e), ai);
        } else {
          gen_instance(e, c, &block_sync_chain, backrefs, block);
        }
      }
    } else if(is_return(c)) {
      next_block(e, c, block, block_start, return_block);
    }
    block_start = update_block(e, c, &block);
  }
}

// TODO clean up
static
bool gen_outputs(const tcell_t *e) {
  bool ret = false;
  csize_t out = e->entry.out;
  trace_t tr;
  COUNTUP(i, out) {
    get_trace_info_for_output(&tr, e, i);
    type_t t = tr.type;
    if(t == T_OPAQUE) continue;
    printf("  assign out%d =", (int)i);
    const tcell_t *r = &e[e->trace.first_return];
    const tcell_t *r_prev = NULL;
    int block = 1;
    do {
      if(t == T_LIST || !is_tail_call(e, r)) {
        if(r_prev) {
          int ri = cgen_index(e, r_prev->value.ptr[REVI(i)]);
          printf("\n      block%d_valid ? ", block);
          print_var(e, &e[ri], block);
          printf(" :");
          block = (r - e) + calculate_cells(r->size);
        }
        r_prev = r;
      }
      r = &e[tr_index(r->alt)];
    } while(r > e);
    assert_error(r_prev);

    printf(" ");
    print_var(e, &e[cgen_index(e, r_prev->value.ptr[REVI(i)])], block);
    printf(";\n");

    if(t == T_LIST) {
      printf("  assign out%d_valid = ", (int)i);
      r = &e[e->trace.first_return];
      SEP(" | ");
      while(r > e) {
        printf_sep("%s%d_valid", cname(t), cgen_index(e, r_prev->value.ptr[REVI(i)]));
        r = &e[tr_index(r->alt)];
      }
      printf(";\n");
      r = &e[e->trace.first_return];
      while(r > e) {
        printf("  assign %s%d_ready = out%d_ready;\n", cname(t), cgen_index(e, r_prev->value.ptr[REVI(i)]), (int)i);
        r = &e[tr_index(r->alt)];
      }
    }
    ret = true;
  }
  return ret;
}

bool is_self_call(const tcell_t *e, const tcell_t *c) {
  return is_user_func(c) && get_entry(c) == e;
}

static
void gen_valid_ready(const tcell_t *e) {
  int block;

  // valid
  printf("  wire %s = ",
         FLAG(*e, entry, STACK) ? "returning" : "valid");
  SEP(" | ");
  block = 1;
  FOR_TRACE_CONST(c, e) {
    if(is_return(c) &&
       !is_tail_call(e, c)) {
      printf_sep("block%d_valid", block);
    }
    update_block(e, c, &block);
  }
  if(!*sep) printf("`false");
  printf(";\n");
  if(FLAG(*e, entry, STACK)) {
    printf("  wire valid = returning & ~|sp;\n");
  }
  printf("  assign out_valid = %svalid;\n", FLAG(*e, entry, RECURSIVE) ? "active & " : "");
}

static
void gen_loop(const tcell_t *e, const tcell_t *self_call, int block) {
  csize_t in = e->entry.in;
  printf("      if(block%d_valid) begin\n", block);
  if(NOT_FLAG(*self_call, trace, JUMP)) {
    printf("        stack[sp] <= {");
    if(FLAG(*e, entry, RETURN_ADDR)) {
      int next_block = (self_call - e) + calculate_cells(self_call->size);
      printf("label_block%d, ", next_block);
    }
    SEP(", ");
    RANGEUP(i, 1, e->entry.in + 1) {
      const tcell_t *v = &e[i];
      printf_sep("%s%d", cname(trace_type(v)), (int)i);
    }
    const tcell_t *prev = NULL;
    FOR_TRACE_CONST(c, e) {
      if(is_self_call(e, c) && NOT_FLAG(*c, trace, JUMP)) {
        if(prev) {
          printf_sep("");
          print_var(e, prev, block);
        }
        prev = c;
      }
    }
    printf("};\n");
    printf("        sp <= sp + 1;\n");
  }
  COUNTUP(i, in) {
    int a = cgen_index(e, self_call->expr.arg[i]);
    type_t t = trace_type(&e[a]);
    assert_error(trace_type(&e[in - i]) == t);
    if(!ONEOF(t, T_LIST, T_OPAQUE)) {
      printf("        %s%d <= %s%d;\n",
             cname(t), (int)(in - i),
             cname(t), a);
    }
  }
  printf("      end\n");
}

static
void gen_loops(tcell_t *e) {
  printf("  always @(posedge clk) begin\n");
  if(FLAG(*e, entry, STACK))
     printf("    returned <= returning;\n");
  printf("    if(in_valid & ~active) begin\n");
  FOR_TRACE_CONST(c, e) {
    if(!is_var(c)) break;
    int i = c - e;
    type_t t = trace_type(c);
    if(!ONEOF(t, T_LIST, T_OPAQUE)) {
      printf("      %s%d <= in%d;\n", cname(t), i, e->entry.in - i);
    }
  }
  printf("      `set(active);\n");
  if(FLAG(*e, entry, STACK))
    printf("      `reset(returned);\n");
  printf("    end\n"
         "    else if(valid) begin\n"
         "       if(out_ready) `reset(active);\n"
         "    end\n");
  if(FLAG(*e, entry, STACK)) {
    printf("    else if(returning) begin\n");
    COUNTUP(i, e->entry.out) {
      printf("      return%d <= out%d;\n", (int)i, (int)i);
    }
    if(FLAG(*e, entry, RETURN_ADDR)) {
      printf("      {return_addr, ");
    } else {
      printf("      {");
    }
    SEP(", ");
    RANGEUP(i, 1, e->entry.in + 1) {
      tcell_t *v = &e[i];
      printf_sep("%s%d", cname(trace_type(v)), (int)i);
    }
    const tcell_t *prev = NULL;
    FOR_TRACE_CONST(c, e) {
      if(is_self_call(e, c) && NOT_FLAG(*c, trace, JUMP)) {
        if(prev) {
          printf_sep("%s%d", cname(trace_type(prev)), (int)(prev-e));
        }
        prev = c;
      }
    }
    printf("} <= stack[sp - 1];\n");
    printf("      sp <= sp - 1;\n"
           "    end\n");
  }
  printf("    else begin\n");
  int block = 1;
  FOR_TRACE_CONST(c, e) {
    if(is_self_call(e, c)) {
      gen_loop(e, c, block);
    }
    update_block(e, c, &block);
  }
  printf("    end\n"
         "  end\n");
}

static
bool gen_buses(tcell_t *e) {
  bool output = false;
  COUNTUP(i, e->entry.in) {
    int ix = REVI(i) + 1;
    const tcell_t *v = &e[ix];
    if(v->value.type != T_OPAQUE ||
       trace_get_opaque_symbol(e, v) != SYM_Array) continue;
    int a = ix;
    SEP(" |\n");
    printf("  assign `to_bus(intf%d) =\n", (int)i);
    FOR_TRACE_CONST(c, e, ix) {
      if(is_return(c)) {
        a = ix;
      } else if(!is_dep(c) &&
                tr_index(c->expr.arg[0]) == a) {
        a = c - e;
        if(FLAG(*c, trace, JUMP) && is_self_call(e, c)) break;
        printf_sep("    `to_bus(inst%d_intf%d)", a, (int)i);
        if(trace_type(c) != T_OPAQUE) break;
      }
    }
    printf(";\n");
    output = true;
  }
  return output;
}

static
void gen_module(tcell_t *e) {
  e->op = OP_value;
  gen_module_interface(e);
  printf("\n");
  gen_decls(e);
  if(FLAG(*e, entry, STACK)) {
    gen_stack(e);
  }
  gen_body(e);
  printf("\n");
  if(FLAG(*e, entry, SYNC)) {
    gen_valid_ready(e);
    printf("\n");
  }
  if(gen_buses(e)) printf("\n");
  if(FLAG(*e, entry, RECURSIVE)) {
    gen_loops(e);
    printf("\n");
  }
  if(gen_outputs(e)) printf("\n");
  printf("endmodule\n");

  FOR_TRACE(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      tcell_t *x = get_entry(c);
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
    cell_t *m = eval_module();
    tcell_t *e = tcell_entry(module_lookup_compiled(tok_seg(rest), &m));

    if(e) {
      gen_module(e);
      clear_ops(e);
    }
  }
  if(command_line) quit = true;
}
