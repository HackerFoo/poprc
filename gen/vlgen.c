/* Copyright 2012-2020 Dustin DeWeese
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
#include "startle/static_alloc.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/compile.h"
#include "parse/parse.h"
#include "debug/print.h"
#include "gen/cgen.h"
#include "gen/vlgen.h"
#include "eval.h"
#include "parse/lex.h"
#include "user_func.h"
#include "ir/trace.h"
#include "list.h"
#include "var.h"
#include "ir/analysis.h"

// size must be a prime number
STATIC_ALLOC(vl_set, uintptr_t, 31);
STATIC_ALLOC_DEPENDENT(vl_set_final, uintptr_t, vl_set_size);
STATIC_ALLOC(vl_outputs, uintptr_t, 16);

// table of corresponding Verilog types
static
const char *vltype(type_t t) {
  return t == T_LIST ? "stream" : "simple";
}

static
const char *vltype_full(const tcell_t *c) {
  type_t t = trace_type(c);
  if(t == T_OPAQUE) {
    val_t sym = trace_opaque_symbol(c);
    return symbol_string(sym);
  } else {
    return vltype(t);
  }
}

// for interleaving a separator
#define SEP(str) \
  const char *sep = ""; \
  const char *const sep_next = str

#define printf_sep(fmt, ...)                    \
  do {                                          \
    printf("%s" fmt, sep, ##__VA_ARGS__);       \
    sep = sep_next;                             \
  } while(0)

#define printf_psep(fmt, ...)                   \
  do {                                          \
    printf("%s" fmt, *sep, ##__VA_ARGS__);      \
    *sep = sep_next;                            \
  } while(0)

static
void print_type_and_dims(const trace_t *tr) {
  if(tr->type == T_OPAQUE) {
    val_t sym = tr->range.min;
    assert_error(sym == SYM_Array);
    printf("%s, (%d, %d)", symbol_string(sym), tr->addr_width, tr->bit_width);
  } else {
    printf("%s, %d", vltype(tr->type), tr->bit_width);
  }
}

// print module name and ports (everything before module body)
static
void gen_module_interface(const tcell_t *e) {
  const tcell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  trace_t tr;

  printf("module ");
  print_entry_cname(e);
  printf(" (\n");
  SEP(",\n");
  if(FLAG(*e, entry, SYNC)) {
    printf_sep("  `sync_ports");
  }
  COUNTDOWN(i, e->entry.in) {
    const tcell_t *a = &p[i];
    printf_sep("  `input(%s", STR_IF(!a->trace.bit_width, "null_"));
    print_type_and_dims(&a->trace);
    printf(", %d)", (int)(e->entry.in - 1 - i));
  }

  COUNTUP(i, out_n) {
    get_trace_info_for_output(&tr, e, i);
    printf_sep("  `output(%s", STR_IF(!tr.bit_width, "null_"));
    print_type_and_dims(&tr);
    printf(", %d)", (int)i);
  }

  printf("\n);\n");
}

static
void gen_constant(const tcell_t *c) {
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
    print_escaped_string(str, false);
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

// does this cell require synchronization?
static bool is_sync(const tcell_t *c) {
  tcell_t *entry;
  return
    !is_value(c) && !is_dep(c) &&
    (ONEOF(trace_type(c), T_LIST, T_OPAQUE) ||
     ((entry = get_entry(c)) &&
      FLAG(*entry, entry, SYNC)));
}

static bool is_blocking(const tcell_t *c) {
  return !is_passthrough(c) && is_sync(c);
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

// stack support: stack, stack pointer, labels, return registers
static
void gen_stack(const tcell_t *e) {
  int ra_bits = 0; // return address bits
  int rd_bits = 0; // return data bits
  int nt_self_calls = 0; // non-tail self calls
  int in_bits = 0; // input bits
  trace_t tr;
  RANGEUP(i, 1, e->entry.in + 1) {
    in_bits += e[i].trace.bit_width;
  }

  // calculate the width of labels
  if(FLAG(*e, entry, RETURN_ADDR)) {
    nt_self_calls = 0;
    int last_nt_bits = 0;
    FOR_TRACE_CONST(c, e) {
      if(is_self_call(e, c) &&
         NOT_FLAG(*c, trace, JUMP)) {
        nt_self_calls++;
        rd_bits += c->trace.bit_width;
        last_nt_bits = c->trace.bit_width;
      }
    }
    rd_bits -= last_nt_bits; // no need to store the last return
    ra_bits = int_log2(nt_self_calls);
  } else {
    nt_self_calls = 1;
    ra_bits = 0;
  }
  printf("\n"
         "  // stack\n"
         "  `define RB %d\n", ra_bits);
  if(ra_bits) { // generate the labels
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
  printf("  localparam STACK_WIDTH = `RB + %d;\n"
         "  reg [STACK_WIDTH-1:0] stack[0:255];\n"
         "  reg [7:0] sp;\n"
         "  reg returned;\n", in_bits + rd_bits);
  if(ra_bits) {
    printf("  reg [`RB-1:0] return_addr;\n");
  }
  COUNTUP(i, e->entry.out) { // a register for each return
    get_trace_info_for_output(&tr, e, i);
    printf("  reg [%d-1:0] return%d;\n", tr.bit_width, (int)i);
  }
}

static
void or_signals(const char *sig, const tcell_t *e, const tcell_t *c, const uintptr_t *outs, size_t n) {
  int ci = c - e;
  type_t t = trace_type(c);
  const char *pre = cname(t);
  SEP(" | ");
  printf("  assign %s%d_%s = ", pre, ci, sig);
  COUNTUP(i, n) {
    int oi = outs[i];
    const tcell_t *out = &e[oi];
    if(!is_dep(out) && !is_tail_call(e, out)) {
      printf_sep("inst%d_%s%d_%s", oi, pre, ci, sig);
    }
  }
  if(!*sep) printf("`false");
  printf(";\n");
}

static
bool in_vl_outputs(uintptr_t x) {
  STATIC_FOREACH(i, vl_outputs) {
    uintptr_t y = vl_outputs[i];
    if(!y) break;
    if(x == y) return true;
  }
  return false;
}

static
void get_outputs_np_step(const tcell_t *e, const tcell_t *c, uintptr_t const *const *backrefs, size_t *cnt) {
  const uintptr_t *outs;
  size_t n = get_outputs(e, c, backrefs, &outs);
  COUNTUP(i, n) {
    int oi = outs[i];
    if(in_vl_outputs(oi)) continue;
    const tcell_t *out = &e[oi];
    if(!is_dep(out)) {
      if(is_passthrough(out)) {
        if(tr_index(out->expr.arg[0]) == c - e) {
          get_outputs_np_step(e, out, backrefs, cnt);
        }
      } else {
        assert_error(*cnt < vl_outputs_size);
        vl_outputs[(*cnt)++] = oi;
      }
    }
  }
}

static
size_t get_outputs_np(const tcell_t *e, const tcell_t *c, uintptr_t const *const *backrefs) {
  static_zero(vl_outputs);
  size_t cnt = 0;
  get_outputs_np_step(e, c, backrefs, &cnt);
  return cnt;
}

static
void dup_streams(const tcell_t *e, const tcell_t *c, uintptr_t const *const *backrefs) {
  int ci = c - e;
  size_t n = get_outputs_np(e, c, backrefs), nt = 0;
  type_t t = trace_type(c);
  const char *pre = cname(t);
  COUNTUP(i, n) {
    int oi = vl_outputs[i];
    const tcell_t *out = &e[oi];
    if(!is_dep(out)) {
      nt++;
      printf("  `dup_signals(");
      print_type_and_dims(&c->trace);
      printf(", inst%d, %s%d);\n", oi, pre, ci);
    }
  }
  if(nt == 0) {
    printf("  assign %s%d_ready = `false;\n", pre, ci);
  } else {
    printf("  dup_%s #(.N(%d)) dup_%s%d(\n"
           "    .clk(clk),\n"
           "    .in_valid(%s%d_valid),\n"
           "    .in_ready(%s%d_ready),\n"
           "    .out_valid({", vltype_full(c), (int)nt, pre, ci, pre, ci, pre, ci);
    SEP(", ");
    COUNTUP(i, n) {
      int oi = vl_outputs[i];
      const tcell_t *out = &e[oi];
      if(!is_dep(out)) {
        printf_sep("inst%d_%s%d_valid", oi, pre, ci);
      }
    }
    printf("}),\n"
           "    .out_ready({");
    sep = "";
    COUNTUP(i, n) {
      int oi = vl_outputs[i];
      const tcell_t *out = &e[oi];
      if(!is_dep(out)) {
        printf_sep("inst%d_%s%d_ready", oi, pre, ci);
      }
    }
    printf("})\n"
           "  );\n");
    if(t == T_OPAQUE) {
      or_signals("addr", e, c, vl_outputs, n);
      or_signals("di", e, c, vl_outputs, n);
      or_signals("we", e, c, vl_outputs, n);
    }
  }
}

// declare registers and wires
static
void gen_decls(tcell_t *e, uintptr_t const *const *backrefs) {
  FOR_TRACE_CONST(tc, e) {
    int i = tc - e;
    type_t t = trace_type(tc);
    if(ONEOF(t, T_BOTTOM, T_RETURN)) continue;
    const char *null_str = STR_IF(!tc->trace.bit_width, "null_");
    if(is_var(tc)) { // inputs
      const char *decl = FLAG(*e, entry, RECURSIVE) && t != T_OPAQUE ? "variable" : "alias"; // <---
      printf("  `%s(%s", decl, null_str);
      print_type_and_dims(&tc->trace);
      printf(", %s%d, in%d);\n", cname(t), i, e->entry.in - i);
    } else {
      if(tc->op == OP_value) { // constants
        if(t == T_LIST) {
          printf("  `%sconst_nil(%s%d);\n", null_str, cname(t), i);
        } else {
          printf("  `%sconst(%s, %d, %s%d, ", null_str, vltype(t), tc->trace.bit_width, cname(t), i);
          gen_constant(tc);
          printf(");\n");
        }
      } else if(!is_tail_call(e, tc)) { // intermediate values
        if(is_sync(tc)) {
          printf("  wire inst%d_in_ready;\n", i);
        }
        if(t != T_OPAQUE && is_self_call(e, tc)) {
          printf("  `reg(%s%s, %d, %s%d);\n", null_str, vltype(t), tc->trace.bit_width, cname(t), i); // ***
        } else {
          if(tc->trace.bit_width || ONEOF(t, T_LIST, T_OPAQUE)) {
            printf("  `wire(%s", null_str);
            print_type_and_dims(&tc->trace);
            printf(", %s%d);\n", cname(t), i);
          }
        }
      } else if(t == T_LIST) { // nil
        printf("  `%sconst_nil(%s%d);\n", null_str, cname(t), i);
      }
    }
    if(direct_refs(&tc->c) > 1 && ONEOF(t, T_LIST, T_OPAQUE) && !is_passthrough(tc)) {
      dup_streams(e, tc, backrefs);
    }
  }
}

// find all inputs with which to synchronize
static
void find_sync_inputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, int depth) {
  while(is_passthrough(c)) {
    if(c->op == OP_assert && !depth) set_insert(c-e, set, size);
    c = &e[tr_index(c->expr.arg[0])];
  }
  if(!depth && is_sync(c) && !is_tail_call(e, c)) {
    set_insert(c-e, set, size);
  } else if(is_var(c)) {
    if(!depth) set_insert(e->entry.len+1, set, size);
  } else {
    TRAVERSE(c, const, in, ptrs) {
      const tcell_t *tc = &e[tr_index(*p)];
      if(is_dep(c) || !ONEOF(trace_type(tc), T_LIST, T_OPAQUE)) {
        find_sync_inputs(e, tc, set, size, max(0, depth-1));
      }
    }
  }
}

// synchronize the minimum set of inputs
// TODO document
static
void gen_sync_disjoint_inputs(const tcell_t *e, const tcell_t *c) {
  LOG("gen_sync_disjoint_inputs %E %d", e, c-e);
  static_zero(vl_set);
  static_zero(vl_set_final);
  find_sync_inputs(e, c, vl_set, vl_set_size, 1);
  STATIC_FOREACH(i, vl_set) {
    if(INRANGE(vl_set[i], 1, e->entry.len)) {
      find_sync_inputs(e, &e[vl_set[i]], vl_set_final, vl_set_final_size, 1);
    }
  }
  STATIC_FOREACH(i, vl_set_final) {
    if(vl_set_final[i]) {
      set_remove(vl_set_final[i], vl_set, vl_set_size);
    }
  }
  size_t inputs = squeeze(vl_set, vl_set_size);
  SEP(" & ");
  if(inputs) {
    COUNTUP(i, inputs) {
      if(vl_set[i] <= e->entry.len) {
        printf_sep("inst%d_out_valid", (int)vl_set[i]);
      } else {
        if(FLAG(*e, entry, RECURSIVE)) {
          printf_sep("active");
        } else if(FLAG(*e, entry, SYNC)) {
          printf_sep("in_valid");
        }
      }
    }
  }
  if(!*sep) printf("`true");
}

// synchronize the output of a block
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

// find all outputs with which to synchronize
static void find_all_sync_outputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, uintptr_t const *const *backrefs);
static
void find_sync_outputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, uintptr_t const *const *backrefs) {
  if(is_return(c)) return;
  const uintptr_t *outs;
  size_t n = get_outputs(e, c, backrefs, &outs);
  COUNTUP(i, n) {
    int oi = outs[i];
    const tcell_t *out = &e[oi];
    if(is_tail_call(e, out)) {
      // continue
    } else if(is_return(out) || is_blocking(out)) {
      set_insert(oi, set, size);
    } else if(is_passthrough(out) && cgen_lookup(e, out) != c - e) {
      // continue
    } else {
      find_all_sync_outputs(e, out, set, size, backrefs);
    }
  }
}

static
void find_all_sync_outputs(const tcell_t *e, const tcell_t *c, uintptr_t *set, size_t size, uintptr_t const *const *backrefs) {
  if(is_return(c)) return;
  if(!ONEOF(trace_type(c), T_LIST, T_OPAQUE)) {
    find_sync_outputs(e, c, set, size, backrefs);
  }
  if(!is_value(c)) {
    TRAVERSE(c, const, out) {
      const tcell_t *tc = &e[tr_index(*p)];
      if(!ONEOF(trace_type(tc), T_LIST, T_OPAQUE)) {
        find_sync_outputs(e, tc, set, size, backrefs);
      }
    }
  }
}

// synchronize the minimum set of outputs
// TODO document
static
void gen_sync_disjoint_outputs(const tcell_t *e, const tcell_t *c, uintptr_t const *const *backrefs) {
  static_zero(vl_set);
  static_zero(vl_set_final);
  LOG("gen_sync_disjoint_outputs %E %d", e, c-e);
  if(c > e) {
    find_sync_outputs(e, c, vl_set, vl_set_size, backrefs);
  } else {
    COUNTUP(i, e->entry.in) {
      find_sync_outputs(e, &e[i+1], vl_set, vl_set_size, backrefs);
    }
  }
  STATIC_FOREACH(i, vl_set) {
    if(INRANGE(vl_set[i], 1, e->entry.len)) {
      find_all_sync_outputs(e, &e[vl_set[i]], vl_set_final, vl_set_final_size, backrefs);
    }
  }
  STATIC_FOREACH(i, vl_set_final) {
    if(vl_set_final[i]) {
      //set_remove(vl_set_final[i], vl_set, vl_set_size);
    }
  }
  size_t outputs = squeeze(vl_set, vl_set_size);
  if(outputs) {
    bool top = false;
    SEP(" & ");
    COUNTUP(i, outputs) {
      if(!is_return(&e[vl_set[i]])) {
        printf_sep("inst%d_in_ready", (int)vl_set[i]);
      } else if(!top && FLAG(*e, entry, SYNC)) {
        printf_sep("out_ready");
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
    printf("%s%d", cname(t), (int)(c - e));
  }
}

// parameters for an argument e.g. width
static
void print_params_for_arg(const char *pre, const tcell_t *tc, int i,
                          const char **sep, const char *sep_next) {
  if(trace_type(tc) == T_OPAQUE) {
    if(is_array(tc)) {
      printf_psep(".%s%dAN(%d)", pre, i, tc->trace.addr_width);
      printf_psep(".%s%dDN(%d)", pre, i, tc->trace.bit_width);
    }
  } else {
    printf_psep(".%s%dN(%d)", pre, i, tc->trace.bit_width);
  }
}

// parameters for a module instance
static
void gen_params(const tcell_t *e, const tcell_t *c) {
  printf("#(");
  if(!is_user_func(c)) {
    csize_t
      in = closure_in(c),
      n = closure_args(c),
      start_out = n - closure_out(c);
    SEP(", ");
    COUNTUP(i, in) {
      print_params_for_arg("in", &e[cgen_index(e, c->expr.arg[i])], i, &sep, sep_next);
    }
    print_params_for_arg("out", c, 0, &sep, sep_next);
    RANGEUP(i, start_out, n) {
      int a = cgen_index(e, c->expr.arg[i]);
      if(a > 0) {
        print_params_for_arg("out", &e[a], i - start_out + 1, &sep, sep_next);
      }
    }
  }
  printf(")");
}

// make an instance (corresponding to an op)
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

  // name, synchronization, and params
  if(sync) {
    printf("    `inst_sync(");
    print_function_name(e, c);
    printf(", inst%d, ", inst);
    gen_params(e, c);
    printf(")(\n      `sync(");
    gen_sync_disjoint_inputs(e, c);
    printf(", ");
    gen_sync_disjoint_outputs(e, c, backrefs);
    printf("),");
  } else {
    printf("    `inst(");
    print_function_name(e, c);
    printf(", inst%d, ", inst);
    gen_params(e, c);
    printf(")(");
  }

  // inputs
  SEP(",");
  COUNTUP(i, in) {
    const tcell_t *a = &e[cgen_index(e, c->expr.arg[i])];
    type_t t = trace_type(a);
    const char *null_str = STR_IF(!a->trace.bit_width && t != T_OPAQUE, "null_");
    const char *dup_str = STR_IF(ONEOF(t, T_LIST, T_OPAQUE) && direct_refs(&a->c) > 1, "dup_");
    printf_sep("\n      `in(%s%s%s, %d, ",
               dup_str, null_str,
               vltype_full(a), (int)i); // ***
    print_var(e, a, block);
    printf(")");
  }

  // outputs
  printf_sep("\n      `out(%s%s, 0, %s%d)",
             STR_IF(!c->trace.bit_width, "null_"),
             vltype_full(c), cname(t), inst);
  RANGEUP(i, start_out, n) {
    int a = cgen_index(e, c->expr.arg[i]);
    if(a > 0) {
      type_t at = trace_type(&e[a]);
      printf_sep("\n      `out(%s%s, %d, %s%d)",
                 STR_IF(!e[a].trace.bit_width, "null_"),
                 vltype_full(&e[a]), (int)(i - start_out + 1), cname(at), a);
    }
  }

  printf("\n    );\n");
  if(sync) *sync_chain = inst;
}

// end one block and start the next
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
void gen_body(tcell_t *e, uintptr_t const *const *backrefs) {
  bool block_start = true;
  int block = 1;
  int return_block = 0;
  int block_sync_chain = 0;

  // top level synchronization
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
              char inst[8];
              int i = tr_index(*p);
              const tcell_t *a = &e[i];
              if(direct_refs(&a->c) > 1) {
                snprintf(inst, sizeof(inst), "inst%d_", (int)(c - e));
              } else {
                inst[0] = '\0';
              }
              if(trace_type(a) == T_LIST && direct_refs(&a->c) <= 1) { // *** HACK
                printf("    assign %s%s%d_ready = `true;\n", inst, cname(T_LIST), i);
              }
              if(is_array(a)) {
                // teminate bus
                printf("    assign `to_bus(%s%s%d) = 0;\n", inst, cname(T_OPAQUE), i);
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
// assign the data outputs of the module
static
bool gen_outputs(const tcell_t *e) {
  bool ret = false;
  csize_t out = e->entry.out;
  trace_t tr;
  COUNTUP(i, out) {
    get_trace_info_for_output(&tr, e, i);
    type_t t = tr.type;
    const tcell_t *r;
    if(tr.bit_width) {
      printf("  assign out%d =", (int)i);
      r = &e[e->trace.first_return];
      const tcell_t *r_prev = NULL;
      int block = 1;
      do {
        if(ONEOF(t, T_LIST, T_OPAQUE) || !is_tail_call(e, r)) {
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
    }
    if(ONEOF(t, T_LIST, T_OPAQUE)) {
      char inst[8];
      printf("  assign out%d_valid = ", (int)i);
      r = &e[e->trace.first_return];
      SEP(" | ");
      while(r > e) {
        int ri = r - e;
        int x = cgen_index(e, r->value.ptr[REVI(i)]);
        if(direct_refs(&e[x].c) > 1) {
          snprintf(inst, sizeof(inst), "inst%d_", ri);
        } else {
          inst[0] = '\0';
        }
        printf_sep("%s%s%d_valid", inst, cname(t), x);
        r = &e[tr_index(r->alt)];
      }
      printf(";\n");
      r = &e[e->trace.first_return];
      while(r > e) {
        int x = cgen_index(e, r->value.ptr[REVI(i)]);
        int ri = r - e;
        if(direct_refs(&e[x].c) > 1) {
          snprintf(inst, sizeof(inst), "inst%d_", ri);
        } else {
          inst[0] = '\0';
        }
        printf("  assign %s%s%d_ready = out%d_ready;\n", inst, cname(t), x, (int)i);
        if(t == T_OPAQUE) {
          printf("  assign %sintf%d_we = out%d_we;\n", inst, x, (int)i);
          printf("  assign %sintf%d_di = out%d_di;\n", inst, x, (int)i);
        }
        r = &e[tr_index(r->alt)];
      }
    }
    ret = true;
  }
  return ret;
}

// outgoing synchronization
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

// assign stack or loop registers to support recursion for each self call
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

// implement recursion by setting up for next iteration
static
void gen_loops(tcell_t *e) {
  printf("  always @(posedge clk) begin\n");
  if(FLAG(*e, entry, STACK))
     printf("    returned <= returning;\n");
  printf("    if(!nrst) begin\n"
         "      `reset(active);\n");
  if(FLAG(*e, entry, STACK)) {
    printf("      sp <= 0;\n");
  }
  printf("    end\n");
  printf("    else if(in_valid & ~active) begin\n");
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

    // pop from stack:
    // {label, data...} <= stack[sp - 1]; sp <= sp - 1;
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
void gen_module(tcell_t *e) {
  size_t backrefs_n = backrefs_size(e);
  assert_le(backrefs_n, 1024);
  uintptr_t const *backrefs[backrefs_n];
  build_backrefs(e, (uintptr_t **)backrefs, backrefs_n);

  e->op = OP_value;
  gen_module_interface(e);
  printf("\n");
  gen_decls(e, backrefs);
  if(FLAG(*e, entry, STACK)) {
    gen_stack(e);
  }
  gen_body(e, backrefs);
  printf("\n");
  if(FLAG(*e, entry, SYNC)) {
    gen_valid_ready(e);
    printf("\n");
  }
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
