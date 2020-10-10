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

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"
#include "startle/support.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "list.h"
#include "var.h"
#include "ir/analysis.h"
#include "user_func.h"
#include "ir/compile.h"

static
bool quote_has_call(const tcell_t *e, const tcell_t *call) {
  if(NOT_FLAG(*e, entry, QUOTE)) return false;
  FOR_TRACE_CONST(p, e) {
    if(is_user_func(p)) {
      const tcell_t *pe = get_entry(p);
      if(pe == call ||
         quote_has_call(pe, call)) return true;
    }
  }
  return false;
}

// check for recursion and throw errors on nontermination
bool trace_recursive_changes(tcell_t *entry) {
  bool changes = false;
  bool non_tail_call = false;
  bool forced_inline = FLAG(*entry, entry, FORCED_INLINE);

  FOR_TRACE(p, entry) {
    if(is_user_func(p)) {
      const tcell_t *pe = get_entry(p);
      if(pe == entry) {
        bool branch_changes = false;
        csize_t in = closure_in(p);
        assert_error(in == entry->entry.in,
                     "incorrect self call arity at %s %d",
                     entry->word_name, p-entry);
        COUNTUP(i, in) {
          trace_index_t v = in - i;
          if(tr_index(p->expr.arg[i]) != v) {
            tcell_t *a = &entry[v];
            assert_error(is_var(a));
            branch_changes = true;
            // mark variables that change during recursion
            FLAG_SET(*a, trace, CHANGES);
          }
        }

        // if !branch_changes, a recusive call has been made without modifying any arguments
        // so a tail call will loop forever without producing anything
        non_tail_call |= NOT_FLAG(*p, trace, JUMP);
        if(!forced_inline) {
          assert_throw(NOT_FLAG(*p, trace, JUMP) || branch_changes, "infinite tail recursion, tail call with constant args");
        }
        changes = true;
      } else if(quote_has_call(pe, entry)) {
        non_tail_call = true;
        changes = true;
      }
    }
  }

  /* TODO check isn't conservative enough
  // if there's only one path, even if the arguments change, it will loop forever
  if(!forced_inline) {
    // very conservative check, should only fail if the function is definitely non-terminating
    // TODO use return types e.g. T_BOTTOM
    assert_throw(!changes
                 || non_tail_call // could be lazy
                 || entry->entry.alts > 1 // maybe only one of alts are recursive
                 || entry->entry.out > 1, // could drop recursive path
                 "infinite tail recursion, single alt");
  }
  */
  return changes;
}

// set SYNC, STACK, RETURN_ADDR, RAM flags to indicate
// where extra resources are needed for vlgen
// SYNC: needs synchronization logic
// STACK: needs a stack
// RETURN_ADDR: stack will need labels
// RAM: uses RAM
void hw_analysis(tcell_t *e) {
  // mark functions that use recursive functions
  if(FLAG(*e, entry, RECURSIVE)) {
    FLAG_SET(*e, entry, SYNC);
  }
  FOR_TRACE_CONST(c, e) {
    if(ONEOF(trace_type(c), T_LIST, T_OPAQUE)) {
      FLAG_SET(*e, entry, SYNC);
    }
    if(is_user_func(c)) {
      tcell_t *entry = get_entry(c);
      if(!entry) continue;
      if(entry == e && NOT_FLAG(*c, trace, JUMP)) {
        if(FLAG(*e, entry, STACK)) {
          FLAG_SET(*e, entry, RETURN_ADDR);
        }
        FLAG_SET(*e, entry, STACK);
      }
      if(FLAG(*entry, entry, RECURSIVE) ||
         FLAG(*entry, entry, SYNC)) {
        FLAG_SET(*e, entry, SYNC);
      }
      if(FLAG(*entry, entry, RAM)) {
        FLAG_SET(*e, entry, RAM);
      }
    } else if(c->op == OP_ap) {
      FLAG_SET(*e, entry, RAM);
    }
  }
}

// creates a hash to be used as a fingerprint
// NOTE: the same hash only means two things are similar (if no collisions),
//       not identical i.e. some details are intentionally ignored.
#define HASH(l, x) (hash = (hash * 1021 + (uint32_t)(l)) * 1979 + (uint32_t)(x))
uint32_t hash_trace_cell(tcell_t *entry, tcell_t *tc) {
  uint32_t hash = 1;
  int idx = tc - entry - 1;

  if(!tc || !tc->op ||
     !INRANGE(idx, 0, entry->entry.len)) {
    HASH('n', 1);
    return hash;
  }

  if(tc->trace.hash) return tc->trace.hash;

  if(is_value(tc)) {
    if(is_var(tc)) {
      HASH('v', tc->var_index);
    } else if(tc->value.type == T_LIST) {
      TRAVERSE(tc, ptrs) {
        HASH('p', hash_trace_cell(entry, &entry[tr_index(*p)]));
      }
    } else if(tc->value.type == T_INT) {
      HASH('i', tc->value.integer);
    } else if(tc->value.type == T_SYMBOL) {
      HASH('y', tc->value.symbol);
    }
    // HASH('t', tc->value.type);
  } else {
    if(is_dep(tc)) {
      HASH('d', dep_arg_index(entry, tc));
    } else {
      // TODO hash entry arg
      TRAVERSE(tc, in) {
        HASH('a', hash_trace_cell(entry, &entry[tr_index(*p)]));
      }
      if(is_user_func(tc)) {
        tcell_t *e = get_entry(tc);
        HASH('u', e->trace.hash);
      }
    }
    HASH('o', tc->op);
    HASH('O', closure_out(tc));
  }
  tc->trace.hash = hash;
  return hash;
}

uint32_t hash_entry(tcell_t *entry) {
  uint32_t hash = 1;
  FOR_TRACE(tc, entry) {
    tc->trace.hash = 0;
  }
  FOR_TRACE(tc, entry) {
    HASH('c', hash_trace_cell(entry, tc));
  }
  entry->trace.hash = hash;
  return hash;
}
#undef HASH

FORMAT(tag, 'H') {
  tag_t tag;
  write_tag(tag, i);
  printf(FORMAT_TAG, tag);
}

// *** temporary
bool is_tail_call(const tcell_t *entry, const tcell_t *c) {
  return
    FLAG(*c, trace, JUMP) &&
    FLAG(*entry, entry, RECURSIVE);
}

// last use analysis: finds points where a resource can be dropped
// NOTE: there can be multiple points, because each branch has a
//       point after which it cannot fail, hence references used
//       in other branches can be dropped.

static
void last_use_mark(tcell_t *entry, cell_t **p) {
  int x = tr_index(*p);
  tcell_t *a = &entry[x];
  if(x && NOT_FLAG(*a, trace, USED)) {
    tr_set_flags(p, TR_FINAL);
    FLAG_SET(*a, trace, USED);
  }
}

static
void clear_used(tcell_t *entry) {
  FOR_TRACE(tc, entry) {
    FLAG_CLEAR(*tc, trace, USED);
  }
}

static
void last_use_mark_cell(tcell_t *entry, cell_t *c) {
  if(is_value(c)) {
    if(is_return(c)) {
      COUNTUP(i, list_size(c)) {
        last_use_mark(entry, &c->value.ptr[i]);
      }
    }
  } else if(!is_dep(c)) {
    COUNTDOWN(i, closure_in(c)) {
      last_use_mark(entry, &c->expr.arg[i]);
    }
  }
}

void last_use_analysis(tcell_t *entry) {
  set_prev_cells(entry);
  clear_used(entry);

  // calls and returns
  FOR_TRACE_REV(tc, entry) {
    cell_t *c = &tc->c;
    last_use_mark_cell(entry, c);
    if(!is_return(c) && !is_dep(c) && !direct_refs(c)) {
      trace_set_type(tc, T_BOTTOM);
    }
  }

  // go back over non-partial segments for each branch
  bool partial = false;
  FOR_TRACE_REV(tc, entry) {
    cell_t *c = &tc->c;
    if(is_return(c)) {
      clear_used(entry);
      partial = false;
    } else if(is_expr(c) && FLAG(*c, expr, PARTIAL)) {
      partial = true;
    }

    if(!partial) {
      last_use_mark_cell(entry, c);
    }
  }
}

// mark jumps, where return information isn't needed e.g. tail calls
void mark_jumps(tcell_t *entry) {
  tcell_t *jump = NULL;
  FOR_TRACE(tc, entry) {
    cell_t *c = &tc->c;
    if(ONEOF(c->op, OP_seq, OP_assert, OP_unless, OP_pushr, OP_compose, OP_dep)) continue;
    if(is_return(c) && jump) {
      tcell_t *e = get_entry(jump);
      if(e == entry) {
        FLAG_SET(*tc, trace, JUMP);
      }
      FLAG_SET(*jump, trace, JUMP);
      jump = NULL;
      continue;
    }
    if(is_value(c)) continue;
    if(is_user_func(c)) {
      jump = tc;
      continue;
    }
    jump = NULL;
  }
}

// Recursively mark dependencies in a previous branch, breaking the dependency chain
// at a partial. Direct dependencies on that partial are not marked.
// TODO optimize
static
bool mark_no_skip(tcell_t *entry, tcell_t *c, int last_return, int last_partial) {
  bool res = true;
  int i = c - entry;
  if(!is_expr(c) || i < last_partial) return true;
  if(i < last_return && last_partial == 0) {
    // look for last partial before this
    FOR_TRACE_REV(p, entry, i) {
      if(FLAG(*p, expr, PARTIAL)) {
        last_partial = p - entry;
        break;
      }
    }

    // return if none is found
    if(last_partial == 0) return true;
  }

  if(i == last_partial) {
    last_partial = 0;
    res = false;
  }

  // mark dependencies
  if(ONEOF(c->op, OP_seq, OP_assert, OP_unless)) {
    res &= mark_no_skip(entry, &entry[tr_index(c->expr.arg[0])], last_return, last_partial);
  } else {
    TRAVERSE(c, in) {
      if(*p) {
        res &= mark_no_skip(entry, &entry[tr_index(*p)], last_return, last_partial);
      }
    }
  }

  if(res && i < last_return) FLAG_SET(*c, trace, NO_SKIP);
  return res;
}

// mark instructions that can't be skipped on failure
// used in cgen
void no_skip_analysis(tcell_t *entry) {
  int last_return = 0;
  FOR_TRACE(tc, entry) {
    if(is_return(tc)) {
      last_return = tc - entry;
    } else {
      mark_no_skip(entry, tc, last_return, 0);
    }
  }
}

// calculate the number of back references needed
size_t backrefs_size(const tcell_t *entry) {
  size_t n = entry->entry.len;
  FOR_TRACE_CONST(tc, entry) {
    if(!is_return(tc)) {
      n += tc->n + 1;
    }
  }
  return n;
}

// set nonzero entries in arr[0..n-1] to x
bool set_nonzero(uintptr_t *arr, size_t n, uintptr_t x) {
  assert_error(x);
  COUNTUP(i, n) {
    if(!arr[i]) {
      arr[i] = x;
      return true;
    }
  }
  return false;
}

// return a pointer (in *outputs) and size of an array of the outputs
size_t get_outputs(const tcell_t *entry, const tcell_t *c, uintptr_t const *const *backrefs, uintptr_t const **outputs) {
  int i = c - entry;
  assert_error(INRANGE(i, 1, entry->entry.len));
  *outputs = backrefs[i - 1];
  return entry[i].n + 1;
}

// build back reference data
void build_backrefs(const tcell_t *entry, uintptr_t **table, size_t size) {
  uintptr_t **index = table;
  uintptr_t *backref = (uintptr_t *)(table + entry->entry.len);
  memset(table, 0, sizeof(*table) * size);

  // build the index
  FOR_TRACE_CONST(tc, entry) {
    if(!is_return(tc)) {
      index[tc - entry - 1] = backref;
      backref += tc->n + 1;
    } else {
      index[tc - entry - 1] = NULL;
    }
  }

  assert_le((uintptr_t **)backref - table, (int)size);

  // fill backrefs
  FOR_TRACE_CONST(tc, entry) {
    TRAVERSE(tc, const, in, ptrs) {
      int x = tr_index(*p);
      UNUSED bool success = set_nonzero(index[x-1], entry[x].n + 1, tc - entry);
      assert_error(success);
    }
  }
}

// bits need for a range, or to store an absolute constant
int bits_needed(range_t r, bool is_constant) {
  if(range_empty(r) || !range_bounded(r) ||
     (!is_constant && range_singleton(r))) {
    return 0;
  } else if(r.min >= 0) { // unsigned
    return max(1, int_log2l(sat_add(r.max, 1)));
  } else { // signed
    return max(1, int_log2l(sat_add(max(r.max, sat_neg(r.min)), 1)) + 1);
  }
}

// calculate the width of a stream expression
int stream_bits(tcell_t *entry, tcell_t *tc, int offset) {
  if(!ONEOF(trace_type(tc), T_LIST, T_ANY, T_BOTTOM)) return offset; // ***
  if(tc->trace.bit_width && tc->trace.bit_width >= offset) {
    return tc->trace.bit_width;
  }
  if(is_value(tc) || is_dep(tc)) {
    return tc->trace.bit_width = offset;
  }
  if(is_user_func(tc)) {
    const tcell_t *e = get_entry(tc);
    trace_t tr;
    tr.bit_width = 0;
    if(e != entry) get_trace_info_for_output(&tr, e, 0);
    if(tr.bit_width == 0) {
      tc->trace.bit_width = offset;
    } else {
      assert_error(tr.bit_width >= offset);
      tc->trace.bit_width = tr.bit_width;
    }
    return tc->trace.bit_width;
  }

  int bits = 0;
  cell_t *const *stream_in =
    tc->op == OP_quote ? NULL : // no input
    tc->op == OP_ap ? &tc->expr.arg[closure_in(tc)-1] : // last input
    &tc->expr.arg[0]; // first input
  if(ONEOF(tc->op, OP_quote, OP_ap, OP_pushr)) {
    TRAVERSE(tc, const, in) {
      if(p != stream_in && *p) bits += bit_width(entry, &entry[tr_index(*p)]);
    }
    TRAVERSE(tc, const, out) {
      if(*p) bits -= bit_width(entry, &entry[tr_index(*p)]);
    }
  }
  if(stream_in) {
    bits += stream_bits(entry, &entry[tr_index(*stream_in)], -min(0, bits));
  }
  assert_ge(bits, 0);
  return tc->trace.bit_width = max(tc->trace.bit_width, bits);
}

// calculate the address and data width of an array expression
void array_bits(tcell_t *entry, tcell_t *tc, int aw, int bw) {
  assert_error(ONEOF(trace_type(tc), T_OPAQUE, T_ANY, T_BOTTOM));
  trace_set_type(tc, T_OPAQUE); // ***
  tc->trace.range.min = SYM_Array; // ***
  assert_error(trace_opaque_symbol(tc) == SYM_Array);
  if(tc->trace.addr_width && tc->trace.addr_width >= aw &&
     tc->trace.bit_width && tc->trace.bit_width >= bw) return;
  int addr, data;
  switch(tc->op) {
  case OP_read_array:
  case OP_write_array:
    addr = tr_index(tc->expr.arg[1]);
    data = tr_index(tc->expr.arg[2]);

    tc->trace.addr_width = max(aw, bit_width(entry, &entry[addr]));
    tc->trace.bit_width = max(bw, data ? bit_width(entry, &entry[data]) : 0);
    array_bits(entry, &entry[tr_index(tc->expr.arg[0])],
               tc->trace.addr_width,
               tc->trace.bit_width);
    break;
  case OP_dup_array:
    array_bits(entry, &entry[tr_index(tc->expr.arg[0])],
               tc->trace.addr_width = aw,
               tc->trace.bit_width = bw);
    break;
  case OP_dep: {
    tcell_t *c = &entry[tr_index(tc->expr.arg[0])];
    if(trace_type(c) == T_OPAQUE) {
      array_bits(entry, c,
                 tc->trace.addr_width = aw,
                 tc->trace.bit_width = bw);
    }
    break;
  }
  case OP_value:
    tc->trace.addr_width = aw;
    tc->trace.bit_width = bw;
  default:
    break;
  }
}

// calculate bit widths
void calculate_bit_width(tcell_t *entry) {
  LOG("calculate_bit_width %E", entry);
  FOR_TRACE(tc, entry) {
    bit_width(entry, tc);
  }
}

// calculate bit widths for one expression
int bit_width(tcell_t *entry, tcell_t *tc) {
  if(is_user_func(tc)) {
    const tcell_t *e = get_entry(tc);
    csize_t unbound = e->entry.in - closure_in(tc);
    RANGEUP(i, unbound, e->entry.in) {
      const tcell_t *v_e = &e[e->entry.in - i];
      type_t t = trace_type(v_e);
      tcell_t *v = &entry[tr_index(tc->expr.arg[i - unbound])];
      if(t == T_LIST) {
        stream_bits(entry, v, v_e->trace.bit_width);
      } else if(t == T_OPAQUE &&
                trace_opaque_symbol(v_e) == SYM_Array) {
        array_bits(entry, v, v_e->trace.addr_width, v_e->trace.bit_width);
      }
    }
    if(trace_type(tc) != T_BOTTOM &&
       (e != entry || trace_type(tc) != T_LIST)) {
      get_trace_info_for_output(&tc->trace, e, 0);
    }
  }
  if(!tc->trace.bit_width) {
    type_t t = trace_type(tc);
    range_t r = tc->trace.range;
    if(ONEOF(t, T_INT, T_SYMBOL) &&
       range_bounded(r) &&
       range_span(r) > 0) {
      tc->trace.bit_width = bits_needed(r, is_value(tc));
    } else if(t == T_LIST) {
      tc->trace.bit_width = stream_bits(entry, tc, 0);
    } else if(t == T_OPAQUE &&
              trace_opaque_symbol(tc) == SYM_Array) {
      array_bits(entry, tc, 0, 0);
    } else {
      tc->trace.bit_width = bits_needed(default_bound, is_value(tc));
    }
  }
  return tc->trace.bit_width;
}
