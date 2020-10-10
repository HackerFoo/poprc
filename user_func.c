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
#include <stdlib.h>
#include <stdio.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/compile.h"
#include "ir/trace.h"
#include "list.h"
#include "user_func-local.h"
#include "debug/print.h"
#include "parse/parse.h" // for string_printf
#include "debug/tags.h"
#include "var.h"
#include "ir/analysis.h"
#include "primitive/core.h"
#include "parameters.h"

// hash_entry() of `[id] map`
#define ID_MAP_HASH 0xee5107e7

#if INTERFACE
#define is_user_func(c) _is_user_func(GET_CELL(c))
#endif

bool _is_user_func(const cell_t *c) {
  return c->op == OP_exec;
}

tcell_t *closure_entry(cell_t *c) {
  return is_user_func(c) ?
    (tcell_t *)c->expr.arg[closure_in(c)] :
    NULL;
}

const tcell_t *closure_entry_const(const cell_t *c) {
  return is_user_func(c) ?
    (tcell_t *)c->expr.arg[closure_in(c)] :
    NULL;
}

// alts in trace are used to temporarily point to cell instances
// they are copied to during expansion.
// this gets that pointer
static
cell_t *map_cell(tcell_t *entry, intptr_t x) {
  return x <= 0 ? NULL : entry[x].alt;
}

// get instance for a return
static
cell_t *get_return_arg(tcell_t *entry, tcell_t *returns, intptr_t x) {
  assert_error(x < entry->entry.out);
  trace_index_t i = tr_index(returns->value.ptr[x]);
  assert_error(i > 0);
  return map_cell(entry, i);
}

// given a list l with arity in -> out, produce an application of the list
// [X -> Y] => [X -> Y] apXY
static
cell_t *apply_list(cell_t *l, csize_t in, csize_t out) {
  LOG("apply list %C %d %d", l, in, out);
  cell_t *c = func(OP_ap, in + 1, out + 1);
  if(in) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(in - 1);
  } else {
    FLAG_CLEAR(*c, expr, NEEDS_ARG);
  }
  c->expr.arg[in] = l;
  RANGEUP(i, in+1, in+1+out) {
    c->expr.arg[i] = dep(c);
  }
  refn(c, out);
  return c;
}

static
bool match_op(const cell_t *a, const cell_t *b) {
  if(is_value(a) || is_value(b)) return false;
  if(a->op != b->op) return false;
  if(a->op == OP_exec &&
     closure_entry_const(a) !=
     closure_entry_const(b)) return false;
  if(closure_in(a) != closure_in(b)) return false;
  return true;
}

// build a binding list by applying the pattern to c
// don't produce bindings if tail is NULL, used to simplify before USE_TMP()
// TODO add reduction back in
static
cell_t **bind_pattern(tcell_t *entry, cell_t **cp, cell_t *pattern, cell_t **tail) {
  cell_t *c;
start:
  c = *cp;
  assert_error(c);
  CONTEXT("bind_pattern %s %C %C @barrier", strfield(entry, word_name), c, pattern);
  if(!pattern) return tail;
  if(c->tmp || (tail && c == *tail)) return tail;
  relates_to(c, pattern);
  if(c == pattern) { // c == pattern, so add all variables
    if(tail) {
      cell_t **t = tail;
      tail = trace_var_list(c, tail);
      FOLLOW(p, *t, tmp) {
        ref(p);
      }
    }
    if(entry && is_var(pattern)) switch_entry(entry, pattern);
  } else if(is_var(pattern)) { // bind the pattern variable to c
    assert_error(!pattern->alt);
    assert_error(!is_persistent(c));
    if(entry) {
      switch_entry(entry, pattern);
    } else {
      entry = var_entry(pattern->value.var);
    }
    LOG("bound %s[%d] = %C", entry->word_name, pattern->value.var-entry, c);
    if(tail) LIST_ADD(tmp, tail, ref(c));
  } else if(is_id_list(c)) { // walk through id lists
    cp = &c->value.ptr[0];
    if(!tail) simplify(cp);
    goto start;
  } else if(is_id_list(pattern)) {
    pattern = pattern->value.ptr[0];
    goto start;
  } else if(is_list(pattern) && !is_empty_list(pattern)) { // match inside list pattern

    // push entry in
    if(pattern->pos) entry = pos_entry(pattern->pos);

    if(is_list(c)) { // match the list
      list_iterator_t ci = list_begin(c), pi = list_begin(pattern);
      cell_t **cp, **pp;
      while(cp = list_next(&ci, true),
            pp = list_next(&pi, true),
            cp && pp) {
        if(!tail &&
           closure_is_ready(*cp) && !is_row_arg(&ci)) {
          simplify(cp); // ***
          LOG_WHEN((*cp)->alt, MARK("WARN") " bind drops alt %C -> %C", *cp, (*cp)->alt);
        }
        tail = bind_pattern(entry, cp, *pp, tail);
      }
      assert_error(!cp && !pp, "mismatched lists %C %C", c, pattern);
    } else { // c isn't expanded yet, so add code to extract match
      csize_t
        in = function_in(pattern),
        out = function_out(pattern, false);
      if(out) {
        // this will rip the list apart (later)
        cell_t *l = apply_list(ref(c), in, out);
        drop(l); // don't care about the result

        list_iterator_t it = list_begin(pattern);
        COUNTDOWN(i, out) {
          cell_t **p = list_next(&it, false);
          assert_error(p);
          cell_t **dp = &l->expr.arg[in+1+i], *d = *dp;
          tail = bind_pattern(entry, dp, *p, tail);
          drop(d);
        }
      }
    }
  } else if(pattern->op == OP_seq ||
            (pattern->op == OP_id && // walk through id
             pattern->expr.alt_set == 0)) {
    pattern = pattern->expr.arg[0];
    goto start;
  } else if(is_row_list(c)) {
    cell_t **r = left_elem(c);
    LOG("match through row list %C -> %C", c, *r);
    *cp = *r;
    *r = NULL;
    drop(c);
    goto start;
  } else if(ONEOF(c->op, OP_compose, OP_seq)) {
    cell_t **r = &c->expr.arg[0];
    LOG("match through %O %C -> %C", c->op, c, *r);
    *cp = *r;
    *r = NULL;
    drop(c);
    goto start;
  } else if(match_op(c, pattern)) { // fusion
    COUNTUP(i, closure_in(c)) {
      tail = bind_pattern(entry, &c->expr.arg[i], pattern->expr.arg[i], tail);
    }
  } else if(is_var(c)) { // if c is a var, there's no need to unify it
    LOG("bind var %C %C", c, pattern);
    if(tail) LIST_ADD(tmp, tail, ref(c));
  } else {
    // This can be caused by an operation before an infinite loop
    // This will prevent reducing the operation, and therefore fail to unify
    assert_error(false, "binding error: %C %C", c, pattern);
    // TODO promote to var instead of failing
    return NULL;
  }
  return tail;
}

// re-pack results of a recursive function back into a list
// see: func_exec_specialize
static
response exec_list(cell_t **cp, context_t *ctx) {
  PRE_NO_CONTEXT(exec_list);

  if(ctx->t != T_LIST || closure_out(c) != 0) {
    return SUCCESS;
  }

  CONTEXT_LOG("exec_list %C", c);
  cell_t *res;
  csize_t in = closure_in(c);
  tcell_t *entry = closure_entry(c);
  csize_t out = entry->entry.out - 1;
  bool row = FLAG(entry->entry, specialize, ROW);
  csize_t ln = max(entry->entry.out, ctx->s.out + row);
  assert_error(entry->entry.specialize);
  const uintptr_t dep_mask = entry->entry.specialize->dep_mask;

  if(NOT_FLAG(entry->entry, specialize, UNWRAPPED)) {
    return SUCCESS;
  }

  // the list may be larger than the outputs due to dropping,
  // so pad with fail_cell when corresponding bit isn't set in dep_mask
  cell_t *nc = expand(c, out);
  nc->expr.out = out;
  res = make_list(ln);
  cell_t **out_arg = &nc->expr.arg[in + 1];
  uintptr_t remaining = dep_mask;
  RANGEUP(i, 1, ln) {
    remaining &= ~(1 << i);
    cell_t *d = &fail_cell;
    if(dep_mask & (1 << i)) {
      d = remaining ? dep(ref(nc)) : nc;
      *out_arg++ = d;
    }
    res->value.ptr[REVI(i) - 1] = d;
  }
  if(!dep_mask || // can't drop all outputs
     dep_mask & 1) {
    res->value.ptr[ln-1] = nc;
  } else {
    res->value.ptr[ln-1] = &fail_cell;
  }

  if(FLAG(entry->entry, specialize, ROW)) {
    FLAG_SET(*res, value, ROW);
  }

  LOG(TODO " fix condition %C @condition", res);

  *cp = res;
  return RETRY;
}

// unify c with pattern pat if possible, returning the unified result
// The purpose is to fuse a recursive function to itself e.g.:
//   for F = g . f . g', where g . g' = id,
//       F^n = (g . f . g')^n = g . f^n . g'
// eliminating the intermediate cancelling g/g' pairs.
static
response unify_exec(cell_t **cp, context_t *ctx) {
  PRE(unify_exec, "#specialize");

  csize_t
    in = closure_in(c),
    out = closure_out(c);
  tcell_t *entry = closure_entry(c);
  cell_t *initial = entry->entry.specialize->initial;

  if(!initial) return FAIL;
  assert_error(NOT_FLAG(*c, expr, NO_UNIFY));
  assert_eq(in, closure_in(initial)); // TODO skip over non-changing args

  LOG_WHEN(out != 0, TODO " unify_convert %d: out(%d) != 0 @unify-multiout", c-cells, out);

  // simplify
  COUNTUP(i, in) {
    simplify(&c->expr.arg[i]);
    bind_pattern(NULL, &c->expr.arg[i], initial->expr.arg[i], NULL);
  }

  // match c against initial and extract variable bindings
  USE_TMP();
  cell_t
    *vl = 0,
    **tail = &vl;
  COUNTUP(i, in) {
    tail = bind_pattern(NULL, &c->expr.arg[i], initial->expr.arg[i], tail);
    if(!tail) {
      LOG("bind_pattern failed %C %C", c->expr.arg[i], initial->expr.arg[i]);
      break;
    }
  }

  if(!tail) { // match failed
    FOLLOW(p, vl, tmp) {
      drop(p);
    }
    clean_tmp(vl);
    return FAIL;
  }

  csize_t n_in = tmp_list_length(vl);
  assert_error(n_in, "no inputs %C", c);
  cell_t *n = ALLOC(n_in + out + 1,
    .expr.out = out,
    .op = OP_exec
  );
  n->expr.arg[n_in] = (cell_t *)entry;
  FLAG_SET(*n, expr, RECURSIVE);
  FLAG_SET(*n, expr, NO_UNIFY);

  // TODO this list should be sorted first by the parent variable pos's
  int pos = 0;
  FOLLOW(p, vl, tmp) { // get arguments from the binding list
    n->expr.arg[pos++] = p;
  }
  clean_tmp(vl);

  LOG("unified %s %C with initial %C",
      entry->word_name, c, initial);
  LOG_WHEN(closure_out(c), TODO " handle deps in c = %C, n = %C", c, n);

  TRAVERSE(c, in) {
    drop(*p);
  }
  reassign_deps(c, n);
  store_lazy(cp, n, 0);
  return exec_list(cp, ctx);
}

// reassign c's deps to r
static
void reassign_deps(cell_t *c, cell_t *r) {
  refcount_t n = 0;
  csize_t out_n = closure_out(c);
  assert_error(closure_out(r) == out_n);

  if(out_n) {
    cell_t
      **c_out = &c->expr.arg[closure_args(c) - out_n],
      **r_out = &r->expr.arg[closure_args(r) - out_n];
    COUNTUP(i, out_n) {
      cell_t *d = c_out[i];
      if(d) {
        d->expr.arg[0] = r;
        r_out[i] = d;
        n++;
      }
    }

    // update ref counts
    refn(r, n);
    assert_error(c->n >= n);
    c->n -= n;
  }
}

// expand the user function into an instance
static
cell_t *exec_expand(cell_t *c, tcell_t *new_entry) {
  size_t
    in = closure_in(c),
    out = closure_out(c),
    n = closure_args(c);
  tcell_t *entry = closure_entry(c);
  cell_t *res;
  tcell_t *returns = NULL;
  int pos = c->pos;

  WATCH(c, "exec_expand", "%E", entry);

  // pointers to the outputs
  cell_t **results[out + 1];
  results[out] = &res;
  COUNTUP(i, out) {
    cell_t **dp = &c->expr.arg[n - 1 - i]; // ***
    assert_error(!*dp || (*dp)->expr.arg[0] == c,
                 "%C refers to %C instead of %C",
                 *dp, (*dp)->expr.arg[0], c);
    results[i] = dp;
  }

  CONTEXT("exec_expand %s: %C 0x%x", entry->word_name, c, c->expr.flags);

  assert_error(entry->entry.len && FLAG(*entry, entry, COMPLETE));
  trace_clear_alt(entry); // *** probably shouldn't need this

  // assign function inputs to entry inputs
  COUNTUP(i, in) {
    tcell_t *p = &entry[i + 1];
    cell_t *a = c->expr.arg[REVI(i)];
    assert_error(is_var(p), "%d", i);
    if(p->n + 1 == 0) {
      // unused inputs
      p->alt = NULL;
      drop(a);
    } else {
      p->alt = refn(a, p->n);
    }
  }

  // allocate, copy, and index
  FOR_TRACE(p, entry, in + 1) {
    int i = p - entry;
    if(!p->op) {
      p->alt = 0;
      continue; // skip empty cells TODO remove these
    }
    if(trace_type(p) == T_RETURN) {
      // store a pointer to the first return
      if(!returns) returns = p;
      continue;
    }

    // allocate a cell and copy instruction
    tcell_t *e = is_user_func(p) ? get_entry(p) : NULL;
    cell_t *nc;
    csize_t p_in;
    if(e && e->entry.out == 1 &&
       (p_in = closure_in(p)) < e->entry.in) {
      // wrap incomplete function in a quote
      LOG("quote wrap %d", (int)i);
      nc = closure_alloc(e->entry.in + 1);
      memcpy(nc, &p->c, offsetof(cell_t, expr.arg));
      nc->size = e->entry.in + 1;
      csize_t remaining = e->entry.in - p_in;
      memcpy(&nc->expr.arg[remaining], p->expr.arg, p_in * sizeof(cell_t *));
      nc->expr.idx[0] = remaining - 1;
      nc->expr.arg[e->entry.in] = p->expr.arg[p_in];
      closure_set_ready(nc, false);
      nc = row_quote(nc);
    } else {
      nc = copy(&p->c);
      nc->expr.flags &= EXPR_TRACE; // only keep TRACE
      nc->n = p->n;
    }
    nc->tmp = 0;
    nc->src = c->src;
    p->alt = nc;
  }

  // check that a return was found
  assert_error(returns);

  // rewrite args/ptrs to pointers stored in alts
  FOR_TRACE(p, entry, in + 1) {
    int i = p - entry;
    cell_t *t = map_cell(entry, i);
    if((is_value(p) && p->value.type == T_RETURN) || !t) continue;

    unwrap_id_lists(&t); // for row quotes created above

    TRAVERSE(t, args, ptrs) {
      if(*p) {
        trace_index_t x = tr_index(*p);
        *p = map_cell(entry, x);
      }
    }

    // rewrite entries
    if(is_user_func(t)) {
      cell_t **t_entry = &t->expr.arg[closure_in(t)];
      tcell_t *e = tr_entry(*t_entry);
      *t_entry = (cell_t *)e;

      if(e == entry) {
        // recursive call
        FLAG_SET(*t, expr, RECURSIVE);
        t->pos = pos; // *** move tail calls out
        if(new_entry) {
          // replace if specialized
          *t_entry = (cell_t *)new_entry;
          LOG("replaced self call %C %E -> %E", c, entry, new_entry);
        }
      }
    }
  }

  // handle returns
  // HACK: only allocate alt ids when compiling
  bool tracing = trace_current_entry() != NULL || NOT_FLAG(*entry, entry, RECURSIVE);
  uint8_t alt_n = tracing ? int_log2(entry->entry.alts) : 0;
  uint8_t alt_id = new_alt_id(alt_n);
  unsigned int branch = 0;

  // first one
  alt_set_t alt_set = tracing ? as_multi(alt_id, alt_n, branch++) : 0;
  *results[out] = id(get_return_arg(entry, returns, out), alt_set);
  COUNTUP(i, out) {
    store_lazy_dep(*results[i], get_return_arg(entry, returns, i), alt_set);
  }

  // rest
  trace_index_t next = tr_index(returns->alt);
  while(next > 0) {
    alt_set_t as = tracing ? as_multi(alt_id, alt_n, branch++) : 0;
    returns = &entry[next];
    FOREACH(i, results) {
      cell_t *a = get_return_arg(entry, returns, i);
      if(*results[i]) {
        results[i] = &(*results[i])->alt;
        *results[i] = a ? id(a, as) : NULL;
      } else {
        drop(a);
      }
    }
    next = tr_index(returns->alt);
  }

  mark_pos(res, pos);
  return res;
}

// builds a temporary list of referenced variables
static
cell_t **input_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp_val && tail != &c->tmp) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
    } else {
      c->tmp_val = true; // prevent loops
      if(is_list(c)) {
        COUNTDOWN(i, list_size(c)) {
          tail = trace_var_list(c->value.ptr[i], tail);
        }
      } else {
        TRAVERSE(c, in) {
          tail = trace_var_list(*p, tail);
        }
      }
      c->tmp_val = false;
    }
  }
  return tail;
}

// remove vars from p that are not in entry
static
void vars_in_entry(cell_t **p, tcell_t *entry) {
  while(*p) {
    cell_t *v = *p;
    cell_t **next = &v->tmp;
    if(!var_for_entry(entry, v->value.var)) {
      // remove from list
      LOG("remove var %C", v);
      *p = *next;
      v->tmp = 0;
    } else {
      p = next;
    }
  }
}

// the argument order needs to be consistent on recursive calls
// re-number inputs to match the order they are reached from specialize->expand
static
void reassign_input_order(tcell_t *entry) {
  if(!entry->entry.specialize) return;
  cell_t *c = entry->entry.specialize->expand;
  if(!c) return;
  tcell_t *parent_entry = entry->entry.parent;
  CONTEXT("reassign input order %C (%s -> %s)", c,
          parent_entry->word_name, entry->word_name);
  USE_TMP(); // fix
  cell_t *vl = 0;
  input_var_list(c, &vl);
  vars_in_entry(&vl, entry); // ***
  on_assert_error(tmp_list_length(vl) == entry->entry.in,
                  "%d != %d, %s %C @specialize",
                  tmp_list_length(vl), entry->entry.in,
                  entry->word_name, c) {
    FOLLOW(p, vl, tmp) {
      LOG("input var %C", p);
    }
  }

  int i = entry->entry.in;
  FOLLOW(p, vl, tmp) {
    tcell_t *tn = var_for_entry(entry, p->value.var);
    assert_error(tn);
    assert_error(is_var(tn));
    assert_error(tn->var_index, "%s[%d] (%C)",
                 entry->word_name, tn-entry, p);
    tn->var_index = i--;
  }
  clean_tmp(vl);
}

// build a call into entry from its parent
// this doesn't work for quotes
// all c's input args must be params
static
cell_t *flat_call(cell_t *c, tcell_t *entry) {
  tcell_t *parent_entry = entry->entry.parent;
  CONTEXT("flat call %C (%s -> %s)", c,
          parent_entry->word_name, entry->word_name);
  USE_TMP(); // fix
  cell_t *vl = 0;
  input_var_list(c, &vl); // expand if needed
  FOLLOW(p, vl, tmp) {
    switch_entry(entry, p);
  }
  csize_t
    in = entry->entry.in,
    out = entry->entry.out;
  cell_t *nc = ALLOC(in + out,
    .expr.out = out - 1,
    .op = OP_exec
  );
  assert_error(tmp_list_length(vl) == in,
               "%d != %d, %s @specialize",
               tmp_list_length(vl), in, entry->word_name);

  int i = 0;
  FOLLOW(p, vl, tmp) {
    tcell_t *tn = get_var(entry, p);
    assert_error(tn, "get_var(%E, %C)", entry, p);
    // Is it okay to update this from reassign_input_order?
    tn->var_index = in - i;
    // assert_error(tn->var_index == i, "%T (%C)", p->value.var, p);
    switch_entry(parent_entry, p);
    cell_t *v = var_create_nonlist(T_ANY, p->value.var);
    nc->expr.arg[i] = v;
    LOG("arg[%d] -> %T", i, p->value.var);
    i++;
  }
  clean_tmp(vl);
  nc->expr.arg[in] = (cell_t *)entry;
  return nc;
}

// TODO generalize these

// pull out the contents of a list using ap
// i.e. remove a level of nesting: [[...] ...] -> [... ...]
// TODO handle row
static
cell_t *unwrap(cell_t *c, uintptr_t dep_mask, int out, int dropped, bool row) {
  int offset = list_size(c) - 1;

  // head (offset == 0)
  assert_error(is_list(c) && list_size(c) == 1, TODO " size = %d, %C", list_size(c), c);

  // N = out, ap0N swapN drop
  cell_t *l = make_list(out + offset - dropped + row);
  if(row) FLAG_SET(*l, value, ROW);
  cell_t *ap = ready_func(OP_ap, 1, out + 1);
  LOG("unwrap %C %d %C [%C]", c, out, l, ap);
  COUNTUP(i, offset) {
    l->value.ptr[i] = ref(c->value.ptr[i]);
  }
  cell_t **p = &l->value.ptr[offset];
  COUNTDOWN(i, out) {
    cell_t **a = &ap->expr.arg[i + 1];
    if(dep_mask & (1 << i)) {
      *p++ = *a = dep(ref(ap));
    } else {
      *a = NULL;
    }
  }
  ap->expr.arg[0] = ref(c->value.ptr[offset]);
  if(row) {
    *p++ = ap;
  } else {
    drop(ap);
  }
  drop(c);
  return l;
}

// [res dep_0 ... dep_out-1]
cell_t *wrap_vars(cell_t **res, cell_t *p, uintptr_t dep_mask, csize_t out) {
  assert_error(dep_mask);
  assert_error(out);
  cell_t *l = make_list(out);
  int offset = closure_args(p) - closure_out(p);
  cell_t **out_arg = &p->expr.arg[offset];
  cell_t *out_arg0 = NULL;
  LOG("wrap_vars %d %C", out, l);
  int dpos = offset;
  COUNTUP(i, out) {
    cell_t *d;
    if(dep_mask & (1 << i)) {
      if(out_arg0) {
        d = closure_alloc(1);
        store_dep(d, (*res)->value.var, dpos++, T_ANY, default_bound, 0);
        *out_arg++ = d;
      } else {
        d = out_arg0 = *res;
      }
    } else {
      d = &fail_cell;
    }
    l->value.ptr[REVI(i)] = d;
  }
  assert_error(out_arg0);
  *res = l;
  return out_arg0;
}

// expand a user function into a list of outputs
static
cell_t *expand_list(cell_t *c, tcell_t *new_entry) {
  size_t out = closure_out(c), n = out;
  cell_t *l = make_list(out + 1);
  TRAVERSE(c, out) {
    l->value.ptr[--n] = *p ? (*p = dep(c)) : &fail_cell;
  }
  refn(c, out);
  l->value.ptr[out] = exec_expand(c, new_entry); // deps will be in c ***
  return l;
}

// collect references to up to n deps from the context
// i.e. where the top n list items will go
static
context_t *collect_ap_deps(context_t *ctx, cell_t **deps, int n) {
  while(n > 0) {
    const cell_t *c = *ctx->src;
    if(c->op != OP_ap ||
       closure_in(c) != 1 ||
       c->n > count_out_used(c)) return NULL;
    int out = closure_out(c);
    int out_n = min(out, n);
    n -= out_n;
    memcpy(&deps[n],
           &c->expr.arg[closure_args(c) - out],
           out_n * sizeof(cell_t *));
    if(n > 0) ctx = ctx->up;
  }
  return ctx;
}

// generate a specialized sub-trace
static
response func_exec_specialize(cell_t **cp, context_t *ctx) {
  csize_t
    in = closure_in(*cp),
    out = closure_out(*cp);
  tcell_t *entry = closure_entry(*cp);
  specialize_data specialize;
  PRE(exec_specialize, "%s 0x%x #specialize", entry->word_name, (*cp)->expr.flags);
  LOG_UNLESS(entry->entry.out == 1, "out = %d #unify-multiout", entry->entry.out);

  specialize.entry = entry;
  specialize.flags = 0;

  // calculate dep_mask, which indicates which list items will be used,
  // as well as collecting references to where they will be stored.
  assert_error(ctx->s.out < sizeof_bits(specialize.dep_mask));
  specialize.dep_mask = (1 << ctx->s.out) - 1;
  int dropped = 0;
  context_t *top = NULL;
  if(ctx->t == T_LIST && ctx->s.out) {
    cell_t *deps[ctx->s.out];
    top = collect_ap_deps(ctx->up, deps, ctx->s.out);
    if(top) {
      COUNTUP(i, ctx->s.out) {
        if(!deps[i]) {
          specialize.dep_mask &= ~(1 << i);
          dropped++;
          LOG("dropped ap %C, out = %d", c, i);
        }
      }
      LOG("dep_mask %x", specialize.dep_mask);
    }
  }

  // start a new entry
  tcell_t *parent_entry = trace_current_entry();
  tcell_t *new_entry = trace_start_entry(parent_entry, entry->entry.out);
  new_entry->entry.specialize = &specialize;
  new_entry->module_name = parent_entry->module_name;
  int sub_id = parent_entry->entry.sub_id++;
  const char *sub_name = entry->entry.parent && FLAG(*entry->entry.parent, entry, RECURSIVE) ?
    suffix(entry->word_name, ':') :
    entry->word_name;
  new_entry->word_name = sub_id ?
    string_printf("%s:%s_%d", parent_entry->word_name, sub_name, sub_id) :
    string_printf("%s:%s", parent_entry->word_name, sub_name);
  LOG("created entry for %s(%d)", new_entry->word_name, TRACE_INDEX(new_entry));

  specialize.initial = TAG_PTR(ref(c), "specialize.initial");
  insert_root(&c);

  // make a list with expanded outputs of c
  cell_t *nc = COPY_REF(c, in);
  specialize.expand = TAG_PTR(nc, "specialize.expand");
  insert_root(&nc);

  // constrain movement of inputs
  mark_barriers(new_entry, nc);
  COUNTUP(i, in) {
    if(c->expr.arg[i]->op != OP_ap ||
       TWEAK(true, "to disable ap simplify %C", c->expr.arg[i]))
      simplify(&c->expr.arg[i]);
  }
  move_changing_values(new_entry, c);

  cell_t *l = expand_list(nc, new_entry);

  // eliminate intermediate list using unwrap
  // this will be undone by exec_list
  bool row = false;
  if(ctx->t == T_LIST) {
    row = FLAG(*entry, entry, ROW);
    l = unwrap(l, specialize.dep_mask, ctx->s.out, dropped, row);
    new_entry->entry.out += ctx->s.out - 1 - dropped + row;
    FLAG_SET(new_entry->entry, specialize, UNWRAPPED);
    if(row) {
      FLAG_SET(new_entry->entry, specialize, ROW);
      specialize.dep_mask |= 1 << ctx->s.out;
    }
  }

  // perform reduction
  TRAVERSE_REF(nc, in);
  new_entry->entry.alts = trace_reduce(new_entry, &l);
  drop(l);
  remove_root(&nc);

  // eliminate [id] map
  uintptr_t hash = hash_entry(new_entry);
  LOG("new_entry hash = 0x%x", hash);
  if(hash == ID_MAP_HASH) {
    LOG("[id] map %C %C", c, nc);

    cell_t *p = c->expr.arg[0];
    tcell_t *tn = get_var(parent_entry, p);
    cell_t *v = var_create_nonlist(tn->trace.type, tn);
    *cp = v;

    drop(nc);
    dropn(c, 2);
    remove_root(&c);
    trace_reset(new_entry);
    return SUCCESS;
  }

  // build self call
  TRAVERSE(nc, in) simplify(p);
  cell_t *p = flat_call(nc, new_entry);
  drop(nc);

  if(top) {
    trace_drop_return(new_entry, ctx->s.out, specialize.dep_mask);
  }

  // must drop everything so that trace_drop is called
  // before the trace is compacted and rearranged
  drop(specialize.initial);
  TRAVERSE(c, in) drop(*p);
  trace_end_entry(new_entry);

  trace_clear_alt(parent_entry);
  trace_t tr;

  get_trace_info_for_output(&tr, new_entry, 0);
  cell_t *res = var_create_with_entry(tr.type, parent_entry, p->size);
  res->value.range = tr.range;

  tcell_t *tc = res->value.var;

  // build list expected by caller
  if(FLAG_(specialize.flags, SPECIALIZE_UNWRAPPED)) {
    cell_t *l = wrap_vars(&res, p, specialize.dep_mask, max(new_entry->entry.out, ctx->s.out + row));
    if(FLAG_(specialize.flags, SPECIALIZE_ROW)) FLAG_SET(*res, value, ROW);
    trace_reduction(p, l);
  }

  // handle deps
  csize_t p_in = closure_in(p);
  if(out) {
    // replace outputs with variables
    int offset = new_entry->entry.out - 1 - out;
    cell_t **c_out = &c->expr.arg[in + 1];
    RANGEUP(i, offset, new_entry->entry.out) {
      cell_t *d = c_out[i - offset];
      if(d && is_dep(d)) { // NOTE should null deps be removed?
        assert_error(d->expr.arg[0] == c);
        drop(c);
        get_trace_info_for_output(&tr, new_entry, i + 1);
        store_dep(d, tc, i + p_in + 1, tr.type, tr.range, 0);
        d->value.range = tr.range;
        p->expr.arg[p_in + 1 + i] = d;
      }
    }
  }

  trace_reduction(p, res);
  replace_cell(cp, ctx, res);
  add_conditions_from_array(res, p->expr.arg, in);
  remove_root(&c);
  drop(p);
  return SUCCESS;
}

// call trace_update on all reachable vars
static
void trace_update_all(cell_t *c) {
  TRAVERSE(c, in, ptrs) {
    if(*p) {
      if(is_var(*p)) {
        trace_update(*p);
      } else {
        trace_update_all(*p);
      }
    }
  }
}

// trace this call instead of expanding it
static
response func_exec_trace(cell_t **cp, context_t *ctx) {
  size_t in = closure_in(*cp);
  tcell_t *entry = closure_entry(*cp);
  PRE(exec_trace, "%s 0x%x", entry->word_name, (*cp)->expr.flags);

  cell_t *res;
  const size_t out = closure_out(c) + 1;
  trace_t tr;
  assert_ge(out, entry->entry.out);
  if(in < entry->entry.in) { // move this into specialization of v?
    assert_error(in && c->expr.arg[0]);
    int offset = entry->entry.in - in;
    cell_t *v = c->expr.arg[0]; // ***
    while(is_id_list(v)) v = v->value.ptr[0];
    assert_error(is_var(v));
    tcell_t *v_entry = get_entry(v->value.var);
    assert_error(v_entry);
    assert_eq(closure_args(v->value.var), v_entry->entry.in + v_entry->entry.out);
    assert_error(offset < v_entry->entry.out);
    c = expand(c, offset); // *** could break deps
    assert_eq(closure_args(c), entry->entry.in + entry->entry.out);
    memmove(&c->expr.arg[entry->entry.in], &c->expr.arg[in], sizeof(cell_t *) * out);
    int d0 = closure_args(v->value.var) - closure_out(v->value.var);
    COUNTUP(i, offset) {
      c->expr.arg[i + in] = dep_var(v, d0 + i, T_ANY);
    }
    in = entry->entry.in;
  }

  // reduce all inputs
  if(in) {
    csize_t n = 0;
    uint8_t in_types[in];
    val_t in_opaque[in];
    memset(in_types, 0, in * sizeof(in_types[0]));
    reassign_input_order(entry);

    // find types of inputs
    FOR_TRACE(p, entry) {
      if(is_var(p) && p->var_index) {
        assert_le(p->var_index, entry->entry.in);
        int i = entry->entry.in - p->var_index;
        type_t t = p->value.type;
        in_types[i] = t;
        if(t == T_OPAQUE) in_opaque[i] = p->trace.range.min;
        if(++n >= in) break;
      }
    }

    // first reduce up to assertions
    COUNTUP(i, in) {
      type_t t = in_types[i];
      context_t arg_ctx = t == T_OPAQUE ?
        CTX(opaque, in_opaque[i]) :
        CTX(t, t);
      CHECK_IF(WITH(x, &arg_ctx,
                    x->flags &= ~CONTEXT_REDUCE_LISTS,
                    x->priority = PRIORITY_ASSERT - 1,
                    reduce(&c->expr.arg[i], x)) == FAIL, FAIL);
    }

    // reduce again the usual way
    COUNTUP(i, in) {
      CHECK(reduce_arg(c, i, &CTX(t, in_types[i])));
      CHECK_IF(as_conflict(ctx->alt_set), FAIL);
      cell_t *a = c->expr.arg[i];
      LOG_WHEN(a->alt, "split %C[%d] = %C #exec_split", c, i, a);
    }
  }
  CHECK_DELAY();

  // HACK force lists
  COUNTUP(i, in) {
    cell_t **ap = &c->expr.arg[i];
    cell_t *left;
    if(is_list(*ap) &&
       closure_is_ready(left = *leftmost(ap))) {
      LOG(HACK " forced cells[%C].expr.arg[%d]", c, i);
      CHECK(WITH(x, &CTX(return),
                 x->priority = PRIORITY_TOP,
                 func_list(ap, x)));
      CHECK_DELAY();

      // ensure quotes are stored first
      *ap = trace_quote_var(trace_current_entry(), *ap); // ***
    }
  }

  specialize_data *specialize = entry->entry.specialize;
  if(specialize) {
    trace_update_all(specialize->expand);
  }

  // get type and create return var
  get_trace_info_for_output(&tr, entry, 0);
  if(ONEOF(tr.type, T_ANY, T_BOTTOM) && ctx->t != T_ANY) {
    tr.type = ctx->t;
  }
  res = var(tr.type, c);
  res->value.range = tr.range;

  // replace outputs with variables
  cell_t **c_out = &c->expr.arg[in + 1];
  COUNTUP(i, out-1) {
    cell_t *d = c_out[i];
    if(d && is_dep(d)) {
      assert_error(d->expr.arg[0] == c);
      drop(c);
      get_trace_info_for_output(&tr, entry, i + 1);
      store_dep(d, res->value.var, i + in + 1, tr.type, tr.range, ctx->alt_set);
      d->value.range = tr.range;
    }
  }

  add_conditions_from_array(res, c->expr.arg, in);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

bool is_input(cell_t *v) {
  return is_var(v) && is_var(v->value.var);
}

// returns true if the function can not be specialized further
bool is_specialized_to(tcell_t *entry, cell_t *c) {
  int in = entry->entry.in;
  assert_error(in == closure_in(c));
  if(NOT_FLAG(*entry, entry, RECURSIVE)) {
    return false;
  }
  COUNTUP(i, in) {
    int a = REVI(i);
    cell_t **p = &c->expr.arg[a];
    if(FLAG(entry[i+1], trace, CHANGES)) {
      if(ONEOF(entry[i+1].value.type, T_LIST, T_ANY)) {
        simplify(p);
        if(is_user_func(*p)) {
          tcell_t *e = closure_entry(*p);
          if(e->entry.alts == 1 && FLAG(*e, entry, ROW)) {
            return false;
          }
        } else if(ONEOF((*p)->op, OP_pushr, OP_compose)) {
          return false;
        } else if(is_value(*p)) {
          while(is_id_list(*p)) p = &(*p)->value.ptr[0];
          if(is_var(*p) && (*p)->value.type == T_LIST && FLAG(**p, value, ROW)) {
            return false;
          }
        }
      }
    } else {
      force(p); // HACK
      if(!is_input(*p)) {
        return false;
      }
    }
  }
  return true;
}

// copy condition chain (assert, seq, unless) up to old, replace with new
static
cell_t *with_conditions(const cell_t *c, const cell_t *old, cell_t *new) {
  if(new == old) return (cell_t *)c;
  while(is_id_list(c)) c = c->value.ptr[0];
  if(c == old) return new;
  assert_error(ONEOF(c->op, OP_assert, OP_unless, OP_seq, OP_id));
  cell_t *nc = copy(c);
  nc->expr.arg[0] = with_conditions(c->expr.arg[0], old, new);
  RANGEUP(i, 1, closure_in(c)) ref(c->expr.arg[i]);
  return nc;
}

// find recursive call
static
cell_t *find_call(cell_t *l) {
  if(!l) return NULL;
  FOLLOW(p, *left_elem(l), expr.arg[0]) { // ***
    while(is_id_list(p)) p = p->value.ptr[0];
    if(is_user_func(p)) {
      return p;
    }
  }
  return NULL;
}

// expand tail call to add more return values
static
cell_t *expand_tail_call(tcell_t *entry, cell_t *l, int ri, cell_t *call, int n) {
  if(!l || n <= 1) return ref(l);
  int lsize = list_size(l);
  assert_error(lsize);
  int nlsize = lsize - 1 + n;

  assert_error(closure_entry(call) == entry, "expected call %E, got %E %C", entry, closure_entry(call), call);

  // expand the call and return
  cell_t *nl = make_list(entry->entry.out);
  csize_t new_call_size = closure_in(call) + entry->entry.out;
  cell_t *new_call = closure_alloc(new_call_size);
  memcpy(new_call, call, offsetof(cell_t, expr.arg[closure_in(call) + 1]));
  new_call->size = new_call_size;
  new_call->expr.out = entry->entry.out - 1;
  TRAVERSE_REF(new_call, in);
  cell_t **p = &nl->value.ptr[nlsize - 1];
  cell_t *r = l->value.ptr[ri];
  cell_t **deps = get_closure_deps(new_call);
  *p-- = with_conditions(r, call, new_call);
  COUNTUP(i, entry->entry.out - 1) {
    cell_t *d = dep(new_call);
    deps[i] = d;
    *p-- = with_conditions(r, call, d);
  }
  new_call->n = entry->entry.out - 1;

  // expand rest of alts
  nl->alt = expand_tail_call(entry, l->alt, ri, find_call(l->alt), n);

  return nl;
}

// remove inner recursive calls that have been lifted out of loops
static
cell_t *lift_recursion(cell_t *c,
                       cell_t *l,
                       int ri,
                       int n,
                       uintptr_t lifted_args) {
  assert_counter(10000);
  int lsize = list_size(l);
  assert_lt(lsize, 20);
  assert_error(lsize);
  int nlsize = lsize - 1 + n;
  tcell_t *entry = trace_current_entry();
  LOG("lift_recursion %E %C %C %d -> %d", entry, c, l, lsize, nlsize);
  entry->entry.out = nlsize;
  cell_t *nl = make_list(nlsize);
  COUNTUP(i, lsize - 1) {
    nl->value.ptr[i] = ref(l->value.ptr[i]);
  }
  cell_t **p = &nl->value.ptr[nlsize - 1];
  COUNTUP(i, n) {
    if(lifted_args & (1ul << i)) {
      *p-- = with_conditions(l->value.ptr[ri], c, ref(c->expr.arg[i])); // ***
    }
  }

  if(l->alt) {
    cell_t *call = find_call(l->alt);
    if(closure_entry(call) == closure_entry(c)) {
      nl->alt = lift_recursion(call, l->alt, ri, n, lifted_args);
    } else {
      nl->alt = expand_tail_call(entry, l->alt, ri, call, n);
    }
  } else {
    nl->alt = NULL;
  }
  return nl;
}

bool has_var(const cell_t *c) {
  if(!c) return false;
  if(is_var(c)) return true;
  TRAVERSE(c, const, in, ptrs) {
    if(has_var(*p)) return true;
  }
  return false;
}

static
bool eliminate_seq(context_t *ctx) {
  if(ctx->flags & CONTEXT_SEQ) {
    cell_t **cp = NULL;
    FOLLOW(x, ctx, up) {
      cp = x->src;
      if(!(x->flags & CONTEXT_SEQ)) {
        break;
      }
      x->flags |= CONTEXT_RETRY;
    }
    cell_t *c = *cp;
    *cp = ref(c->expr.arg[0]);
    drop(c);
    return true;
  }
  return false;
}

static
response func_exec_lift_recursion(tcell_t *entry, cell_t **cp, context_t *ctx) {
  cell_t *c = *cp;
  assert_error(!(ctx->flags & CONTEXT_REDUCE_LISTS));
  assert_error(FLAG(*entry, entry, ROW)); // TODO
  csize_t in = closure_in(c);
  int n = 0;
  uintptr_t lifted_args = 0;

  // find args that should escape the loop
  assert_error(in < sizeof_bits(lifted_args));
  COUNTUP(i, in) {
    if(has_var(c->expr.arg[i])) {
      lifted_args |= 1ul << i;
      n++;
    }
  }
  if(n == 1) {
    store_reduced(cp, ctx, ref(c->expr.arg[0])); // remove call
    return SUCCESS;
  } else if(!eliminate_seq(ctx)) {
    cell_t **l = NULL, **ptr = NULL;
    FOLLOW(p, ctx, up) { // cause retry up to return
      p->flags |= CONTEXT_RETRY;
      if(l && *l != *p->src) ptr = l;
      l = p->src;
    }
    assert_error(*l && ptr);
    int ri = ptr - (*l)->value.ptr; // return index
    cell_t *nl = lift_recursion(c, *l, ri, n, lifted_args);
    drop(*l);
    *l = nl;
  }
  return RETRY;
}

// debug tracing
void debug_trace(tcell_t *entry, cell_t *c) {
  if(FLAG(*entry, entry, TRACE)) {
    printf(NOTE("TRACE") " %s.%s", entry->module_name, entry->word_name);
    TRAVERSE(c, in) {
      putchar(' ');
      show_one(*p);
    }
    printf("\n");
    if(break_on_trace) {
      LOG("break on trace: %C", c);
      breakpoint();
    }
  }
}

OP(exec) {
  tcell_t *entry = closure_entry(*cp);
  PRE(exec, "%s", entry->word_name);

  if(trace_current_entry() && // tracing enabled and not unrolling
     !(FLAG(*entry, entry, ROW) && ctx->t == T_LIST && ctx->s.out)) {

    if(NOT_FLAG(*entry, entry, COMPLETE)) { // entry is incomplete
      delay_branch(ctx, PRIORITY_DELAY);

      // unify a self call with the initial call
      CHECK_PRIORITY(PRIORITY_EXEC_SELF);
      if(entry->entry.specialize && NOT_FLAG(*c, expr, NO_UNIFY)) {
        rsp = unify_exec(cp, ctx);
        if(rsp == FAIL) {
          LOG(MARK("WARN") " unify failed: %C %C",
              *cp, entry->entry.specialize->initial);
          ABORT(FAIL);
        }
        if(rsp != SUCCESS) return rsp;
      }
      return func_exec_trace(cp, ctx);
    } else if(is_specialized_to(entry, c)) {
      return func_exec_trace(cp, ctx);
    } else if(FLAG(*entry, entry, RECURSIVE)) {
      CHECK_PRIORITY(PRIORITY_EXEC_SELF);
      if(c->pos && c->pos <= trace_current_entry()->pos) {
        // lift tail call out of loop
        return func_exec_lift_recursion(entry, cp, ctx);
      } else {
        return func_exec_specialize(cp, ctx);
      }
    }
  }

  // inline the function
  assert_counter(1000);

  if(c->pos && ctx->t == T_LIST) {
    tcell_t *e = pos_entry(c->pos);
    c = unique(cp);
    c->pos = e->pos; // *** to move tail calls out
    mark_barriers(e, c);
    move_changing_values(e, c);
  }

  if(FLAG(*c, expr, RECURSIVE)) {
    TRAVERSE(c, in) {
      CHECK(force(p));
    }
    CHECK_DELAY();
  }
  cell_t *res = exec_expand(c, NULL);

  debug_trace(entry, c);
  store_lazy(cp, res, 0);
  return RETRY;

 abort:
  return abort_op(rsp, cp, ctx);
}

bool is_self_call(const tcell_t *e, const tcell_t *c) {
  return is_user_func(c) && get_entry(c) == e;
}
