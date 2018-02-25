/* Copyright 2012-2017 Dustin DeWeese
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
#include "primitive.h"
#include "special.h"
#include "byte_compile.h"
#include "trace.h"
#include "list.h"
#include "user_func.h"
#include "print.h"
#include "parse.h" // for string_printf

bool is_user_func(const cell_t *c) {
  return c->op == OP_exec;
}

static
cell_t *map_cell(cell_t *entry, intptr_t x) {
  return
    x == NIL_INDEX ? &nil_cell :
    x <= 0 ? NULL :
    entry[x].alt;
}

static
cell_t *get_return_arg(cell_t *entry, cell_t *returns, intptr_t x) {
  trace_index_t i = trace_decode(returns->value.ptr[x]);
  assert_error(x < entry->entry.out);
  assert_error(i == NIL_INDEX || i > 0);
  return // can't use map_cell, returns empty_list() instead of &nil_cell
    i == NIL_INDEX ? empty_list() : entry[i].alt;
}

// given a list l with arity in -> out, produce an application of the list
// [X -> Y] => [X -> Y] apXY
cell_t *apply_list(cell_t *l, csize_t in, csize_t out) {
  cell_t *c = func(OP_ap, in + 1, out + 1);
  if(in) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(in - 1);
  } else {
    FLAG_CLEAR(c->expr, EXPR_NEEDS_ARG);
  }
  c->expr.arg[in] = l;
  RANGEUP(i, in+1, in+1+out) {
    c->expr.arg[i] = dep(c);
  }
  refn(c, out);
  return c;
}

// print a representation of a pattern for debugging
void print_pattern(cell_t *pattern) {
  if(is_var(pattern)) {
    printf(" ?%d", (int)(pattern->value.tc.index));
  } else if(is_list(pattern)) {
    csize_t in = function_in(pattern);
    cell_t **p;
    printf(" [");
    if(in) printf(" %d ->", in);
    FORLIST(p, pattern) {
      print_pattern(*p);
    }
    printf(" ]");
  } else {
    printf(" %c", 'A' + (char)((pattern-cells) % 26));
  }
}

// print the list of bindings for debugging
void print_bindings(cell_t *vl) {
  FOLLOW(p, q, vl, tmp) {
    csize_t x = p->value.tc.index;
    printf("?%d = %d\n", x, (int)(q-cells));
  }
}

// print the word to match and its patterns
void print_word_pattern(cell_t *word) {
  printf("pattern:");
  COUNTUP(i, closure_in(word)) {
    print_pattern(word->expr.arg[i]);
  }
  printf("\n");
}

// build a binding list by applying the pattern to c
// TODO add reduction back in
cell_t **bind_pattern(cell_t *c, cell_t *pattern, cell_t **tail) {
  assert_error(c);
  CONTEXT("bind_pattern %C %C @barrier", c, pattern);
  if(!pattern || !tail) return NULL;
  if(c->tmp || c == *tail) return tail;
  if(c == pattern) {
    cell_t **t = tail;
    tail = trace_var_list(c, tail);
    FOLLOW(p, *t, tmp) {
      ref(p);
    }
  } else if(is_var(pattern)) {
    // found a binding
    assert_error(!pattern->alt);
    LIST_ADD(tmp, tail, ref(c));
  } else if(is_list(pattern)) {
    if(is_list(c)) {
      assert_error(list_size(c) == list_size(pattern));
      COUNTDOWN(i, list_size(pattern)) {
        tail = bind_pattern(c->value.ptr[i],
                            pattern->value.ptr[i],
                            tail);
      }
    } else {
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
          cell_t *d = l->expr.arg[in+1+i];
          tail = bind_pattern(d, *p, tail);
          drop(d);
        }
      }
    }
  } else if(pattern->op == OP_id &&
            pattern->expr.alt_set == 0) {
    return bind_pattern(c, pattern->expr.arg[0], tail);
  } else {
    // This can be caused by an operation before an infinite loop
    // This will prevent reducing the operation, and therefore fail to unify
    assert_error(false, "binding error: %C %C", c, pattern);
    // TODO promote to var instead of failing
    return NULL;
  }
  return tail;
}

// unify c with pattern pat if possible, returning the unified result
bool unify_exec(cell_t **cp, cell_t *parent_entry) {
  cell_t *c = *cp;
  PRE(c, unify_exec, " #wrap");

  csize_t
    in = closure_in(c),
    out = closure_out(c);
  cell_t
    *entry = c->expr.arg[in],
    *pat = entry->entry.wrap->initial;

  if(!pat) return false;
  if(!is_value(c) &&
     FLAG(c->expr, EXPR_NO_UNIFY)) return true;

  assert_error(in == closure_in(pat));

  LOG_WHEN(out != 0, TODO " unify_convert %d: out(%d) != 0 @unify-multiout", c-cells, out);

  cell_t
    *vl = 0,
    **tail = &vl;
  COUNTUP(i, in) {
    tail = bind_pattern(c->expr.arg[i], pat->expr.arg[i], tail);
    if(!tail) {
      LOG("bind_pattern failed %C %C", c->expr.arg[i], pat->expr.arg[i]);
      break;
    }
  }

  if(tail) {
    csize_t in = tmp_list_length(vl);
    cell_t *n = closure_alloc(in + out + 1);
    n->expr.out = out;
    n->op = OP_exec;
    n->expr.arg[in] = entry;
    FLAG_SET(n->expr, EXPR_RECURSIVE);
    int pos = 0;
    FOLLOW(p, vl, tmp) { // get arguments from the binding list
      n->expr.arg[in - 1 - pos] = p;
      pos++;
    }
    clean_tmp(vl);

    LOG("unified %E %C with initial_word in %E %C",
        entry, c, parent_entry, pat);
    LOG_WHEN(closure_out(c), TODO " handle deps in c = %C, n = %C", c, n);

    TRAVERSE(c, in) {
      drop(*p);
    }
    store_lazy_and_update_deps(cp, n, 0);
    return true;
  } else {
    FOLLOW(p, vl, tmp) {
      drop(p);
    }
    clean_tmp(vl);
    return false;
  }
}

static
cell_t *find_input_entry(cell_t *c) {
  cell_t *entry = NULL;
  TRAVERSE(c, in, ptrs) {
    cell_t *x = clear_ptr(*p);
    if(x) {
      if(is_var(x)) {
        entry = x->value.tc.entry;
        break;
      } else if((entry = find_input_entry(x))) {
        break;
      }
    }
  }
  return entry;
}

static
cell_t *exec_expand(cell_t *c, cell_t *new_entry) {
  size_t
    in = closure_in(c),
    out = closure_out(c),
    n = closure_args(c);
  cell_t *entry = c->expr.arg[in];
  cell_t *res;
  cell_t *returns = NULL;
  cell_t **results[out + 1];

  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i]; // ***
  }

  CONTEXT("exec_expand %E: %C 0x%x", entry, c, c->expr.flags);

  assert_error(entry->entry.len && FLAG(entry->entry, ENTRY_COMPLETE));
  trace_clear_alt(entry); // *** probably shouldn't need this

  COUNTUP(i, in) {
    cell_t *p = &entry[i + 1];
    assert_error(is_var(p));
    p->alt = refn(c->expr.arg[in - 1 - i], p->n);
  }

  // allocate, copy, and index
  FOR_TRACE(p, entry, in) {
    int i = p - entry;
    if(!p->op) {
      p->alt = 0;
      continue; // skip empty cells TODO remove these
    }
    if(trace_type(p).exclusive == T_RETURN) {
      if(!returns) returns = p;
      continue;
    }
    cell_t *e = is_user_func(p) ? get_entry(p) : NULL;
    cell_t *nc;
    csize_t p_in;
    if(e && e->entry.out == 1 &&
       (p_in = closure_in(p)) < e->entry.in) {
      // wrap incomplete function in a quote
      LOG("quote wrap %d", (int)i);
      nc = closure_alloc(e->entry.in + 1);
      memcpy(nc, p, offsetof(cell_t, expr.arg));
      nc->size = e->entry.in + 1;
      csize_t remaining = e->entry.in - p_in;
      memcpy(&nc->expr.arg[remaining], p->expr.arg, p_in * sizeof(cell_t *));
      nc->expr.idx[0] = remaining - 1;
      nc->expr.arg[e->entry.in] = p->expr.arg[p_in];
      closure_set_ready(nc, false);
      nc = row_quote(nc);
    } else {
      nc = copy(p);
      nc->n = p->n;
    }
    nc->tmp = 0;
    p->alt = nc;
  }

  // check that a return was found
  assert_error(returns);

  // rewrite pointers
  FOR_TRACE(p, entry, in) {
    int i = p - entry;
    cell_t *t = map_cell(entry, i);
    if((is_value(p) && p->value.type.exclusive == T_RETURN) || !t) continue;

    if(is_row_list(t)) t = t->value.ptr[0]; // for row quotes created above

    // skip rewriting for the entry argument
    cell_t **t_entry = NULL;
    if(is_user_func(t)) {
      t_entry = &t->expr.arg[closure_in(t)];
      *t_entry = get_entry(t);
      if(*t_entry == entry) *t_entry = new_entry;
    }

    TRAVERSE(t, alt, args, ptrs) {
      if(p != t_entry && *p) {
        trace_index_t x = trace_decode(*p);
        *p = map_cell(entry, x);
      }
    }

    // TODO remove this
    if(t_entry &&
       *t_entry == entry) { // mark recursion
      FLAG_SET(t->expr, EXPR_RECURSIVE);
      //LOG("recursive exec %C -> %d", c, trace_ptr-trace_cur);
    }
  }

  // handle returns
  uint8_t alt_n = int_log2(entry->entry.alts);
  uint8_t alt_id = new_alt_id(alt_n);
  unsigned int branch = 0;

  // first one
  alt_set_t alt_set = as_multi(alt_id, alt_n, branch++);
  *results[out] = id(get_return_arg(entry, returns, out), alt_set);
  COUNTUP(i, out) {
    store_lazy_dep(*results[i], get_return_arg(entry, returns, i), alt_set);
  }

  // rest
  trace_index_t next = trace_decode(returns->alt);
  while(next >= 0) {
    alt_set_t as = as_multi(alt_id, alt_n, branch++);
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
    next = trace_decode(returns->alt);
  }

  return res;
}

bool is_within_entry(cell_t *entry, cell_t *p) {
  return p > entry && p <= entry + entry->entry.len;
}

// builds a temporary list of referenced variables
cell_t **input_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp && tail != &c->tmp) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
    } else {
      c->tmp = FLIP_PTR(0); // prevent loops
      if(is_list(c)) {
        COUNTDOWN(i, list_size(c)) {
          tail = trace_var_list(c->value.ptr[i], tail);
        }
      } else {
        TRAVERSE(c, in) {
          tail = trace_var_list(*p, tail);
        }
      }
      c->tmp = 0;
    }
  }
  return tail;
}

void vars_in_entry(cell_t **p, cell_t *entry) {
  while(*p) {
    cell_t *v = *p;
    cell_t **next = &v->tmp;
    if(v->value.tc.entry != entry) {
      // remove from list
      *p = *next;
      v->tmp = 0;
    }
    p = next;
  }
}

void reassign_input_order(cell_t *entry) {
  if(!entry->entry.wrap) return;
  cell_t *c = entry->entry.wrap->expand;
  if(!c) return;
  cell_t *parent_entry = entry->entry.parent;
  CONTEXT("reassign input order %C (%e -> %e)", c,
          parent_entry, entry);
  csize_t in = entry->entry.in;
  cell_t *vl = 0;
  input_var_list(c, &vl);
  vars_in_entry(&vl, entry);
  assert_error(tmp_list_length(vl) == in, "%d != %d, %E %C @wrap", tmp_list_length(vl), in, entry, c);

  int pos = 1;
  FOLLOW(p, vl, tmp) {
    assert_error(p->value.tc.entry == entry);
    cell_t *tn = trace_cell_ptr(p->value.tc);
    assert_error(tn->pos, "%e[%d] (%C)", p->value.tc.entry, p->value.tc.index, p);
    if(tn->pos != pos) {
      tn->pos = pos;
      FLAG_SET(entry->entry, ENTRY_MOV_VARS);
    }
    pos++;
  }
  clean_tmp(vl);
}

// this doesn't work for quotes
// all c's input args must be params
cell_t *flat_call(cell_t *c, cell_t *entry) {
  cell_t *parent_entry = entry->entry.parent;
  CONTEXT("flat call %C (%e -> %e)", c,
          parent_entry, entry);
  csize_t
    in = entry->entry.in,
    out = entry->entry.out;
  cell_t *nc = closure_alloc(in + out);
  nc->expr.out = out - 1;
  nc->op = OP_exec;
  cell_t *vl = 0;
  input_var_list(c, &vl);
  assert_error(tmp_list_length(vl) == in, "%d != %d, %E @wrap", tmp_list_length(vl), in, entry);

  int pos = 1;
  FOLLOW(p, vl, tmp) {
    assert_error(p->value.tc.entry == entry);
    cell_t *tn = trace_cell_ptr(p->value.tc);
    // Is it okay to update this from reassign_input_order?
    tn->pos = pos;
    // assert_error(tn->pos == pos, "%e[%d] (%C)", p->value.tc.entry, p->value.tc.index, p);
    switch_entry(parent_entry, tn);
    assert_error(tn->value.tc.entry == parent_entry);
    cell_t *tp = trace_cell_ptr(tn->value.tc);
    cell_t *v = var_create_nonlist(T_ANY, (trace_cell_t) {parent_entry, tp-parent_entry});
    nc->expr.arg[in - pos] = v;
    LOG("arg[%d] -> %d", in - pos, tp - parent_entry);
    pos++;
  }
  clean_tmp(vl);
  nc->expr.arg[in] = entry;
  return nc;
}

// TODO generalize these

// [[...]] -> [...]
static
cell_t *unwrap(cell_t *c, csize_t out) {

  // head
  assert_error(is_list(c) && list_size(c) == 1, TODO " %C", c);
  c = CUT(c, value.ptr[0]);

  // N = out, ap0N swapN drop
  cell_t *l = make_list(out);
  LOG("unwrap %d %C", out, l);
  cell_t *ap = func(OP_ap, 1, out + 1);
  COUNTUP(i, out) {
    cell_t **p = &l->value.ptr[i];
    *p = dep(ref(ap));
    arg(ap, *p);
  }
  arg(ap, c);
  drop(ap);
  return l;
}

cell_t *wrap_vars(cell_t *res, csize_t out) {
  csize_t n = trace_cell_ptr(res->value.tc)->size;
  cell_t *l = make_list(out);
  LOG("wrap_vars %d %C", out, l);
  COUNTUP(i, out - 1) {
    cell_t *d = closure_alloc(1);
    store_dep(d, res->value.tc, n - i - 1, T_ANY, 0);
    l->value.ptr[i] = d;
  }
  l->value.ptr[out - 1] = res;
  return l;
}

// expand a user function into a list of outputs
static
cell_t *expand_list(cell_t *entry, cell_t *c) {
  size_t out = closure_out(c);
  cell_t *l = make_list(out + 1);
  int n = out;
  TRAVERSE(c, out) {
    *p = dep(c);
    l->value.ptr[--n] = *p;
  }
  refn(c, out);
  l->value.ptr[out] = exec_expand(c, entry); // deps will be in c ***
  return l;
}

static
response func_exec_wrap(cell_t **cp, type_request_t treq, cell_t *parent_entry) {
  cell_t *c = *cp;
  size_t in = closure_in(c);
  cell_t *entry = c->expr.arg[in];
  wrap_data wrap;
  PRE(c, exec_wrap, " %E 0x%x #wrap", entry, c->expr.flags);
  LOG_UNLESS(entry->entry.out == 1, "out = %d #unify-multiout", entry->entry.out);

  cell_t *new_entry = trace_start_entry(parent_entry, entry->entry.out);
  new_entry->entry.wrap = &wrap;
  new_entry->module_name = parent_entry->module_name;
  new_entry->word_name = string_printf("%s_r%d", parent_entry->word_name, parent_entry->entry.sub_id++);
  LOG("created entry for %E", new_entry);

  wrap.initial = ref(c);
  insert_root(&wrap.initial);
  move_changing_values(new_entry, c);

  // make a list with expanded outputs of c
  cell_t *nc = COPY_REF(c, in);
  mark_barriers(new_entry, nc);
  wrap.expand = nc;

  cell_t *l = expand_list(new_entry, nc);

  // eliminate intermediate list
  if(treq.t == T_LIST) {
    l = unwrap(l, treq.out);
    new_entry->entry.out = treq.out; // ***
  }

  // IDEA break here?

  // TODO delay this to avoid quote creation
  TRAVERSE_REF(nc, in);
  insert_root(&nc);
  new_entry->entry.alts = trace_reduce(new_entry, &l);
  drop(l);
  remove_root(&nc);

  // IDEA split this up? {
  cell_t *p = flat_call(nc, new_entry);
  drop(nc);
  drop(c);

  trace_final_pass(new_entry);
  trace_end_entry(new_entry);
  remove_root(&wrap.initial);
  drop(wrap.initial);

  trace_clear_alt(parent_entry);
  type_t rtypes[new_entry->entry.out];
  resolve_types(new_entry, rtypes);
  cell_t *res = var_create_with_entry(rtypes[0].exclusive, parent_entry, p->size);
  // }

  // build list expected by caller
  if(treq.t == T_LIST) {
    res = wrap_vars(res, treq.out);
    csize_t n = closure_args(p);
    COUNTUP(i, treq.out - 1) {
      p->expr.arg[n - i - 1] = res->value.ptr[i];
    }
    trace_reduction(p, res->value.ptr[treq.out - 1]);
  }
  *cp = p;
  store_reduced(cp, res);
  return SUCCESS;
}

static
response exec_list(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  PRE_NO_CONTEXT(c, exec_list);

  if(treq.t != T_LIST || closure_out(c) != 0) {
    return SUCCESS;
  }

  CONTEXT_LOG("exec_list %C", c);
  cell_t *res;
  csize_t
    in = closure_in(c),
    out = treq.out - 1,
    n = in + out;

  // if treq.t == T_LIST, need to wrap here
  // move to func_exec; expand, wrap, and return the original function
  c = expand(c, out);
  FLAG_SET(c->expr, EXPR_NO_UNIFY);
  c->expr.out = out;
  res = make_list(treq.out);
  res->value.ptr[out] = c;
  COUNTUP(i, out) {
    cell_t *d = dep(ref(c));
    int j = n - i;
    res->value.ptr[i] = d;
    c->expr.arg[j] = d;
  }
  *cp = res;
  return RETRY;
}

// linear scan for variables to find the type ***
static
int input_type(cell_t *entry, int pos) {
  if(INRANGE(pos, 0, entry->entry.in)) {
    FOR_TRACE(c, entry) {
      if(is_var(c) && c->pos == pos) {
        return trace_type(c).exclusive;
      }
    }
  } else {
    LOG("input_type pos out of bounds: %E %d", entry, pos);
  }
  return T_ANY;
}

static
response func_exec_trace(cell_t **cp, type_request_t treq, cell_t *parent_entry) {
  cell_t *c = *cp;
  response rsp;
  size_t in = closure_in(c);
  cell_t *entry = c->expr.arg[in];
  PRE(c, exec_trace, " %E 0x%x", entry, c->expr.flags);

  assert_error(parent_entry);

  cell_t *res;
  const size_t entry_out = entry->entry.out;
  type_t rtypes[entry_out];
  assert_error(in, "recursive functions must have at least one input");
  assert_error(closure_out(c) + 1 == entry_out);

  alt_set_t alt_set = 0;

  // reduce all inputs
  reassign_input_order(entry);
  COUNTUP(i, in) {
    treq.delay_assert = true;
    uint8_t t = input_type(entry, in - i);
    if(t == T_FUNCTION) t = T_ANY; // HACK, T_FUNCTION breaks things
    CHECK(reduce(&c->expr.arg[i], REQ(t, t)) == FAIL, FAIL);
  }
  COUNTUP(i, in) {
    treq.delay_assert = false;
    uint8_t t = input_type(entry, in - i);
    if(t == T_FUNCTION) t = T_ANY; // HACK, T_FUNCTION breaks things
    CHECK(AND0(reduce_arg(c, i, &alt_set, REQ(t, t)),
               fail_if(as_conflict(alt_set))));
    cell_t *a = clear_ptr(c->expr.arg[i]);
    LOG_WHEN(a->alt, "split %C[%d] = %C #exec_split", c, i, a);
  }
  clear_flags(c);

  // HACK force lists on tail calls
  if(entry == parent_entry) {
    COUNTUP(i, in) {
      cell_t **ap = &c->expr.arg[i];
      cell_t *left;
      if(is_list(*ap) &&
         closure_is_ready(left = *leftmost(ap))) {
        LOG(HACK " forced cells[%C].expr.arg[%d]", c, i);
        // hacky, switches function var in placeholders
        if(is_placeholder(left)) {
          cell_t *f = left->expr.arg[closure_in(left) - 1];
          if(is_var(f)) {
            switch_entry(entry, f);
          }
        }
        func_list(ap, REQ(return));

        // ensure quotes are stored first
        cell_t *l = *ap;
        *ap = trace_quote_var(l);
        drop(l);
      }
    }
  }

  resolve_types(entry, rtypes);
  {
    uint8_t t = rtypes[0].exclusive;
    if(t == T_ANY) {
      t = treq.t;
    }
    if(t == T_FUNCTION) t = T_LIST;
    res = var(t, c, parent_entry->pos);
  }

  // replace outputs with variables
  cell_t **c_out = &c->expr.arg[in + 1];
  COUNTUP(i, entry_out-1) {
    cell_t *d = c_out[i];
    if(d && is_dep(d)) {
      assert_error(d->expr.arg[0] == c);
      drop(c);
      uint8_t t = rtypes[i+1].exclusive;
      if(t == T_FUNCTION) t = T_LIST;
      store_dep(d, res->value.tc, i + in + 1, t, alt_set);
    }
  }

  res->value.alt_set = alt_set;
  res->alt = c->alt;

  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

bool all_dynamic(cell_t *entry) {
  COUNTUP(i, entry->entry.in) {
    if(NOT_FLAG(entry[i+1].value.type, T_CHANGES)) {
      return false;
    }
  }
  return true;
}

OP(exec) {
  cell_t *c = *cp;
  response rsp;
  cell_t *entry = c->expr.arg[closure_in(c)];
  PRE(c, exec, " %E", entry);

  cell_t *parent_entry = find_input_entry(c);

  if(NOT_FLAG(entry->entry, ENTRY_COMPLETE)) {
    if(NOT_FLAG(c->expr, EXPR_DELAYED)) {
      FLAG_SET(c->expr, EXPR_DELAYED);
      LOG("delay exec %E %C", entry, c);
      return DELAY;
    }
    assert_error(parent_entry,
                 "incomplete entry can't be unified without "
                 "a parent entry %C @exec_split", c);
    if(entry->entry.wrap && !unify_exec(cp, parent_entry)) {
      LOG(MARK("WARN") " unify failed: %C %C",
          *cp, entry->entry.wrap->initial);
      ABORT(FAIL);
    }
    return AND0(exec_list(cp, treq),
                func_exec_trace(cp, treq, parent_entry));
  } else if(parent_entry && all_dynamic(entry)) {
    return func_exec_trace(cp, treq, parent_entry);
  } else if(parent_entry && entry->entry.rec) {
    return func_exec_wrap(cp, treq, parent_entry);
  } else {
    assert_counter(1000);

    if(FLAG(c->expr, EXPR_RECURSIVE)) {
      TRAVERSE(c, in) {
        CHECK(reduce(p, REQ(any)));
      }
    }
    cell_t *res = exec_expand(c, entry);

    if(FLAG(entry->entry, ENTRY_TRACE)) {
      // HACK forces inputs
      printf("TRACE: %s.%s", entry->module_name, entry->word_name);
      TRAVERSE(c, in) {
        show_one(*p);
      }
      printf("\n");
    }
    store_lazy(cp, res, 0);
    return RETRY;
  }

 abort:
  return abort_op(rsp, cp, treq);
}

void reduce_quote(cell_t **cp) {
  if(is_user_func(*cp) && closure_is_ready(*cp)) { // HACKy
    LOG("HACK reduce_quote[%C]", *cp);
    insert_root(cp);
    reduce(cp, REQ(any));
    remove_root(cp);
  }
}
