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

#include "gen/error.h"
#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/byte_compile.h"
#include "gen/trace.h"
#include "gen/list.h"
#include "gen/user_func.h"
#include "gen/print.h"
#include "gen/log.h"
#include "gen/parse.h" // for string_printf

bool is_user_func(const cell_t *c) {
  return c->func == func_exec;
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
  return // can't use map_cell, returns empty_list() instead of &nil_cell
    i == NIL_INDEX ? empty_list() :
    i <= 0 ? NULL :
    entry[i].alt;
}

// given a list l with arity in -> out, produce an application of the list
// [X -> Y] => [X -> Y] apXY
cell_t *apply_list(cell_t *l, csize_t in, csize_t out) {
  cell_t *c = func(func_ap, in + 1, out + 1);
  if(in) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(in - 1);
  } else {
    FLAG_CLEAR(c->expr, FLAGS_NEEDS_ARG);
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
  CONTEXT("bind_pattern %d %d", CELL_INDEX(c), CELL_INDEX(pattern));
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
  } else {
    assert_error(false, "binding error");
    return NULL;
  }
  return tail;
}

// unify c with pattern pat if possible, returning the unified result
cell_t *unify_convert(cell_t *entry, cell_t *c, cell_t *pat) {
  if(!pat) return NULL;
  assert_error(c->size == pat->size);
  assert_error(c->expr.out == pat->expr.out);
  csize_t out = entry->entry.out;
  cell_t *ret = NULL;
  if(out != 1) { // for now
    LOG("unify_convert %d: out(%d) != 1", c-cells, out);
    return NULL;
  }

  cell_t *vl = 0;
  cell_t **tail = &vl;
  COUNTUP(i, closure_in(c)) {
    tail = bind_pattern(c->expr.arg[i], pat->expr.arg[i], tail);
    if(!tail) {
      LOG("bind_pattern failed %d %d",
          CELL_INDEX(c->expr.arg[i]),
          CELL_INDEX(pat->expr.arg[i]));
      break;
    }
  }
  if(tail) {
    csize_t in = tmp_list_length(vl);
    cell_t *n = closure_alloc(in + out);
    n->func = func_exec;
    n->expr.arg[in] = entry;
    FLAG_SET(n->expr, FLAGS_RECURSIVE);
    int pos = 0;
    FOLLOW(p, vl, tmp) { // get arguments from the binding list
      n->expr.arg[in - 1 - pos] = p;
      pos++;
    }
    ret = n;
  } else {
    FOLLOW(p, vl, tmp) {
      drop(p);
    }
  }
  clean_tmp(vl);
  return ret;
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
  size_t len = entry->entry.len;
  cell_t *res;
  cell_t *returns = NULL;
  cell_t **results[out + 1];

  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i]; // ***
  }

  CONTEXT("exec_expand %s: %d 0x%x", entry->word_name, CELL_INDEX(c), c->expr.flags);

  assert_error(len && FLAG(entry->entry, ENTRY_COMPLETE));
  trace_clear_alt(entry); // *** probably shouldn't need this

  COUNTUP(i, in) {
    cell_t *p = &entry[i + 1];
    assert_error(is_var(p));
    p->alt = refn(c->expr.arg[in - 1 - i], p->n);
  }

  // allocate, copy, and index
  FOR_TRACE(p, entry, in) {
    int i = p - entry;
    if(!p->func) {
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
      FLAG_SET(t->expr, FLAGS_RECURSIVE);
      //LOG("recursive exec %d -> %d", CELL_INDEX(c), trace_ptr-trace_cur);
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
      results[i] = &(*results[i])->alt;
      *results[i] = a ? id(a, as) : NULL;
    }
    next = trace_decode(returns->alt);
  }

  return res;
}

bool is_within_entry(cell_t *entry, cell_t *p) {
  return p > entry && p <= entry + entry->entry.len;
}

// this doesn't work for quotes
// all c's input args must be params
cell_t *flat_call(cell_t *c, cell_t *entry) {
  cell_t *parent_entry = entry->entry.parent;
  CONTEXT("flat call %d (%d -> %d)",
          CELL_INDEX(c),
          entry_number(parent_entry),
          entry_number(entry));
  csize_t
    in = entry->entry.in,
    out = entry->entry.out;
  cell_t *nc = closure_alloc(in + out);
  nc->expr.out = out - 1;
  nc->func = func_exec;
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  assert_log(tmp_list_length(vl) == in, "%d != %d", tmp_list_length(vl), in);

  int pos = 1;
  FOLLOW(p, vl, tmp) {
    assert_error(p->value.tc.entry == entry);
    cell_t *tn = trace_cell_ptr(p->value.tc);
    assert_error(tn->pos);
    tn->pos = pos;
    switch_entry(parent_entry, tn);
    assert_error(tn->value.tc.entry == parent_entry);
    cell_t *tp = trace_cell_ptr(tn->value.tc);
    cell_t *v = var_create_nonlist(T_ANY, (trace_cell_t) {parent_entry, tp-parent_entry});
    nc->expr.arg[in - pos] = v;
    LOG("arg[%d] -> %d", in - pos, tp - parent_entry);
    pos++;
  }
  nc->expr.arg[in] = entry;
  return nc;
}

// TODO generalize these
static
cell_t *unwrap(cell_t *c, type_request_t treq) {
  if(treq.t != T_LIST) {
    return quote(c);
  }

  cell_t *l = make_list(treq.out + 1);
  cell_t *ap = func(func_ap, 1, treq.out + 1);
  COUNTUP(i, treq.out) {
    cell_t **p = &l->value.ptr[i];
    *p = dep(ref(ap));
    arg(ap, *p);
  }
  arg(ap, c);
  l->value.ptr[treq.out] = ap;
  FLAG_SET(l->value.type, T_ROW);
  return l;
}

cell_t *wrap_vars(cell_t *res, csize_t out) {
  csize_t n = trace_cell_ptr(res->value.tc)->size;
  cell_t *l = make_list(out);
  COUNTUP(i, out - 1) {
    cell_t *d = closure_alloc(1);
    store_dep(d, res->value.tc, n - i - 1, T_ANY);
    l->value.ptr[i] = d;
  }
  l->value.ptr[out - 1] = res;
  return l;
}

static
bool func_exec_wrap(cell_t **cp, type_request_t treq, cell_t *parent_entry) {
  cell_t *c = *cp;
  assert_error(!is_marked(c));

  size_t in = closure_in(c);
  cell_t *entry = c->expr.arg[in];
  assert_error(entry->entry.out == 1, "TODO");
  CONTEXT("exec_wrap %s: %d 0x%x", entry->word_name, CELL_INDEX(c), c->expr.flags);

  cell_t *new_entry = trace_start_entry(parent_entry, 1);
  new_entry->module_name = entry->module_name;
  new_entry->word_name = string_printf("%s_r%d", parent_entry->word_name, parent_entry->entry.sub_id++);
  LOG("created entry %d", entry_number(new_entry));

  new_entry->initial = ref(c);

  cell_t *nc = COPY_REF(c, in);
  mark_barriers(new_entry, nc);
  cell_t *p = exec_expand(nc, new_entry); // deps will be in nc ***
  p = unwrap(p, treq);
  if(treq.t == T_LIST) {
    new_entry->entry.out = treq.out; // ***
  }

  // TODO delay this to avoid quote creation
  TRAVERSE_REF(nc, in);
  insert_root(&nc);
  new_entry->entry.alts = trace_reduce(new_entry, &p);
  drop(p);
  remove_root(&nc);

  p = flat_call(nc, new_entry);
  drop(nc);
  drop(c);

  trace_final_pass(new_entry);
  trace_end_entry(new_entry);

  trace_clear_alt(parent_entry);
  cell_t *res = var_create_with_entry(T_ANY, parent_entry, p->size);
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
  return true;
}

static
bool unify_exec(cell_t **cp, cell_t *parent_entry) {
  cell_t *c = *cp;
  assert_error(!is_marked(c));

  size_t in = closure_in(c);
  cell_t *entry = c->expr.arg[in];
  cell_t *initial = entry->initial;
  cell_t *n = unify_convert(parent_entry, c, initial);
  if(n) {
    LOG("unified %s.%s %d with initial_word in %s.%s %d",
        entry->module_name,
        entry->word_name,
        CELL_INDEX(c),
        parent_entry->module_name,
        parent_entry->word_name,
        CELL_INDEX(initial));
    drop(c);
    *cp = n;
  }
  return n != NULL;
}

static
bool func_exec_trace(cell_t **cp, type_request_t treq, cell_t *parent_entry) {
  cell_t *c = *cp;
  assert_error(!is_marked(c));

  size_t in = closure_in(c);
  assert_error(in, "recursive functions must have at least one input");
  cell_t *entry = c->expr.arg[in];
  size_t len = entry->entry.len;
  cell_t *res;
  type_t rtypes[entry->entry.out];
  CONTEXT("exec_trace %s: %d 0x%x", entry->word_name, CELL_INDEX(c), c->expr.flags);

  alt_set_t alt_set = 0;
  unsigned int nonvar = 0;

  // reduce all inputs
  COUNTUP(i, in) {
    uint8_t t = len > 0 ? entry[in - i].value.type.exclusive : T_ANY; // ***
    if(t == T_FUNCTION) t = T_ANY; // HACK, T_FUNCTION breaks things
    if(!reduce_arg(c, i, &alt_set, req_simple(t)) ||
       as_conflict(alt_set)) goto fail;
    // if all vars in a recursive function, don't expand
    // TODO make this less dumb
    cell_t *a = clear_ptr(c->expr.arg[i]);
    if(!is_var(a)) nonvar++;
  }
  clear_flags(c);

  // HACK force lists on tail calls
  if(entry == parent_entry) {
    COUNTUP(i, in) {
      if(is_list(c->expr.arg[i]) &&
         closure_is_ready(*leftmost(&c->expr.arg[i]))) {
        LOG("HACK forced cells[%d].expr.arg[%d]", CELL_INDEX(c), i);
        func_list(&c->expr.arg[i], REQ(return));

        // ensure quotes are stored first
        cell_t *l = c->expr.arg[i];
        c->expr.arg[i] = trace_quote_var(l);
        drop(l);
      }
    }
  }

  /*
  // is this still necessary?
  if(nonvar > 0 &&
     len > 0 &&
     NOT_FLAG(c->expr, FLAGS_RECURSIVE) &&
     entry->entry.rec <= parent_entry->entry.in)
  {
    // okay to expand
    cell_t *res = exec_expand(c, entry);
    store_lazy(cp, c, res, 0);
    return false;
  }
  */

  if(treq.t == T_BOTTOM) {
    COUNTUP(i, entry->entry.out) {
      rtypes[i].exclusive = T_BOTTOM;
    }
  } else if(NOT_FLAG(entry->entry, ENTRY_COMPLETE)) {
    // this will be fixed up for tail calls in tail_call_to_bottom()
    COUNTUP(i, entry->entry.out) {
      rtypes[i].exclusive = T_ANY;
    }
  } else {
    resolve_types(entry, rtypes);
  }
  {
    uint8_t t = rtypes[0].exclusive;
    if(t == T_ANY) {
      t = FLAG(c->expr, FLAGS_RECURSIVE) ? T_BOTTOM : treq.t;
    }
    if(t == T_FUNCTION) t = T_LIST;
    res = var(t, c, parent_entry->pos);
  }
  res->value.alt_set = alt_set;
  res->alt = c->alt;

  // replace outputs with variables
  RANGEUP(i, in + 1, in + entry->entry.out) {
    cell_t *d = c->expr.arg[i];
    if(d && is_dep(d)) {
      assert_error(d->expr.arg[0] == c);
      drop(c);
      uint8_t t = rtypes[i].exclusive;
      if(t == T_FUNCTION) t = T_LIST;
      store_dep(d, res->value.tc, i, t);
    }
  }

  store_reduced(cp, res);
  return true;

fail:
  fail(cp, treq);
  return false;
}

bool func_exec(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  assert_error(!is_marked(c));

  cell_t *entry = c->expr.arg[closure_in(c)];
  cell_t *parent_entry = find_input_entry(c);

  if(NOT_FLAG(entry->entry, ENTRY_COMPLETE)) {
    if(entry->initial) unify_exec(cp, parent_entry);
    return func_exec_trace(cp, treq, parent_entry);
  } else if(parent_entry && entry->entry.rec) {
    return func_exec_wrap(cp, treq, parent_entry);
  } else {
    assert_counter(1000);
    cell_t *res = exec_expand(c, entry);
    store_lazy(cp, c, res, 0);
    return false;
  }
}

void reduce_quote(cell_t **cp) {
  if(is_user_func(*cp) && closure_is_ready(*cp)) { // HACKy
    LOG("HACK reduce_quote[%d]", CELL_INDEX(*cp));
    insert_root(cp);
    reduce(cp, req_any);
    remove_root(cp);
  }
}
