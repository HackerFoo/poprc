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
#include <assert.h>

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/byte_compile.h"
#include "gen/list.h"
#include "gen/user_func.h"

static
cell_t *map_cell(cell_t **map, intptr_t x) {
  return
    x == NIL_INDEX ? &nil_cell :
    x < 0 ? NULL :
    map[x];
}

static
cell_t *get_return_arg(cell_t **map, cell_t *returns, intptr_t x) {
  trace_index_t i = trace_decode(returns->value.ptr[x]);
  return
    i == NIL_INDEX ? empty_list() :
    i < 0 ? NULL :
    map[i];
}

bool func_exec(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  size_t in = closure_in(c) - 1;
  cell_t *entry = c->expr.arg[in];
  cell_t *code = entry + 1;
  size_t len = entry->entry.len;
  cell_t *map[len];
  cell_t *res;
  cell_t *returns = NULL;

  // don't execute, just reduce all args and return variables
  if(trace_enabled &&
     (c->expr.rec || // the function has already been expanded once
      len == 0 || // the function is being compiled
      (entry->entry.rec &&
       (initial_word ||
        entry->entry.rec > trace_cur[-1].entry.in)))) { // not the outermost function
    csize_t c_in = closure_in(c) - 1, n = closure_args(c);
    alt_set_t alt_set = 0;
    unsigned int nonvar = 0;
    bool specialize = false;
    COUNTUP(i, c_in) {
      uint8_t t = len > 0 ? code[c_in - 1 - i].value.type.exclusive : T_ANY;
      if(t == T_FUNCTION) t = T_ANY; // HACK, T_FUNCTION breaks things
      if(!reduce_arg(c, i, &alt_set, req_simple(t)) ||
         as_conflict(alt_set)) goto fail;
      // if all vars in a recursive function, don't expand
      // TODO make this less dumb
      cell_t *a = clear_ptr(c->expr.arg[i]);
      if(!is_var(a)) nonvar++;
      if(is_list(a)) { // TODO this should probably be recursive, or based on unification
        specialize = true;
        if(closure_is_ready(*leftmost(&a))) {
          // adapted from reduce_arg
          cell_t **ap = &c->expr.arg[i];
          bool marked = is_marked(*ap);
          *ap = clear_ptr(*ap);
          bool r = func_list(ap, req_simple(T_RETURN)); // HACK to reduce lists
          alt_set |= (*ap)->value.alt_set;
          if(marked) *ap = mark_ptr(*ap);
          split_arg(c, i);
          if(!r) goto fail;
        }
      }
    }
    clear_flags(c);

    if(nonvar > 0 &&
       len > 0 &&
       !c->expr.rec &&
       (!entry->entry.rec || entry->entry.rec <= trace_cur[-1].entry.in))
    {
      goto expand;
    }

    cell_t *res;
    bool disable_trace = false;
    uint8_t t = treq.t == T_ANY ? T_BOTTOM : treq.t;
    if(specialize && !dont_specialize) {
      res = trace_var_specialized(t, c);
    } else if(trace_match_self(c)) {
      res = trace_var_self(t);
      disable_trace = true;
    } else {
      res = var(t, c);
    }
    res->value.alt_set = alt_set;
    res->alt = c->alt;

    for(csize_t i = c_in + 1; i < n; ++i) {
      cell_t *d = c->expr.arg[i];
      if(d && is_dep(d)) {
        assert(d->expr.arg[0] == c);
        drop(c);
        d->expr.arg[0] = res;
        store_var(d, T_ANY);
      }
    }

    store_reduced(cp, res);
    trace_enabled = !disable_trace;
    return true;

  fail:
    fail(cp, treq);
    return false;
  }

expand:
  assert(len);

  c->expr.arg[in] = 0;
  memset(map, 0, sizeof(map[0]) * len);

  COUNTUP(i, in) {
    cell_t *p = &code[i];
    assert(is_var(p));
    map[i] = refn(c->expr.arg[in - 1 - i], p->n);
  }

  // allocate, copy, and index
  size_t s = 0;
  for(size_t i = in; i < len; i += s) {
    cell_t *p = &code[i];
    s = calculate_cells(p->size);
    if(!p->func) {
      map[i] = 0;
      continue; // skip empty cells TODO remove these
    }
    if(trace_type(p).exclusive == T_RETURN) {
      if(!returns) returns = p;
      continue;
    }
    cell_t *nc = closure_alloc_cells(s);
    memcpy(nc, p, s * sizeof(cell_t));
    nc->tmp = 0;
    map[i] = nc;
  }

  // rewrite pointers
  for(size_t i = in; i < len; i++) {
    cell_t *t = map_cell(map, i);
    if(!t) continue;

    // skip rewriting for the entry argument
    cell_t **t_entry = NULL;
    if(t->func == func_exec || t->func == func_quote) {
      t_entry = &t->expr.arg[closure_in(t) - 1];
      *t_entry = &trace_cells[trace_decode(*t_entry)];
    }

    traverse(t, {
        if(p != t_entry) {
          trace_index_t x = trace_decode(*p);
          *p = map_cell(map, x);
        }
      }, ARGS | PTRS | ALT);

    if(trace_enabled &&
       t_entry &&
       *t_entry == entry &&
       !initial_word) { // mark recursion
      t->expr.rec = 1;
      initial_word = copy(c);
      initial_word->expr.arg[in] = entry;
      traverse_ref(initial_word, ARGS_IN | ALT);
    }
  }

  // handle returns
  uint8_t alt_n = int_log2(entry->entry.alts);
  uint8_t alt_id = new_alt_id(alt_n);
  unsigned int branch = 0;
  size_t
    out = closure_out(c),
    n = closure_args(c);
  cell_t **results[out + 1];
  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i];
  }

  // first one
  alt_set_t alt_set = as_multi(alt_id, alt_n, branch++);
  res = id(get_return_arg(map, returns, out), alt_set);
  COUNTUP(i, out) {
    cell_t *d = c->expr.arg[n - 1 - i];
    store_lazy_dep(d, get_return_arg(map, returns, i), alt_set);
  }

  // rest
  trace_index_t next = trace_decode(returns->alt);
  while(next >= 0) {
    alt_set_t as = as_multi(alt_id, alt_n, branch++);
    returns = &code[next];
    FOREACH(i, results) {
      cell_t *a = get_return_arg(map, returns, i);
      results[i] = &(*results[i])->alt;
      *results[i] = a ? id(a, as) : NULL;
    }
    next = trace_decode(returns->alt);
  }

  store_lazy(cp, c, res, 0);
  return false;
}

// takes free variables and returns a quoted function
bool func_quote(cell_t **cp, UNUSED type_request_t treq) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  csize_t in = closure_in(c) - 1;
  cell_t *entry = c->expr.arg[in];
  c->expr.arg[in] = 0;
  csize_t
    f_in = entry->entry.in,
    f_out = entry->entry.out;

  cell_t *f = closure_alloc(f_in + f_out);
  csize_t offset = f_in - in;
  if(offset) {
    f->expr.arg[0] = (cell_t *)(trace_index_t)(offset - 1);
    f->func = (reduce_t *)mark_ptr(func_exec);
  } else {
    f->func = func_exec;
  }

  COUNTUP(i, in) {
    f->expr.arg[i + offset] = ref(c->expr.arg[i]);
  }

  f->expr.arg[f_in] = entry;

  cell_t *res = make_list(f_out);
  cell_t **out_arg = &f->expr.arg[f_in+1];
  COUNTUP(i, f_out-1) {
    cell_t *d = dep(f);
    out_arg[f_out - 2 - i] = d;
    res->value.ptr[i] = d;
  }
  f->expr.out = f_out - 1;
  res->value.ptr[f_out-1] = f;
  refn(f, f_out-1);
  res->alt = c->alt;
  if(entry->entry.flags & ENTRY_ROW) res->value.type.flags |= T_ROW;

  store_reduced(cp, res);
  return true;
}

void reduce_quote(cell_t **cp) {
  if((*cp)->func == func_quote || (*cp)->func == func_exec) { // HACKy
    reduce(cp, req_any);
  }
}
