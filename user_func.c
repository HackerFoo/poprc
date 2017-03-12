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
  if(entry->entry.flags & ENTRY_NOINLINE || c->expr.rec) {
    csize_t c_in = closure_in(c), n = closure_args(c);
    alt_set_t alt_set = 0;
    bool expnd = true;
    for(csize_t i = 0; i < c_in - 1; ++i) {
      if(!reduce_arg(c, i, &alt_set, req_any)) goto fail;
      // if any vars in a recursive function, don't expand
      // TODO make this less dumb
      if(is_var(clear_ptr(c->expr.arg[i]))) expnd = false;
    }
    if(expnd && c_in > 1) goto expand;
    for(csize_t i = c_in; i < n; ++i) {
      cell_t **d = &c->expr.arg[i];
      if(*d && is_dep(*d)) {
        cell_t *v = var(T_BOTTOM, *d);
        v->value.alt_set = alt_set;
        store_reduced(d, v);
      }
    }

    cell_t *res = var(treq.t == T_ANY ? T_BOTTOM : treq.t, c);
    res->value.alt_set = alt_set;
    store_reduced(cp, res);
    return true;

  fail:
    fail(cp, treq);
    return false;
  }

expand:

  c->expr.arg[in] = 0;
  memset(map, 0, sizeof(map[0]) * len);
  csize_t function_args = 0;

  COUNTUP(i, in) {
    cell_t *p = &code[i];
    assert(is_var(p));
    if(is_function(p)) function_args++;
    map[i] = refn(c->expr.arg[in - 1 - i], p->n);
  }

  // allocate, copy, and index
  size_t s = 0;
  cell_t *encoded_entry = trace_encode(entry - trace_cells);
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
    cell_t *nc;
    if(trace_enabled &&
       p->func == func_exec &&
       p->expr.arg[closure_in(p) - 1] == encoded_entry) {
      csize_t out = closure_out(c);
      nc = closure_alloc(p->size - function_args);
      nc->expr.out = out;
      nc->expr.rec = 1;
      nc->func = func_exec;
      csize_t j = 0;
      COUNTUP(i, in) {
        if(!is_function(&code[in - 1 - i])) {
          nc->expr.arg[j++] = p->expr.arg[i];
        } else {
          drop(map_cell(map, trace_decode(p->expr.arg[i])));
        }
      }
      nc->expr.arg[j] = trace_encode(trace_cur - trace_cells - 1);
      if(out) memcpy(&nc->expr.arg[j+1], &p->expr.arg[in+1], out * sizeof(cell_t));
    } else {
      nc = closure_alloc_cells(s);
      memcpy(nc, p, s * sizeof(cell_t));
    }
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
      if(*t_entry == entry) { // track recursion depth
        t->expr.rec = c->expr.rec + 1;
      }
    }

    traverse(t, {
        if(p != t_entry) {
          trace_index_t x = trace_decode(*p);
          *p = map_cell(map, x);
        }
      }, ARGS | PTRS | ALT);
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

bool func_exec_recursive(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  csize_t c_in = closure_in(c);
  alt_set_t alt_set = 0;
  for(csize_t i = 0; i < c_in - 1; ++i) {
    if(!reduce_arg(c, i, &alt_set, req_any)) goto fail;
  }

  c->func = func_exec;
  return func_exec(cp, treq);

fail:
  fail(cp, treq);
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
