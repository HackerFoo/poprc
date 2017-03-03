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

#include <assert.h>
#include "rt_types.h"
#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/list.h"

cell_t *empty_list() {
  cell_t *c = closure_alloc(1);
  c->func = func_value;
  c->value.type.exclusive = T_LIST;
  return c;
}

cell_t *make_list(csize_t n) {
  if(n == 0) return &nil_cell;
  cell_t *c = closure_alloc(n + 1);
  c->func = func_value;
  c->value.type.exclusive = T_LIST;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(2);
  c->func = func_value;
  c->value.type.exclusive = T_LIST;
  c->value.ptr[0] = x;
  return c;
}

bool is_list(cell_t const *c) {
  return c && is_value(c) && c->value.type.exclusive == T_LIST;
}

bool is_row_list(cell_t const *c) {
  return is_list(c) && (c->value.type.flags & T_ROW);
}

bool is_function(cell_t const *c) {
  return c && is_value(c) && c->value.type.exclusive == T_FUNCTION;
}


// not really a func
bool func_list(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  if(treq.t == T_ANY && treq.t == T_LIST) return true;
  if(treq.t != T_RETURN) goto fail;
  csize_t n = list_size(c);

  alt_set_t alt_set = c->value.alt_set;
  COUNTDOWN(i, n) {
    if(!reduce_ptr(c, i, &alt_set, req_any) ||
      as_conflict(alt_set)) goto fail;
  }
  traverse(c, *p = clear_ptr(*p), PTRS);
  return true;

 fail:
  fail(cp, treq);
  return false;
}

void reduce_list(cell_t **cp) {
  if(!*cp) return;
  while(*cp) {
    if(func_list(cp, req_simple(T_RETURN))) {
      cp = &(*cp)->alt;
    }
  }
}

list_iterator_t list_begin(cell_t *l) {
  assert(is_list(l));
  bool row = is_row_list(l);
  list_iterator_t it = {
    .array = l->value.ptr,
    .index = 0,
    .size = list_size(l) - row,
    .row = row
  };
  return it;
}

bool list_has_more(list_iterator_t *it) {
  return it->array && (it->row || it->index < it->size);
}

cell_t **list_next(list_iterator_t *it) {
  if(!it->array) return NULL;
  if(it->index < it->size) {
    return &it->array[it->index++];
  } else if(it->row) {
    cell_t **rp = &it->array[it->size];
    reduce(rp, req_simple(T_LIST)); // HACK fix this
    *it = list_begin(*rp);
    return &it->array[++it->index];
  } else {
    it->array = NULL;
    return NULL;
  }
}

// number of remaining elements
// NOTE: will reduce all rows
csize_t list_remaining_size(list_iterator_t it) {
  if(!it.array) return 0;
  csize_t n = 0;
  while(it.row) {
    cell_t **rp = &it.array[it.size];
    reduce(rp, req_simple(T_LIST)); // HACK fix this
    it = list_begin(it.array[it.size]);
    n += it.size;
  }
  return n + it.size;
}

// finds the cell which contains a pointer
cell_t *ptr_to_cell(void *p) {
  return &cells[(cell_t *)p - cells];
}

// returns a copy of the rest of the list
cell_t *list_rest(list_iterator_t it) {
  if(!it.array) return NULL;
  if(!it.index) return ref(ptr_to_cell(it.array));
  csize_t elems = it.size - it.index + it.row;
  cell_t *rest;
  if(elems == 1 && it.row) {
    rest = ref(it.array[it.size]);
  } else {
    rest = make_list(elems);
    COUNTUP(i, elems) {
      rest->value.ptr[i] = ref(it.array[i + it.index]);
    }
    if(it.row) rest->value.type.flags |= T_ROW;
  }
  return rest;
}

cell_t *flat_copy(cell_t *l) {
  assert(is_list(l));
  if(!(l->value.type.flags & T_ROW)) return ref(l);
  csize_t n = flat_list_size(l);
  if(!n) return &nil_cell;
  cell_t *res = make_list(n);
  res->value.type = l->value.type;
  res->value.type.flags &= ~T_ROW;
  cell_t **p, **rp = res->value.ptr;
  FORLIST(p, l) {
    *rp++ = ref(*p);
  }
  return res;
}

bool is_empty_list(const cell_t *l) {
  return is_list(l) && list_size(l) == 0;
}
