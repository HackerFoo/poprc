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
#include "gen/user_func.h"
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
    if(i == 0 && is_row_list(c) && is_list(c->value.ptr[0])) {
      if(!func_list(&c->value.ptr[0], req_any)) goto fail;
    }
  }
  traverse(c, *p = clear_ptr(*p), PTRS);
  return true;

 fail:
  fail(cp, treq);
  return false;
}

void flatten_list(cell_t **lp) {
  cell_t *l = *lp;
  if(is_list(l)) {
    *lp = flat_copy(l);
    drop(l);
  }
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

bool list_has_more(list_iterator_t it) {
  return it.array && (it.row || it.index < it.size);
}

cell_t **list_next(list_iterator_t *it, bool include_row) {
  if(!it->array) return NULL;
start:
  if(it->index < it->size) {
    return &it->array[it->index++];
  } else if(it->row && it->index == it->size) {
    cell_t **rp = &it->array[it->size];
    reduce_quote(rp); // ***
    if(is_list(*rp)) { // ***
      *it = list_begin(*rp);
      goto start;
    } else {
      if(include_row) {
        it->index++;
        return &it->array[it->size];
      } else {
        return NULL;
      }
    }
  } else {
    return NULL;
  }
}

// get last row variable
cell_t *list_row(list_iterator_t it) {
  return it.array && it.row && it.index == it.size ? it.array[it.size] : NULL;
}

// number of remaining elements
// NOTE: will reduce all quotes
csize_t list_remaining_size(list_iterator_t it, bool count_last_row) {
  if(!it.array) return 0;
  csize_t n = 0;
  while(it.row) {
    cell_t **rp = &it.array[it.size];
    reduce_quote(rp); // ***
    if(!is_list(*rp)) {
      n += !!count_last_row;
      break;
    }
    it = list_begin(it.array[it.size]);
    n += it.size;
  }
  return n + it.size - min(it.size, it.index);
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
  if(elems == 1 && it.row && is_list(it.array[it.size])) {
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
  csize_t n = function_out(l);
  if(!n) return &nil_cell;
  cell_t *res = make_list(n);
  res->value.type = l->value.type;
  res->value.type.flags &= ~T_ROW;
  cell_t **p, **rp = res->value.ptr;
  list_iterator_t it = list_begin(l);
  WHILELIST(p, it) {
    *rp++ = *p;
  }
  // get row variable
  if(it.array && it.row) {
    *rp++ = it.array[it.size];
    res->value.type.flags |= T_ROW;
  }
  return res;
}

bool is_empty_list(const cell_t *l) {
  return is_list(l) && list_size(l) == 0;
}

csize_t function_out(const cell_t *l) {
  list_iterator_t it = list_begin((cell_t *)l);
  return list_remaining_size(it, true);
}

// *** TODO compose if needed
cell_t **left_list(cell_t **l) {
  while(is_row_list(*l)) {
    cell_t **x = &(*l)->value.ptr[list_size(*l) - 1];
    if(!is_list(*x)) break;
    l = x;
  }
  return l;
}

cell_t **left_elem(cell_t *l) {
  csize_t n = list_size(l);
  assert(n);
  return &l->value.ptr[n-1];
}

cell_t **leftmost(cell_t **l) {
  return is_empty_list(*l) ? l : left_elem(*left_list(l));
}

csize_t function_in(const cell_t *l) {
  if(is_empty_list(l)) return 0;
  cell_t *c = *leftmost((cell_t **)&l); // ***
  if(!c) return 0;
  if(closure_is_ready(c)) return 0;
  csize_t in = 1;
  while(c) {
    csize_t i = closure_next_child(c);
    in += i;
    c = c->expr.arg[i];
  }
  return in;
}
