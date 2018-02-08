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

#include <stdio.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "user_func.h"
#include "list.h"

cell_t *empty_list() {
  cell_t *c = closure_alloc(1);
  c->op = OP_value;
  c->value.type.exclusive = T_LIST;
  return c;
}

cell_t *make_list(csize_t n) {
  if(n == 0) return &nil_cell;
  cell_t *c = closure_alloc(n + 1);
  c->op = OP_value;
  c->value.type.exclusive = T_LIST;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(2);
  c->op = OP_value;
  c->value.type.exclusive = T_LIST;
  c->value.ptr[0] = x;
  return c;
}

cell_t *row_quote(cell_t *x) {
  cell_t *q = quote(x);
  FLAG_SET(q->value.type, T_ROW);
  return q;
}

bool is_list(cell_t const *c) {
  return c && is_value(c) && c->value.type.exclusive == T_LIST;
}

bool is_row_list(cell_t const *c) {
  return is_list(c) && FLAG(c->value.type, T_ROW);
}

bool is_function(cell_t const *c) {
  return c && is_value(c) && c->value.type.exclusive == T_FUNCTION;
}


// not really a func
bool func_list(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  PRE(c, list);
  if(treq.t == T_ANY && treq.t == T_LIST) return true;
  if(!check_type(treq.t, T_RETURN)) goto fail;
  csize_t n = list_size(c);

  alt_set_t alt_set = c->value.alt_set;
  COUNTUP(i, n) {
    if(!reduce_ptr(c, i, &alt_set, req_pos(REQ(any), treq.pos)) ||
      as_conflict(alt_set)) goto fail;
  }
  TRAVERSE(c, ptrs) {
    *p = clear_ptr(*p);
  }
  if(n && is_row_list(c) && is_list(c->value.ptr[n-1])) {
    if(!func_list(&c->value.ptr[n-1], req_pos(REQ(any), treq.pos))) goto fail;
  }
  alt_set |= c->value.ptr[n-1]->value.alt_set;
  if(as_conflict(alt_set)) goto fail;
  c->value.alt_set = alt_set;
  return true;

 fail:
  fail(cp, treq);
  return false;
}

void reduce_list(cell_t **cp) {
  while(*cp) {
    if(func_list(cp, REQ(return))) {
      cp = &(*cp)->alt;
    }
  }
}

list_iterator_t list_begin(cell_t *l) {
  assert_error(is_list(l));
  bool row = is_row_list(l);
  list_iterator_t it = {
    .array = l->value.ptr,
    .index = 0,
    .size = list_size(l) - row,
    .row = row
  };
  return it;
}

cell_t **list_next(list_iterator_t *it, bool include_row) {
  if(!it->array) return NULL;
start:
  if(it->index < it->size) {
    return &it->array[it->index++];
  } else if(it->row && it->index == it->size) {
    cell_t **rp = &it->array[it->size];
    if(closure_is_ready(*rp)) reduce_quote(rp); // ***
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

static
cell_t *_make_test_list(csize_t n, cell_t *row) {
  cell_t *l = make_list(n + !!row);
  csize_t rn = row ? list_size(row) : 0;
  COUNTUP(i, n) {
    l->value.ptr[i] = int_val(rn + i);
  }
  if(row) {
    l->value.ptr[n] = row;
    FLAG_SET(l->value.type, T_ROW);
  }
  return l;
}

static
cell_t *_test_list_add(csize_t x, csize_t y) {
  return _make_test_list(y, _make_test_list(x, NULL));
}

static
bool _check_list_next(csize_t x, csize_t y) {
  cell_t **p, *l = _test_list_add(x, y);
  csize_t z = 0;
  FORLIST(p, l) {
    z++;
  }
  drop(l);
  printf("_check_list_next: %d + %d = %d\n", x, y, z);
  return x + y == z;
}

TEST(list_next) {
  bool ret = true;
  ret &= _check_list_next(2, 2);
  ret &= _check_list_next(0, 1);
  ret &= _check_list_next(1, 0);
  return ret ? 0 : -1;
}

// number of remaining elements
// NOTE: will reduce all quotes
csize_t list_remaining_size(list_iterator_t it, bool count_last_row) {
  if(!it.array || it.index > it.size) return 0;
  csize_t n = 0;
  while(it.row) {
    cell_t **rp = &it.array[it.size];
    reduce_quote(rp); // ***
    if(is_list(*rp)) {
      n += it.size;
      it = list_begin(it.array[it.size]);
    } else {
      it.size += !!count_last_row;
      break;
    }
  }
  return n + it.size - min(it.size, it.index);
}

static
bool _check_list_remaining_size(csize_t x, csize_t y) {
  cell_t *l = _test_list_add(x, y);
  csize_t z = list_remaining_size(list_begin(l), false);
  drop(l);
  printf("_check_list_remaining_size: %d + %d = %d\n", x, y, z);
  return x + y == z;
}

TEST(list_remaining_size) {
  bool ret = true;
  ret &= _check_list_remaining_size(2, 2);
  ret &= _check_list_remaining_size(0, 1);
  ret &= _check_list_remaining_size(1, 0);
  return ret ? 0 : -1;
}

// finds the cell which contains a pointer
cell_t *ptr_to_cell(void *p) {
  volatile size_t i = ((cell_t *)p) - cells; // prevent optimizing this away
  return (cell_t *)((char *)cells + i * sizeof(cell_t));
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
    if(it.row) FLAG_SET(rest->value.type, T_ROW);
  }
  return rest;
}

void collapse_row(cell_t **cp) {
  if(is_row_list(*cp) &&
     list_size(*cp) == 1 &&
     closure_is_ready((*cp)->value.ptr[0])) {
    cell_t *x = *cp;
    *cp = ref((*cp)->value.ptr[0]);
    drop(x);
  }
}

cell_t *flat_copy(cell_t *l) {
  assert_error(is_list(l));
  csize_t n = function_out(l, true);
  if(!n) return &nil_cell;
  cell_t *res = make_list(n);
  res->value.type = l->value.type;
  FLAG_CLEAR(res->value.type, T_ROW);
  cell_t **p, **rp = res->value.ptr;
  list_iterator_t it = list_begin(l);
  WHILELIST(p, it, true) {
    *rp++ = *p;
  }

  if(it.row) {
    FLAG_SET(res->value.type, T_ROW);
  }
  return res;
}

static
bool _check_flat_copy(csize_t x, csize_t y) {
  cell_t *l = _test_list_add(x, y);
  cell_t *fl = flat_copy(l);
  csize_t z = list_size(fl);
  closure_free(fl);
  drop(l);
  printf("_check_flat_copy: %d + %d = %d\n", x, y, z);
  return x + y == z;
}

TEST(flat_copy_list) {
  bool ret = true;
  ret &= _check_flat_copy(2, 2);
  ret &= _check_flat_copy(0, 1);
  ret &= _check_flat_copy(1, 0);
  return ret ? 0 : -1;
}

csize_t flattened_list_size(cell_t *l) {
  csize_t n = 0;
  cell_t **p;
  FORLIST(p, l) {
    n += is_list(*p) ? flattened_list_size(*p) : 1;
  }
  return n;
}

cell_t **flattened_list_copy_(cell_t *l, cell_t **dst) {
  cell_t **p;
  FORLIST(p, l) {
    if(is_list(*p)) {
      dst = flattened_list_copy_(*p, dst);
    } else {
      *dst++ = *p;
    }
  }
  return dst;
}

cell_t *flattened_list_copy(cell_t *l) {
  csize_t n = flattened_list_size(l);
  cell_t *fl = make_list(n);
  flattened_list_copy_(l, fl->value.ptr);
  return fl;
}

TEST(flattened_list_copy) {
  cell_t *l1 = make_list(1);
  l1->value.ptr[0] = int_val(1);
  cell_t *l2 = make_list(2);
  l2->value.ptr[0] = int_val(3);
  l2->value.ptr[1] = int_val(4);
  cell_t *l0 = make_list(3);
  l0->value.ptr[0] = l1;
  l0->value.ptr[1] = int_val(2);
  l0->value.ptr[2] = l2;
  cell_t *fl = flattened_list_copy(l0);

  cell_t **p;
  int x = 1;
  int status = 0;
  FORLIST(p, fl) {
    if(!is_value(*p) ||
       (*p)->value.type.exclusive != T_INT ||
       (*p)->value.integer[0] != x++) {
      status = -1;
      break;
    }
  }
  closure_free(fl);
  drop(l0);
  return status;
}

bool is_empty_list(const cell_t *l) {
  return is_list(l) && list_size(l) == 0;
}

csize_t function_out(const cell_t *l, bool include_row_var) {
  if(!l) return 0;
  list_iterator_t it = list_begin((cell_t *)l);
  return list_remaining_size(it, include_row_var);
}

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
  return n ? &l->value.ptr[n-1] : NULL;
}

cell_t **leftmost_row(cell_t **lp) {
  return left_elem(*left_list(lp));
}

// TODO optimize this
cell_t **leftmost(cell_t **lp) {
  cell_t *l = *lp, **x, **last = lp;
  FORLIST(x, l, true) {
    last = x;
  }
  return last;
}

csize_t function_in(const cell_t *l) {
  if(!l || is_empty_list(l)) return 0;
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

TEST(function_in) {
  cell_t *l = make_list(2);
  l->value.ptr[0] = int_val(2);
  l->value.ptr[1] = quote(func(OP_exec, 2, 1));
  l->value.type.flags = T_ROW;
  csize_t in = function_in(l);
  drop(l);
  return in == 2 ? 0 : -1;
}
