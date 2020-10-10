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

#include <stdio.h>
#include <string.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "user_func.h"
#include "list.h"
#include "builders.h"
#include "ir/trace.h"
#include "var.h"
#include "parse/parse.h"
#include "parameters.h"

cell_t *empty_list() {
  return make_list(0);
}

cell_t *make_list(csize_t n) {
  cell_t *c = alloc_list(n);
  *c = (cell_t) {
    .size = c->size,
    .op = OP_value,
    .value = { .type = T_LIST }
  };
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = alloc_list(1);
  *c = (cell_t) {
    .size = c->size,
    .op = OP_value,
    .value = { .type = T_LIST,
               .ptr = {x}}
  };
  return c;
}

cell_t *row_quote(cell_t *x) {
  cell_t *q = quote(x);
  FLAG_SET(*q, value, ROW);
  return q;
}

#if INTERFACE
#define is_list(c) _is_list(GET_CELL(c))
#define is_row_list(c) _is_row_list(GET_CELL(c))
#define is_id_list(c) _is_id_list(GET_CELL(c))
#define is_function(c) _is_function(GET_CELL(c))
#endif

bool _is_list(cell_t const *c) {
  return c && is_value(c) && !is_var(c) && ONEOF(c->value.type, T_LIST, T_RETURN);
}

bool _is_row_list(cell_t const *c) {
  return is_list(c) && FLAG(*c, value, ROW);
}

bool _is_id_list(cell_t const *c) {
  return is_list(c) && FLAG(*c, value, ROW) && list_size(c) == 1;
}

bool _is_function(cell_t const *c) {
  return c && is_var(c) && c->value.type == T_LIST;
}

PARAMETER(unroll_limit, int, 20, "limit for unrolling lists") {
  unroll_limit = clamp(1, 200, arg);
}

// not really a func
response func_list(cell_t **cp, context_t *ctx) {
  PRE(list);
  if(c->priority) {
    CHECK_PRIORITY(c->priority);

    // commit - force everything in this branch ***
    ctx->priority = PRIORITY_TOP;
    ctx->flags &= ~CONTEXT_REDUCE_LISTS;
    c->priority = 0;
  }

  csize_t n = list_size(c);
  if(n == 0) return SUCCESS;
  bool row = is_row_list(c);

  ctx->alt_set = c->value.alt_set;
  COUNTUP(i, n) {
    context_t *arg_ctx = row && i == n - 1 ?
      &CTX(list, 0, 0) :
      &CTX(any);
    if(ctx->t != T_RETURN) arg_ctx->flags &= ~CONTEXT_REDUCE_LISTS;
    if(c->value.ptr[i] && !closure_is_ready(c->value.ptr[i])) continue;
    response r = reduce_ptr(c, i, arg_ctx);
    if(ctx->depth > 0 && r == FAIL) {
      // shrink the list to the first failure
      closure_shrink_list(c, i);
      RANGEUP(j, i, n) {
        drop(c->value.ptr[j]);
      }
      FLAG_CLEAR(*c, value, ROW);
      n = i;
      break;
    } else {
      CHECK(r);
      CHECK_IF(as_conflict(ctx->alt_set), FAIL);
    }
  }
  CHECK_DELAY();
  tcell_t *vc = value_condition(c);
  if(vc) {
    TRAVERSE(c, ptrs) {
      unique(p); // avoid modification of *p by add_conditions_var
      add_conditions_var(*p, vc);
    }
  }
  if(n && row && is_list(c->value.ptr[n-1])) {
    if(ctx->depth >= unroll_limit) {
      drop(c->value.ptr[n-1]);
      c->value.ptr[n-1] = make_list(0);
      FLAG_SET(*c, value, ABBREV);
    } else {
      response r = func_list(&c->value.ptr[n-1], &CTX(any));
      if(r == FAIL) {
        c->value.ptr[n-1] = make_list(0);
      } else {
        CHECK(r);
        ctx->alt_set |= c->value.ptr[n-1]->value.alt_set;
        CHECK_IF(as_conflict(ctx->alt_set), FAIL);
      }
    }
  }
  c->value.alt_set = ctx->alt_set;
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

void reduce_list(cell_t **cp, int depth) {
  context_t *arg_ctx = &CTX(return);
  arg_ctx->priority = PRIORITY_TOP;
  arg_ctx->depth = depth;
  response rsp = SUCCESS;
  cell_t **p = cp;
  while(*p) {
    rsp = func_list(p, arg_ctx);
    assert_error(rsp != DELAY);
    if(rsp == SUCCESS) {
      p = &(*p)->alt;
    }
  }
}

list_iterator_t list_begin(cell_t *l) {
  assert_error(is_list(l), "%C", l);
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

bool is_row_arg(const list_iterator_t *it) {
  return it->row && it->index > it->size;
}

response reduce_row(const list_iterator_t *it, csize_t remaining, context_t *ctx) {
  if(it->row && it->index == it->size) {
    cell_t **rp = &it->array[it->size];
    if(!is_value(*rp)) {
      return reduce(rp, &CTX(list, 0, remaining));
    }
  }
  return SUCCESS;
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
    FLAG_SET(*l, value, ROW);
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
// TODO handle unrolling
csize_t list_remaining_size(list_iterator_t it, bool count_last_row) {
  if(!it.array || it.index > it.size) return 0;
  csize_t n = 0;
  while(it.row) {
    cell_t **rp = &it.array[it.size];
    if(is_list(*rp)) {
      n += it.size - it.index;
      it = list_begin(it.array[it.size]);
    } else {
      it.size += !!count_last_row;
      break;
    }
  }
  return n + csub(it.size, it.index);
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

// returns a copy of the rest of the list
cell_t *list_rest(list_iterator_t it) {
  if(!it.array) return NULL;
  if(!it.index) {
    // just return the cell owning the array
    return ref((cell_t *)((char *)it.array - offsetof(cell_t, value.ptr)));
  }
  csize_t elems = it.size - it.index + it.row;
  cell_t *rest;
  if(elems == 1 && it.row && is_list(it.array[it.size])) {
    rest = ref(it.array[it.size]);
  } else {
    rest = make_list(elems);
    COUNTUP(i, elems) {
      rest->value.ptr[i] = ref(it.array[i + it.index]);
    }
    if(it.row) FLAG_SET(*rest, value, ROW);
  }
  return rest;
}

void collapse_row(cell_t **cp) {
  cell_t *l = *cp, *p;
  if(is_id_list(l) &&
     closure_is_ready(p = l->value.ptr[0])) {
    if(l->value.var) {
      *cp = build_seq(ref(p), l);
    } else {
      *cp = CUT(l, value.ptr[0]);
    }
  }
}

cell_t *flat_copy(cell_t *l) {
  assert_error(is_list(l));
  csize_t n = function_out(l, true);
  cell_t *res = make_list(n);
  res->value.type = l->value.type;
  FLAG_CLEAR(*res, value, ROW);
  cell_t **p, **rp = res->value.ptr;
  list_iterator_t it = list_begin(l);
  WHILELIST(p, it, true) {
    *rp++ = *p;
  }

  if(it.row) {
    FLAG_SET(*res, value, ROW);
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
       (*p)->value.type != T_INT ||
       (*p)->value.integer != x++) {
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
  if(!l || !is_list(l)) return 0;
  list_iterator_t it = list_begin((cell_t *)l);
  return list_remaining_size(it, include_row_var); // ***
}

// find the leftmost list
cell_t **left_list(cell_t **l) {
  while(is_row_list(*l)) {
    cell_t **x = left_elem(*l);
    if(!is_list(*x)) break;
    l = x;
  }
  return l;
}

// get the left element in a chunk
cell_t **left_elem(cell_t *l) {
  csize_t n = list_size(l);
  return n ? &l->value.ptr[n-1] : NULL;
}

cell_t **leftmost_row(cell_t **lp) {
  return left_elem(*left_list(lp));
}

// TODO optimize this
cell_t **leftmost_2(cell_t **lp, bool row) {
  cell_t *l = *lp, **x, **last = lp;
  FORLIST(x, l, row) {
    last = x;
  }
  return last;
}

cell_t **leftmost_1(cell_t **lp) {
  return leftmost_2(lp, true);
}

// find the leftmost element
#if INTERFACE
#define leftmost(...) DISPATCH(leftmost, __VA_ARGS__)
#endif

// optimize later
cell_t **needs_arg(cell_t *l) {
  cell_t **x;
  FORLIST(x, l, true) {
    if(*x && !closure_is_ready(*x)) {
      return x;
    }
  }
  return NULL;
}

csize_t args_required(const cell_t *c) {
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

csize_t function_in(cell_t *l) {
  if(!l || !is_list(l) || is_empty_list(l)) return 0;
  csize_t in = 0;
  cell_t **x;
  FORLIST(x, l, true) { // *** is this loop necessary?
    in += args_required(*x);
  }
  return in;
}

TEST(function_in) {
  cell_t *l = make_list(2);
  l->value.ptr[0] = int_val(2);
  l->value.ptr[1] = quote(func(OP_exec, 2, 1));
  l->value.flags = VALUE_ROW;
  csize_t in = function_in(l);
  drop(l);
  return in == 2 ? 0 : -1;
}

qsize_t quote_size(cell_t *c, bool row) {
  return (qsize_t) {
    .in = function_in(c),
    .out = function_out(c, row)
  };
}

bool is_nil(cell_t *l) {
  return is_list(l) && list_size(l) == 0;
}

// find the function variable in a list
cell_t *get_list_function_var(cell_t *c) {
  cell_t *left = *leftmost(&c);
       if(!left)                return NULL;
  else if(is_function(left))    return left;
  else if(is_placeholder(left)) return left->expr.arg[closure_in(left) - 1];
  else                          return NULL;
}

void unwrap_id_lists(cell_t **lp) {
  while(is_id_list(*lp)) *lp = (*lp)->value.ptr[0];
}

cell_t *id_list(cell_t *c) {
  cell_t *l = make_list(1);
  l->value.ptr[0] = c;
  FLAG_SET(*l, value, ROW);
  return l;
}
