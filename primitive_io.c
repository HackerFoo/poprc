/* Copyright 2012-2019 Dustin DeWeese
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

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "trace.h"
#include "io.h"
#include "primitive_io.h"
#include "startle/map.h"

#if INTERFACE

typedef struct {
  file_t *(*open)   (seg_t);
  seg_t   (*read)   (file_t *);
  void    (*write)  (file_t *, seg_t);
  void    (*unread) (file_t *, seg_t);
  void    (*close)  (file_t *);
} io_t;

#endif

const io_t default_io = {
  .read = io_read,
  .write = io_write,
  .unread = io_unread,
  .open = io_open,
  .close = io_close
};

const io_t *io = &default_io;

#define WARN_ALT(op) LOG_WHEN(c->alt, MARK("WARN") " IO (" #op ") with alt")

WORD("open", open, 2, 2)
OP(open) {
  cell_t *res = 0;
  PRE(open);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(string)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(open);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 2, T_OPAQUE, ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    void *h = io->open(value_seg(q));
    CHECK_IF(!h, FAIL);
    store_lazy_dep(c->expr.arg[2], make_opaque(h), ctx->alt_set);
    res = ref(p);
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("close", close, 2, 1)
OP(close) {
  cell_t *res = 0;
  PRE(close);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(opaque)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(close);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    io->close(q->value.opaque);
    res = ref(p);
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

response write_op(cell_t **cp, context_t *ctx, void (*op)(file_t *, seg_t)) {
  cell_t *res = 0;
  PRE(write);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(opaque)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(string)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r);

  WARN_ALT(write);

  if(ANY(is_var, p, q, r)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 3, T_OPAQUE, ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    op((file_t *)q->value.opaque, value_seg(r));
    res = ref(p);
    store_lazy_dep(c->expr.arg[3], ref(q), ctx->alt_set);
  }
  add_conditions(res, p, q, r);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("write", write, 3, 2)
OP(write) {
  return write_op(cp, ctx, io->write);
}

WORD("unread", unread, 3, 2)
OP(unread) {
  return write_op(cp, ctx, io->unread);
}

WORD("read", read, 2, 3)
OP(read) {
  cell_t *res = 0;
  PRE(read);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(opaque)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(read);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 2, T_OPAQUE, ctx->alt_set);
    store_dep_var(c, res, 3, T_STRING, ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    seg_t s = io->read((file_t *)q->value.opaque);
    store_lazy_dep(c->expr.arg[2], ref(q), ctx->alt_set);
    store_lazy_dep(c->expr.arg[3], make_string(s), ctx->alt_set);
    res = ref(p);
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

#define KEY_BITS (sizeof(uintptr_t) * 8)
#define ID_BITS (KEY_BITS / 4)
#define ADDR_BITS (KEY_BITS - ID_BITS)
#define ADDR_MASK ((1l << ADDR_BITS) - 1)
#define ID_MAX ((1l << ID_BITS) - 1)
static MAP(arrays, 1 << 16);

val_t next_array_id = 1;

void array_init() {
  map_clear(arrays);
  next_array_id = 1;
}

bool array_read(uintptr_t arr, uintptr_t addr, val_t *out) {
  addr &= ADDR_MASK;
  if(!INRANGE(arr, 1, ID_MAX)) return false;
  pair_t *p = map_find(arrays, arr << 16 | addr);
  if(!p) return false;
  *out = p->second;
  return true;
}

bool array_write(uintptr_t arr, uintptr_t addr, val_t in) {
  addr &= ADDR_MASK;
  if(!INRANGE(arr, 1, ID_MAX)) return false;
  map_replace_insert(arrays, (pair_t) {arr << 16 | addr, in});
  return true;
}

void unique_id(cell_t *c, val_t *next_id) {
  if(!c->value.id) {
    c->value.id = (*next_id)++;
  }
}

WORD("read_array", read_array, 2, 2)
OP(read_array) {
  cell_t *res = 0;
  PRE(read_array);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_Array)));
  CHECK(reduce_arg(c, 1, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(read_array);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 2, T_INT, ctx->alt_set);
  } else {
    val_t x = 0;
    CHECK_IF(p->value.symbol != SYM_Array, FAIL);
    unique_id(p, &next_array_id);
    CHECK_IF(!array_read(p->value.id, q->value.integer, &x), FAIL);
    store_lazy_dep(c->expr.arg[2], val(T_INT, x), ctx->alt_set);
    res = ref(p);
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("write_array", write_array, 3, 1)
OP(write_array) {
  cell_t *res = 0;
  PRE(write);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_Array)));
  CHECK(reduce_arg(c, 1, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r);

  WARN_ALT(write);

  if(ANY(is_var, p, q, r)) {
    res = var(T_SYMBOL, c);
  } else {
    CHECK_IF(p->value.symbol != SYM_Array, FAIL);
    unique_id(p, &next_array_id);
    CHECK_IF(!array_write(p->value.id,
                          q->value.integer,
                          r->value.integer), FAIL);
    res = ref(p);
  }
  add_conditions(res, p, q, r);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

/* Local Variables: */
/* eval: (add-to-list 'imenu-generic-expression '("Operator" "^.*OP(\\([a-z_]+\\)).*$" 1)) */
/* End: */
