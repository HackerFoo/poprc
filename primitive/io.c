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

#include <string.h>

#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"
#include "startle/static_alloc.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "io.h"
#include "startle/map.h"
#include "var.h"
#include "primitive/io.h"

#if INTERFACE

typedef struct {
  file_t *(*open)   (seg_t);
  seg_t   (*read)   (file_t *);
  void    (*write)  (file_t *, seg_t);
  void    (*unread) (file_t *, seg_t);
  int     (*seek)   (file_t *, int);
  void   *(*mmap)   (file_t *, size_t, int);
  void    (*munmap) (void *, size_t);
  void    (*close)  (file_t *);
} io_t;

#endif

const io_t default_io = {
  .read = io_read,
  .write = io_write,
  .unread = io_unread,
  .seek = io_seek,
  .mmap = io_mmap,
  .munmap = io_munmap,
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
    store_dep_var(c, res, 2, T_OPAQUE, RANGE(SYM_File), ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    void *h = io->open(value_seg(q));
    CHECK_IF(!h, FAIL);
    store_lazy_dep(c->expr.arg[2], opaque(SYM_File, h), ctx->alt_set);
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
  CHECK(reduce_arg(c, 1, &CTX(opaque, SYM_File)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(close);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    CHECK_IF(q->value.symbol != SYM_File, FAIL);
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
  CHECK(reduce_arg(c, 1, &CTX(opaque, SYM_File)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(string)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r);

  WARN_ALT(write);

  if(ANY(is_var, p, q, r)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 3, T_OPAQUE, RANGE(SYM_File), ctx->alt_set);
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
  CHECK(reduce_arg(c, 1, &CTX(opaque, SYM_File)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(read);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 2, T_OPAQUE, RANGE(SYM_File), ctx->alt_set);
    store_dep_var(c, res, 3, T_STRING, RANGE_ALL, ctx->alt_set);
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

#define KEY_BITS sizeof_bits(uintptr_t)
#define ID_BITS (KEY_BITS / 4)
#define ADDR_BITS (KEY_BITS - ID_BITS)
#define ADDR_MASK ((1l << ADDR_BITS) - 1)
#define ID_MAX ((1l << ID_BITS) - 1)
static MAP(arrays, 1 << 16);

val_t next_array_id = 1;

STATIC_ALLOC(mmap_array, mmap_array_t, 8);
static unsigned int mmap_array_count = 0;

void array_init() {
  map_clear(arrays);
  next_array_id = 1;
  mmap_array_count = 0;
}

static
mmap_array_t *lookup_mmap_array(uintptr_t arr) {
  COUNTUP(i, mmap_array_count) {
    mmap_array_t *ma = &mmap_array[i];
    if(ma->id == arr) return ma;
  }
  return NULL;
}

static
mmap_array_t *new_mmap_array(file_t *file, void *addr, int size, int width) {
  if(mmap_array_count >= mmap_array_size) return NULL;
  if(width < 1) width = 1;
  if(size < 0) size = 0;
  mmap_array_t *ma = &mmap_array[mmap_array_count++];
  ma->id = next_array_id++;
  ma->size = size;
  ma->width = width;
  ma->file = file;
  ma->data = (char *)addr;
  return ma;
}

bool array_read(uintptr_t arr, uintptr_t addr, val_t *out) {
  mmap_array_t *ma = lookup_mmap_array(arr);
  if(!ma) {
    addr &= ADDR_MASK;
    if(!INRANGE(arr, 1, ID_MAX)) return false;
    pair_t *p = map_find(arrays, arr << 16 | addr);
    if(!p) return false;
    *out = p->second;
    return true;
  } else {
    if(!FLAG(*ma, file, IN)) return false;
    size_t offset = addr * ma->width;
    if(offset >= ma->size) return false;
    *out = 0;
    memcpy(out, ma->data + offset, min(sizeof(*out), ma->width));
    return true;
  }
}

bool array_write(uintptr_t arr, uintptr_t addr, val_t in) {
  mmap_array_t *ma = lookup_mmap_array(arr);
  if(!ma) {
    addr &= ADDR_MASK;
    if(!INRANGE(arr, 1, ID_MAX)) return false;
    map_replace_insert(arrays, (pair_t) {arr << 16 | addr, in});
    return true;
  } else {
    if(!FLAG(*ma, file, OUT)) return false;
    size_t offset = addr * ma->width;
    if(offset >= ma->size) return false;
    memcpy(ma->data + offset, &in, min(sizeof(in), ma->width));
    return true;
  }
}

WORD("read_array", read_array, 2, 2)
OP(read_array) {
  cell_t *res = 0;
  PRE(read_array);

  CHECK_IF(!check_type(ctx->t, T_OPAQUE), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(opaque, SYM_Array)));
  CHECK(reduce_arg(c, 1, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  WARN_ALT(read_array);

  if(ANY(is_var, p, q)) {
    res = opaque_var(c, SYM_Array);
    store_dep_var(c, res, 2, T_INT, default_bound, ctx->alt_set);
  } else {
    val_t x = 0;
    CHECK_IF(p->value.symbol != SYM_Array, FAIL);
    if(array_read(p->value.id, q->value.integer, &x)) {
      store_lazy_dep(c->expr.arg[2], val(T_INT, x), ctx->alt_set);
    } else {
      drop(c);
      store_fail(c->expr.arg[2], NULL, ctx);
    }
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
  PRE(write_array);

  CHECK_IF(!check_type(ctx->t, T_OPAQUE), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(opaque, SYM_Array)));
  CHECK(reduce_arg(c, 1, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r);

  WARN_ALT(write);

  if(ANY(is_var, p, q, r)) {
    res = opaque_var(c, SYM_Array);
  } else {
    CHECK_IF(p->value.symbol != SYM_Array, FAIL);
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

WORD("dup_array", dup_array, 1, 2)
OP(dup_array) {
  cell_t *res = 0;
  PRE(dup_array);

  CHECK_IF(!check_type(ctx->t, T_OPAQUE), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(opaque, SYM_Array)));
  CHECK_DELAY();
  ARGS(p);

  WARN_ALT(dup_array);

  if(is_var(p)) {
    res = opaque_var(c, SYM_Array);
    store_dep_var(c, res, 1, T_OPAQUE, RANGE(SYM_Array), ctx->alt_set);
  } else {
    res = ref(p);
    store_lazy_dep(c->expr.arg[1], ref(p), ctx->alt_set);
  }
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("seek", seek, 3, 2)
OP(seek) {
  cell_t *res = 0;
  PRE(seek);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(opaque, SYM_File)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r);

  WARN_ALT(seek);

  if(ANY(is_var, p, q, r)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 3, T_OPAQUE, RANGE(SYM_File), ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    io->seek((file_t *)q->value.opaque, r->value.integer);
    res = ref(p);
    store_lazy_dep(c->expr.arg[3], ref(q), ctx->alt_set);
  }
  add_conditions(res, p, q, r);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("mmap", mmap, 5, 2)
OP(mmap) {
  cell_t *res = 0;
  PRE(mmap);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
  CHECK(reduce_arg(c, 1, &CTX(opaque, SYM_File)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 2, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 3, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK(reduce_arg(c, 4, &CTX(int)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q, r, s, t);

  WARN_ALT(mmap);

  if(ANY(is_var, p, q, r, s, t)) {
    res = var(T_SYMBOL, c);
    store_dep_var(c, res, 5, T_OPAQUE, RANGE(SYM_Array), ctx->alt_set);
  } else {
    CHECK_IF(p->value.symbol != SYM_IO, FAIL);
    file_t *file = (file_t *)q->value.opaque;
    void *data = io->mmap(file, r->value.integer, s->value.integer);
    CHECK_IF(!data, FAIL);
    mmap_array_t *ma = new_mmap_array(file, data, r->value.integer, t->value.integer);
    res = ref(p);
    cell_t *arr = opaque(SYM_Array, NULL);
    arr->value.id = ma->id;
    store_lazy_dep(c->expr.arg[5], arr, ctx->alt_set);
  }
  add_conditions(res, p, q, r, s, t);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

// TODO munmap

/* Local Variables: */
/* eval: (add-to-list 'imenu-generic-expression '("Operator" "^.*OP(\\([a-z_]+\\)).*$" 1)) */
/* End: */
