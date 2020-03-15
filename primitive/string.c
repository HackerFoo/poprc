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
#include <stdlib.h>
#include <inttypes.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "debug/print.h"
#include "parse/parse.h"
#include "var.h"
#include "primitive/string.h"

WORD("++", strcat, 2, 1)
OP(strcat) {
  cell_t *res = 0;
  PRE(strcat);

  CHECK_IF(!check_type(ctx->t, T_STRING), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK(reduce_arg(c, 1, &CTX(string)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  if(ANY(is_var, p, q)) {
    res = var(T_STRING, c);
  } else {
    res = make_strcat(value_seg(p), value_seg(q));
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("=s", eq_str, 2, 1)
OP(eq_str) {
  cell_t *res = 0;
  PRE(eq_str);

  CHECK_IF(!check_type(ctx->t, T_SYMBOL), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK(reduce_arg(c, 1, &CTX(string)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  if(ANY(is_var, p, q)) {
    res = var(T_SYMBOL, c);
  } else {
    res = val(T_SYMBOL, eq_seg(value_seg(p), value_seg(q)));
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

static char tmp_string_buf[1024];

seg_t int_to_string(uintptr_t x) {
  int n = snprintf(tmp_string_buf, sizeof(tmp_string_buf), "%lld", (long long)x);
  assert_error(n < (int)sizeof(tmp_string_buf));
  return (seg_t) { .s = tmp_string_buf, .n = n };
}

WORD("->str", to_string, 1, 1)
OP(to_string) {
  cell_t *res = 0;
  PRE(to_string);

  CHECK_IF(!check_type(ctx->t, T_STRING), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(any)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_STRING, c);
  } else if(p->value.type == T_INT) {
    res = make_string(int_to_string(p->value.integer));
  } else if(p->value.type == T_SYMBOL) {
    res = make_string(string_seg(symbol_string(p->value.symbol)));
  } else {
    ABORT(FAIL);
  }
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("->bin", to_string_bin, 1, 1)
OP(to_string_bin) {
  cell_t *res = 0;
  PRE(to_string_bin);

  CHECK_IF(!check_type(ctx->t, T_STRING), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(int)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_STRING, c);
  } else if(p->value.type == T_INT) {
    int32_t x = p->value.integer;
    res = make_string((seg_t) { .s = (char *)&x, .n = sizeof(x) });
  }
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("<-str", from_string, 1, 1)
OP(from_string) {
  cell_t *res = 0;
  PRE(from_string);

  CHECK_IF(!check_type(ctx->t, T_INT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_INT, c);
  } else {
    char *end = NULL;
    val_t x = strtoll(p->value.str, &end, 0);
    CHECK_IF(!end || *end != '\0', FAIL);
    res = val(T_INT, x);
  }
  FLAG_SET(*c, expr, PARTIAL);
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("<-bin", from_string_bin, 1, 1)
OP(from_string_bin) {
  cell_t *res = 0;
  PRE(from_string_bin);

  CHECK_IF(!check_type(ctx->t, T_INT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_INT, c);
  } else {
    val_t x = 0;
    memcpy(&x, p->value.str, min(string_size(p), sizeof(int)));
    res = val(T_INT, x);
  }
  FLAG_SET(*c, expr, PARTIAL);
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("strsplit", strsplit, 2, 2)
OP(strsplit) {
  cell_t *res = 0;
  PRE(strsplit);

  CHECK_IF(!check_type(ctx->t, T_STRING), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK(reduce_arg(c, 1, &CTX(string)));
  CHECK_DELAY();
  ARGS(p, q);

  if(is_var(p)) {
    res = var(T_STRING, c);
    store_dep_var(c, res, 2, T_STRING, RANGE_ALL, ctx->alt_set);
  } else {
    // TODO rewrite to handle strings containing \0
    char *s = strstr(p->value.str, q->value.str);
    CHECK_IF(!s, FAIL);
    int needle_len = strlen(q->value.str);
    res = make_string((seg_t) {.s = p->value.str, .n = s - p->value.str});
    store_lazy_dep(c->expr.arg[2],
                   make_string((seg_t) {.s = s + needle_len,
                                        .n = strlen(s + needle_len)}),
                   0);
  }
  FLAG_SET(*c, expr, PARTIAL);
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("strtrim", strtrim, 1, 1)
OP(strtrim) {
  cell_t *res = 0;
  PRE(strtrim);

  CHECK_IF(!check_type(ctx->t, T_STRING), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(string)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_STRING, c);
  } else {
    res = make_string(seg_trim(string_seg(p->value.str)));
  }
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

/* Local Variables: */
/* eval: (add-to-list 'imenu-generic-expression '("Operator" "^.*OP(\\([a-z_]+\\)).*$" 1)) */
/* End: */
