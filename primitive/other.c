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

#include <stdlib.h>
#include <inttypes.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "list.h"
#include "var.h"
#include "primitive/other.h"

response func_type(cell_t **cp, context_t *ctx, type_t type) {
  PRE(type);

  CHECK_IF(!check_type(ctx->t, type), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(t, type)));
  CHECK_DELAY();

  store_reduced(cp, ctx, ref(c->expr.arg[0]));
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

// annotations to work around inference failures

WORD("int_t", int_t, 1, 1)
OP(int_t) {
  return func_type(cp, ctx, T_INT);
}

WORD("symbol_t", symbol_t, 1, 1)
OP(symbol_t) {
  return func_type(cp, ctx, T_SYMBOL);
}

WORD("float_t", float_t, 1, 1)
OP(float_t) {
  return func_type(cp, ctx, T_FLOAT);
}

WORD("list_t", list_t, 1, 1)
OP(list_t) {
  return func_type(cp, ctx, T_LIST);
}

// a placeholder for FFI
OP(external) {
  PRE(external);

  csize_t in = closure_in(c), n = closure_args(c);
  assert_error(in >= 1);
  bool io = expected_symbol(ctx, SYM_IO);

  // get name, which must be a constant before code generation
  CHECK(reduce_arg(c, in - 1, &CTX(string)));
  cell_t *name = c->expr.arg[in - 1];
  if(!is_var(name) || name->value.var) {
    LOG("extern name must be a constant %C", c);
  }

  COUNTUP(i, in - 1) {
    if(i == 0 && io) {
      CHECK(reduce_arg(c, 0, &CTX(symbol, SYM_IO)));
    } else {
      CHECK(reduce_arg(c, i, &CTX(any)));
    }
    CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  }
  CHECK_DELAY();

  cell_t *res = var(ctx->t, c);
  RANGEUP(i, in, n) {
    cell_t *d = c->expr.arg[i];
    if(d && is_dep(d)) {
      store_dep_var(c, res, i, T_ANY, default_bound, ctx->alt_set);
    } else {
      LOG("dropped extern[%C] output", c);
    }
  }
  add_conditions_from_array(res, c->expr.arg, in);
  store_reduced(cp, ctx, res);
  ASSERT_REF();
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("--partial", partial, 1, 1)
OP(partial) {
  // TODO check arg op and reference count
  *cp = CUT(*cp, expr.arg[0]);
  FLAG_SET(**cp, expr, PARTIAL);
  return RETRY;
}

// force inlining of a quote
WORD("--inline", annotate_inline, 1, 1)
OP(annotate_inline) {
  PRE(annotate_inline);
  CHECK(reduce_arg(c, 0, &CTX_UP));
  CHECK_DELAY();

  cell_t *res = ref(c->expr.arg[0]);
  FLAG_SET(*res, value, INLINE);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

// for testing
WORD("#row", row_id, 1, 1)
OP(row_id) {
  cell_t *c = *cp;
  cell_t *l = make_list(1);
  FLAG_SET(*l, value, ROW);
  l->value.ptr[0] = ref(c->expr.arg[0]);
  drop(c);
  *cp = l;
  return RETRY;
}

/* Local Variables: */
/* eval: (add-to-list 'imenu-generic-expression '("Operator" "^.*OP(\\([a-z_]+\\)).*$" 1)) */
/* End: */
