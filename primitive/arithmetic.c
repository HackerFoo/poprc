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

#include <math.h>
#include <stdlib.h>
#include <inttypes.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "builders.h"
#include "special.h"
#include "ir/trace.h"
#include "var.h"
#include "primitive/arithmetic.h"

// forward inference only
range_t infer_bound(range_t (*range_op)(range_t, range_t),
                    cell_t *x, cell_t *y) {
  return range_op(get_range(x), get_range(y));
}

cell_t *_op2(cell_t *c, type_t arg_type, type_t res_type,
             val_t (*op)(val_t, val_t), range_t (*range_op)(range_t, range_t),
             cell_t *x, cell_t *y) {
  if(ANY(is_var, x, y)) {
    cell_t *v = var(res_type, c);
    v->value.range = infer_bound(range_op, x, y);
    return v;
  } else {
    return val(res_type, op(arg_type == T_INT ? x->value.integer : x->value.symbol,
                            arg_type == T_INT ? y->value.integer : y->value.symbol));
  }
}

cell_t *_op1(cell_t *c, type_t arg_type, type_t res_type,
             val_t (*op)(val_t), cell_t *x) {
  assert_error(ONEOF(arg_type, T_INT, T_SYMBOL));
  if(is_var(x)) {
    return var(res_type, c);
  } else {
    return val(res_type, op(arg_type == T_INT ?
                            x->value.integer :
                            x->value.symbol));
  }
}

static
bool bound_contexts_noop(UNUSED const cell_t *c,
                         UNUSED const context_t *ctx,
                         UNUSED context_t *arg_ctx) {
  return false;
}

static
bool bound_contexts_arith(const cell_t *c,
                          const context_t *ctx,
                          context_t *arg_ctx) {
  if(trace_current_entry() == NULL) {
    return false;
  }

  range_t bound = ctx->t == T_INT ? ctx->bound : default_bound;
  op op = c->op;
  if(is_value(c->expr.arg[0]) && !is_var(c->expr.arg[0])) {
    val_t lhs = c->expr.arg[0]->value.integer;
    context_t *other = &arg_ctx[1];
    switch(op) {
    case OP_add:
      other->bound.min = sat_subi(bound.min, lhs);
      other->bound.max = sat_subi(bound.max, lhs);
      break;
    case OP_sub:
      other->bound.min = sat_subi(lhs, bound.max);
      other->bound.max = sat_subi(lhs, bound.min);
      break;
    case OP_mul:
      if(lhs > 0) {
        other->bound.min = div_maxi(bound.min, lhs);
        other->bound.max = div_mini(bound.max, lhs);
      } else if(lhs < 0) {
        other->bound.min = div_maxi(bound.max, lhs);
        other->bound.max = div_mini(bound.min, lhs);
      } else {
        other->bound.min = INTPTR_MIN;
        other->bound.max = INTPTR_MAX;
      }
      break;
    case OP_div:
    default:
      return false;
    }
    return true;
  } else if(is_value(c->expr.arg[1]) && !is_var(c->expr.arg[1])) {
    val_t rhs = c->expr.arg[1]->value.integer;
    context_t *other = &arg_ctx[0];
    switch(op) {
    case OP_add: // same as above
      other->bound.min = sat_subi(bound.min, rhs);
      other->bound.max = sat_subi(bound.max, rhs);
      break;
    case OP_sub:
      other->bound.min = sat_addi(bound.min, rhs);
      other->bound.max = sat_addi(bound.max, rhs);
      break;
    case OP_mul: // same as above
      if(rhs > 0) {
        other->bound.min = div_maxi(bound.min, rhs);
        other->bound.max = div_mini(bound.max, rhs);
      } else if(rhs < 0) {
        other->bound.min = div_maxi(bound.max, rhs);
        other->bound.max = div_mini(bound.min, rhs);
      } else {
        other->bound.min = INTPTR_MIN;
        other->bound.max = INTPTR_MAX;
      }
      break;
    case OP_div:
      if(rhs > 0) {
        other->bound.min = sat_muli(bound.min, rhs);
        other->bound.max = sat_muli(bound.max, rhs);
      } else if(rhs < 0) {
        other->bound.min = sat_muli(bound.max, rhs);
        other->bound.max = sat_muli(bound.min, rhs);
      }
      break;
    default:
      return false;
    }
    return true;
  } else {
    return false;
  }
}

static
bool bound_contexts_cmp(const cell_t *c,
                        const context_t *ctx,
                        context_t *arg_ctx) {
  if(trace_current_entry() == NULL ||
     ctx->t != T_SYMBOL ||
     !range_singleton(ctx->bound)) {
    return false;
  }

  op op = c->op;
  context_t *other = NULL;
  val_t val;
  if(is_value(c->expr.arg[0]) && !is_var(c->expr.arg[0])) {
    val = c->expr.arg[0]->value.integer;
    other = &arg_ctx[1];
    switch(op) { // reverse op
    case OP_lt:  op = OP_gt;  break;
    case OP_lte: op = OP_gte; break;
    case OP_gt:  op = OP_lt;  break;
    case OP_gte: op = OP_lte; break;
    default: break;
    }
  } else if(is_value(c->expr.arg[1]) && !is_var(c->expr.arg[1])) {
    val = c->expr.arg[1]->value.integer;
    other = &arg_ctx[0];
  } else return false;

  if(ctx->bound.min == SYM_False) {
    switch(op) { // negate op
    case OP_lt:    op = OP_gte;   break;
    case OP_lte:   op = OP_gt;    break;
    case OP_gt:    op = OP_lte;   break;
    case OP_gte:   op = OP_lt;    break;
    case OP_eq:    op = OP_neq;   break;
    case OP_neq:   op = OP_eq;    break;
    case OP_eq_s:  op = OP_neq_s; break;
    case OP_neq_s: op = OP_eq_s;  break;
    default: break;
    }
  }

  if(ONEOF(op, OP_neq, OP_neq_s)) return false; // can't represent neq

  other->bound.min = INTPTR_MIN;
  other->bound.max = INTPTR_MAX;
  switch(op) {
  case OP_lt:  other->bound.max = sat_subi(val, 1); break;
  case OP_lte: other->bound.max = val;              break;
  case OP_gt:  other->bound.min = sat_addi(val, 1); break;
  case OP_gte: other->bound.min = val;              break;
  case OP_eq:
  case OP_eq_s:
    other->bound.min = other->bound.max = val; break;
  default: break;
  }

  return true;
}

range_t no_range_op(UNUSED range_t a,
                    UNUSED range_t b) {
  return RANGE_ALL;
}

cell_t *no_identity(UNUSED cell_t *p,
                    UNUSED cell_t *q) {
  return NULL;
}

// CLEANUP merge with func_op2_float
response func_op2(cell_t **cp, context_t *ctx,
                  type_t arg_type, type_t res_type,
                  val_t (*op)(val_t, val_t),
                  range_t (*range_op)(range_t, range_t),
                  bool nonzero,
                  bool (*bound_contexts)(const cell_t *, const context_t *, context_t *),
                  cell_t *(*identity)(cell_t *, cell_t *)) {
  cell_t *res = 0;
  PRE(op2, "%O", (*cp)->op);

  CHECK_IF(!check_type(ctx->t, res_type), FAIL);

  context_t arg_ctx[] = {CTX(t, arg_type), CTX(t, arg_type)};
  bound_contexts(c, ctx, arg_ctx);
  if(!nonzero) CHECK(reduce_arg(c, 0, &arg_ctx[0]));
  CHECK(reduce_arg(c, 1, &arg_ctx[1]));
  if(nonzero && rsp != DELAY) { // TODO assert this for variables
    cell_t *q = c->expr.arg[1];
    CHECK_IF(!is_var(q) && q->value.integer == 0, FAIL);
  }
  CHECK(reduce_arg(c, 0, &arg_ctx[0]));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  res = identity(p, q);
  if(res) {
    if(!is_value(res)) {
      add_conditions(res, p, q);
      res->alt = take(&c->alt);
      drop(c);
      *cp = res;
      ABORT(RETRY);
    }
  } else {
    res = _op2(c, arg_type, res_type, op, range_op, p, q);
    if(nonzero) FLAG_SET(*c, expr, PARTIAL);
  }
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

response func_op1(cell_t **cp, context_t *ctx,
                  int arg_type, int res_type,
                  val_t (*op)(val_t), val_t (*inv_op)(val_t)) {
  cell_t *res = 0;
  PRE(op1, "%O", (*cp)->op);

  CHECK_IF(!check_type(ctx->t, res_type), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(t, arg_type, CTX_INV(inv_op))));
  CHECK_DELAY();
  ARGS(p);

  res = _op1(c, arg_type, res_type, op, p);
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

cell_t *_op2_float(double (*op)(double, double), cell_t *x, cell_t *y) {
  return float_val(op(x->value.flt,
                      y->value.flt));
}

cell_t *_op1_float(double (*op)(double), cell_t *x) {
  return float_val(op(x->value.flt));
}

response func_op2_float(cell_t **cp, context_t *ctx, double (*op)(double, double), bool nonzero) {
  cell_t *res = 0;
  PRE(op2_float, "%O", (*cp)->op);

  CHECK_IF(!check_type(ctx->t, T_FLOAT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(float)));
  CHECK(reduce_arg(c, 1, &CTX(float)));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();
  ARGS(p, q);

  CHECK_IF(nonzero && !is_var(q) && q->value.flt == 0.0, FAIL); // TODO assert this for variables
  res = ANY(is_var, p, q) ? var(T_FLOAT, c) : _op2_float(op, p, q);
  if(nonzero) FLAG_SET(*c, expr, PARTIAL);
  add_conditions(res, p, q);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

response func_op1_float(cell_t **cp, context_t *ctx, double (*op)(double)) {
  cell_t *res = 0;
  PRE(op1_float, "%O", (*cp)->op);

  CHECK_IF(!check_type(ctx->t, T_FLOAT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(float)));
  CHECK_DELAY();
  ARGS(p);

  res = is_var(p) ? var(T_FLOAT, c) : _op1_float(op, p);
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("+", add, 2, 1)
val_t add_op(val_t x, val_t y) { return x + y; }
range_t add_range_op(range_t a, range_t b) {
  return (range_t) {
    .min = sat_addi(a.min, b.min),
    .max = sat_addi(a.max, b.max)
  };
}
cell_t *add_identity(cell_t *p, cell_t *q) {
  return
    !is_var(p) && p->value.integer == 0 ? ref(q) :
    !is_var(q) && q->value.integer == 0 ? ref(p) : NULL;
}
OP(add) {
  return func_op2(cp, ctx, T_INT, T_INT, add_op, add_range_op, false, bound_contexts_arith, add_identity);
}

WORD("*", mul, 2, 1)
val_t mul_op(val_t x, val_t y) { return x * y; }
range_t mul_range_op(range_t x, range_t y) {
  val_t a = sat_muli(x.min, y.min);
  val_t b = sat_muli(x.min, y.max);
  val_t c = sat_muli(x.max, y.min);
  val_t d = sat_muli(x.max, y.max);
  return (range_t) {
    .min = min(min(a, b), min(c, d)),
    .max = max(max(a, b), max(c, d))
  };
}
static
cell_t *mul_identity_symmetry(cell_t *p, cell_t *q) {
  cell_t *res = NULL;
  val_t x = p->value.integer;
  if(!is_var(p)) {
    if(x == 0) {
      res = val(T_INT, 0);
      if(is_var(q)) res = build_seq(res, ref(q));
    } else if(x == 1) res = ref(q);
    else if(x == -1) res = build_negate(ref(q));
    else {
      val_t ax = x < 0 ? -x : x;
      if(!(ax & ax-1)) { // is ax a power of 2?
        val_t n = int_log2l(ax);
        res = build_shiftl(ref(q), val(T_INT, n));
        if(x < 0) res = build_negate(res);
      }
    }
  }
  return res;
}
cell_t *mul_identity(cell_t *p, cell_t *q) {
  cell_t *res = mul_identity_symmetry(p, q);
  return res ? res : mul_identity_symmetry(q, p);
}
OP(mul) {
  return func_op2(cp, ctx, T_INT, T_INT, mul_op, mul_range_op, false, bound_contexts_arith, mul_identity);
}

static
bool same_var(const cell_t *p, const cell_t *q) {
  return
    p == q ||
    (is_var(p) &&
     is_var(q) &&
     p->value.var &&
     p->value.var == q->value.var);
}

WORD("-", sub, 2, 1)
val_t sub_op(val_t x, val_t y) { return x - y; }
range_t sub_range_op(range_t a, range_t b) {
  return (range_t) {
    .min = sat_subi(a.min, b.max),
    .max = sat_subi(a.max, b.min)
  };
}
cell_t *sub_identity(cell_t *p, cell_t *q) {
  return
    same_var(p, q) ? val(T_INT, 0) :
    !is_var(p) && p->value.integer == 0 ? build_negate(ref(q)) :
    !is_var(q) && q->value.integer == 0 ? ref(p) : NULL;
}
OP(sub) {
  return func_op2(cp, ctx, T_INT, T_INT, sub_op, sub_range_op, false, bound_contexts_arith, sub_identity);
}

WORD("/", div, 2, 1)
val_t div_op(val_t x, val_t y) { return x / y; }
range_t div_range_op(range_t n, range_t d) {
  if(d.min <= 0 && d.max >= 0) return RANGE_ALL;
  val_t d_small = d.max < 0 ? d.max : d.min;
  if(d_small > 0) {
    return (range_t) {
      .min = div_i(n.min, d_small),
      .max = div_i(n.max, d_small)
    };
  } else {
    return (range_t) {
      .min = div_i(n.max, d_small),
      .max = div_i(n.min, d_small)
    };
  }
}
cell_t *div_identity(cell_t *p, cell_t *q) {
  return
    same_var(p, q) ? val(T_INT, 1) :
    is_var(q) ? NULL :
    q->value.integer == 0 ? &fail_cell :
    q->value.integer == 1 ? ref(p) :
    q->value.integer == -1 ? build_negate(ref(p)) :
    NULL;
}
OP(div) {
  return func_op2(cp, ctx, T_INT, T_INT, div_op, div_range_op, true, bound_contexts_arith, div_identity);
}

WORD("%", mod, 2, 1)
val_t mod_op(val_t x, val_t y) { return x % y; }
range_t mod_range_op(range_t n, range_t d) {
  val_t da = max(sat_abs(d.min), sat_abs(d.max));
  if(range_singleton(d) &&
     !ONEOF(d.min, INTPTR_MAX, INTPTR_MIN)) {
    val_t n_span = range_span(n);
    if(n_span == 0 || da == 0) return RANGE_NONE;
    if(n_span ==
       range_span((range_t) { .min = n.min % da,
                              .max = n.max % da })) {
      return (range_t) {
        .min = n.min % da,
        .max = n.max % da
      };
    }
  }
  return (range_t) {
    .min = max(min(n.min, 0), sat_negi(sat_subi(da, 1))),
    .max = min(max(n.max, 0), sat_subi(da, 1))
  };
}
OP(mod) {
  return func_op2(cp, ctx, T_INT, T_INT, mod_op, mod_range_op, true, bound_contexts_noop, no_identity);
}

WORD("neg", negate, 1, 1)
val_t negate_op(val_t x) { return -x; }
OP(negate) {
  return func_op1(cp, ctx, T_INT, T_INT, negate_op, negate_op);
}

WORD("+f", add_float, 2, 1)
double add_float_op(double x, double y) { return x + y; }
OP(add_float) {
  return func_op2_float(cp, ctx, add_float_op, false);
}

WORD("*f", mul_float, 2, 1)
double mul_float_op(double x, double y) { return x * y; }
OP(mul_float) {
  return func_op2_float(cp, ctx, mul_float_op, false);
}

WORD("-f", sub_float, 2, 1)
double sub_float_op(double x, double y) { return x - y; }
OP(sub_float) {
  return func_op2_float(cp, ctx, sub_float_op, false);
}

WORD("/f", div_float, 2, 1)
double div_float_op(double x, double y) { return x / y; }
OP(div_float) {
  return func_op2_float(cp, ctx, div_float_op, true);
}

WORD("log", log, 1, 1)
OP(log) {
  return func_op1_float(cp, ctx, log);
}

WORD("exp", exp, 1, 1)
OP(exp) {
  return func_op1_float(cp, ctx, exp);
}

WORD("cos", cos, 1, 1)
OP(cos) {
  return func_op1_float(cp, ctx, cos);
}

WORD("sin", sin, 1, 1)
OP(sin) {
  return func_op1_float(cp, ctx, sin);
}

WORD("tan", tan, 1, 1)
OP(tan) {
  return func_op1_float(cp, ctx, tan);
}

WORD("atan2", atan2, 2, 1)
OP(atan2) {
  return func_op2_float(cp, ctx, atan2, false);
}

WORD("sqrt", sqrt, 1, 1)
OP(sqrt) {
  return func_op1_float(cp, ctx, sqrt);
}

WORD("&b", bitand, 2, 1)
val_t bitand_op(val_t x, val_t y) { return x & y; }
range_t bitand_range_op(range_t a, range_t b) {
  if(a.min < 0) {
    return b.min < 0 ? RANGE_ALL :
      (range_t) {
        .min = 0,
        .max = b.max
      };
  } else {
    return
      (range_t) {
        .min = 0,
        .max = b.min < 0 ? a.max : min(a.max, b.max)
      };
  }
}
OP(bitand) {
  return func_op2(cp, ctx, T_INT, T_INT, bitand_op, bitand_range_op, false, bound_contexts_noop, no_identity);
}

WORD("|b", bitor, 2, 1)
val_t bitor_op(val_t x, val_t y) { return x | y; }
OP(bitor) {
  return func_op2(cp, ctx, T_INT, T_INT, bitor_op, no_range_op, false, bound_contexts_noop, no_identity);
}

WORD("^b", bitxor, 2, 1)
val_t bitxor_op(val_t x, val_t y) { return x ^ y; }
OP(bitxor) {
  return func_op2(cp, ctx, T_INT, T_INT, bitxor_op, no_range_op, false, bound_contexts_noop, no_identity);
}

WORD("<<b", shiftl, 2, 1)
val_t shiftl_op(val_t x, val_t y) { return x << y; }
OP(shiftl) {
  return func_op2(cp, ctx, T_INT, T_INT, shiftl_op, no_range_op, false, bound_contexts_noop, no_identity);
}

WORD(">>b", shiftr, 2, 1)
val_t shiftr_op(val_t x, val_t y) { return x >> y; }
OP(shiftr) {
  return func_op2(cp, ctx, T_INT, T_INT, shiftr_op, no_range_op, false, bound_contexts_noop, no_identity);
}

WORD("~b", complement, 1, 1)
val_t complement_op(val_t x) { return ~x; }
OP(complement) {
  return func_op1(cp, ctx, T_INT, T_INT, complement_op, complement_op);
}

WORD("not", not, 1, 1)
val_t not_op(val_t x) { return !x; }
OP(not) {
  return func_op1(cp, ctx, T_SYMBOL, T_SYMBOL, not_op, not_op);
}

range_t bool_range_op(UNUSED range_t a, UNUSED range_t b) {
  return (range_t) {
    .min = SYM_False,
    .max = SYM_True
  };
}

cell_t *eq_identity(cell_t *p, cell_t *q) {
  return same_var(p, q) ? val(T_SYMBOL, SYM_True) : NULL;
}

cell_t *neq_identity(cell_t *p, cell_t *q) {
  return same_var(p, q) ? val(T_SYMBOL, SYM_False) : NULL;
}

WORD(">", gt, 2, 1)
val_t gt_op(val_t x, val_t y) { return x > y; }
OP(gt) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, gt_op, bool_range_op, false, bound_contexts_cmp, neq_identity);
}

WORD(">=", gte, 2, 1)
val_t gte_op(val_t x, val_t y) { return x >= y; }
OP(gte) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, gte_op, bool_range_op, false, bound_contexts_cmp, eq_identity);
}

WORD("<", lt, 2, 1)
val_t lt_op(val_t x, val_t y) { return x < y; }
OP(lt) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, lt_op, bool_range_op, false, bound_contexts_cmp, neq_identity);
}

WORD("<=", lte, 2, 1)
val_t lte_op(val_t x, val_t y) { return x <= y; }
OP(lte) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, lte_op, bool_range_op, false, bound_contexts_cmp, eq_identity);
}

val_t eq_op(val_t x, val_t y) { return x == y; }
WORD("==", eq, 2, 1)
OP(eq) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, eq_op, bool_range_op, false, bound_contexts_cmp, eq_identity);
}

WORD("=:=", eq_s, 2, 1)
OP(eq_s) {
  return func_op2(cp, ctx, T_SYMBOL, T_SYMBOL, eq_op, bool_range_op, false, bound_contexts_cmp, eq_identity);
}

WORD("!=", neq, 2, 1)
val_t neq_op(val_t x, val_t y) { return x != y; }
OP(neq) {
  return func_op2(cp, ctx, T_INT, T_SYMBOL, neq_op, bool_range_op, false, bound_contexts_cmp, neq_identity);
}

WORD("!:=", neq_s, 2, 1)
OP(neq_s) {
  return func_op2(cp, ctx, T_SYMBOL, T_SYMBOL, neq_op, bool_range_op, false, bound_contexts_cmp, neq_identity);
}

WORD("->f", to_float, 1, 1)
OP(to_float) {
  cell_t *res = 0;
  PRE(to_float);

  CHECK_IF(!check_type(ctx->t, T_FLOAT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(int)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_FLOAT, c);
  } else {
    res = float_val(p->value.integer);
  }
  add_conditions(res, p);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("trunc", trunc, 1, 1)
OP(trunc) {
  cell_t *res = 0;
  PRE(to_float);

  CHECK_IF(!check_type(ctx->t, T_INT), FAIL);

  CHECK(reduce_arg(c, 0, &CTX(float)));
  CHECK_DELAY();
  ARGS(p);

  if(is_var(p)) {
    res = var(T_INT, c);
  } else {
    res = int_val(p->value.flt);
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
