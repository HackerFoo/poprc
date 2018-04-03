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

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "print.h"
#include "trace.h"
#include "list.h"

   /*-----------------------------------------------,
    |          VARIABLE NAME CONVENTIONS            |
    |-----------------------------------------------|
    |                                               |
    |  cell_t *c = closure being reduced            |
    |  cell_t *d = first dep                        |
    |    (second closure returned, lower on stack)  |
    |  cell_t *e = second dep                       |
    |  cell_t *f = ...                              |
    |  cell_t *p = expr.arg[0], leftmost arg        |
    |  cell_t *q = expr.arg[1]                      |
    |  bool s = success                             |
    |  cell_t *res = result to be stored in c       |
    |  cell_t *res_X = result to be stored in X     |
    |                                               |
    '-----------------------------------------------*/

cell_t *_op2(cell_t *c, uint8_t t, val_t (*op)(val_t, val_t), cell_t *x, cell_t *y) {
  if(is_var(x) || is_var(y)) {
    cell_t *res = var(t, c);
    if(value_in_integer(x) &&
       value_in_integer(y)) {
      set_var_value(res, op(x->value.integer,
                            y->value.integer));
    }
    return res;
  } else {
    return val(t, op(x->value.integer,
                     y->value.integer));
  }
}

cell_t *_op1(cell_t *c, uint8_t t, val_t (*op)(val_t), cell_t *x) {
  if(is_var(x)) {
    cell_t *res = var(t, c);
    if(value_in_integer(x)) {
      set_var_value(res, op(x->value.integer));
    }
    return res;
  } else {
    return val(t, op(x->value.integer));
  }
}

response func_op2(cell_t **cp, type_request_t treq, int arg_type, int res_type, val_t (*op)(val_t, val_t), bool nonzero) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, op2);

  CHECK(!check_type(treq.t, res_type), FAIL);

  alt_set_t alt_set = 0;
  type_request_t atr = REQ(t, arg_type);
  CHECK(AND0(reduce_arg(c, 0, &alt_set, atr),
             reduce_arg(c, 1, &alt_set, atr),
             fail_if(as_conflict(alt_set))));
  clear_flags(c);

  cell_t *p = c->expr.arg[0], *q = c->expr.arg[1];
  CHECK(nonzero && !is_var(q) && q->value.integer == 0, FAIL); // TODO assert this for variables
  res = _op2(c, res_type, op, p, q);
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

response func_op1(cell_t **cp, type_request_t treq, int arg_type, int res_type, val_t (*op)(val_t), val_t (*inv_op)(val_t)) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, op1);

  CHECK(!check_type(treq.t, res_type), FAIL);

  alt_set_t alt_set = 0;
  type_request_t atr = REQ(t, arg_type, REQ_INV(inv_op));
  CHECK(reduce_arg(c, 0, &alt_set, atr));
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  res = _op1(c, res_type, op, p);
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

cell_t *_op2_float(double (*op)(double, double), cell_t *x, cell_t *y) {
  return float_val(op(x->value.flt,
                      y->value.flt));
}

cell_t *_op1_float(double (*op)(double), cell_t *x) {
  return float_val(op(x->value.flt));
}

response func_op2_float(cell_t **cp, type_request_t treq, double (*op)(double, double), bool nonzero) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, op2_float);

  CHECK(!check_type(treq.t, T_FLOAT), FAIL);

  alt_set_t alt_set = 0;
  type_request_t atr = REQ(float);
  CHECK(AND0(reduce_arg(c, 0, &alt_set, atr),
             reduce_arg(c, 1, &alt_set, atr),
             fail_if(as_conflict(alt_set))));
  clear_flags(c);

  cell_t *p = c->expr.arg[0], *q = c->expr.arg[1];
  CHECK(nonzero && !is_var(q) && q->value.flt == 0.0, FAIL); // TODO assert this for variables
  res = is_var(p) || is_var(q) ? var(treq.t, c) : _op2_float(op, p, q);
  res->value.type = T_FLOAT;
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

response func_op1_float(cell_t **cp, type_request_t treq, double (*op)(double)) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, op1_float);

  CHECK(!check_type(treq.t, T_FLOAT), FAIL);

  alt_set_t alt_set = 0;
  type_request_t atr = REQ(float);
  CHECK(reduce_arg(c, 0, &alt_set, atr));
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  res = is_var(p) ? var(treq.t, c) : _op1_float(op, p);
  res->value.type = T_FLOAT;
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

WORD("+", add, 2, 1)
val_t add_op(val_t x, val_t y) { return x + y; }
OP(add) {
  return func_op2(cp, treq, T_INT, T_INT, add_op, false);
}

WORD("*", mul, 2, 1)
val_t mul_op(val_t x, val_t y) { return x * y; }
OP(mul) {
  return func_op2(cp, treq, T_INT, T_INT, mul_op, false);
}

WORD("-", sub, 2, 1)
val_t sub_op(val_t x, val_t y) { return x - y; }
OP(sub) {
  return func_op2(cp, treq, T_INT, T_INT, sub_op, false);
}

WORD("/", div, 2, 1)
val_t div_op(val_t x, val_t y) { return x / y; }
OP(div) {
  return func_op2(cp, treq, T_INT, T_INT, div_op, true);
}

WORD("%", mod, 2, 1)
val_t mod_op(val_t x, val_t y) { return x % y; }
OP(mod) {
  return func_op2(cp, treq, T_INT, T_INT, mod_op, true);
}

WORD("+f", add_float, 2, 1)
double add_float_op(double x, double y) { return x + y; }
OP(add_float) {
  return func_op2_float(cp, treq, add_float_op, false);
}

WORD("*f", mul_float, 2, 1)
double mul_float_op(double x, double y) { return x * y; }
OP(mul_float) {
  return func_op2_float(cp, treq, mul_float_op, false);
}

WORD("-f", sub_float, 2, 1)
double sub_float_op(double x, double y) { return x - y; }
OP(sub_float) {
  return func_op2_float(cp, treq, sub_float_op, false);
}

WORD("/f", div_float, 2, 1)
double div_float_op(double x, double y) { return x / y; }
OP(div_float) {
  return func_op2_float(cp, treq, div_float_op, true);
}

WORD("log", log, 1, 1)
OP(log) {
  return func_op1_float(cp, treq, log);
}

WORD("exp", exp, 1, 1)
OP(exp) {
  return func_op1_float(cp, treq, exp);
}

WORD("cos", cos, 1, 1)
OP(cos) {
  return func_op1_float(cp, treq, cos);
}

WORD("sin", sin, 1, 1)
OP(sin) {
  return func_op1_float(cp, treq, sin);
}

WORD("tan", tan, 1, 1)
OP(tan) {
  return func_op1_float(cp, treq, tan);
}

WORD("atan2", atan2, 2, 1)
OP(atan2) {
  return func_op2_float(cp, treq, atan2, false);
}

WORD("&b", bitand, 2, 1)
val_t bitand_op(val_t x, val_t y) { return x & y; }
OP(bitand) {
  return func_op2(cp, treq, T_INT, T_INT, bitand_op, false);
}

WORD("|b", bitor, 2, 1)
val_t bitor_op(val_t x, val_t y) { return x | y; }
OP(bitor) {
  return func_op2(cp, treq, T_INT, T_INT, bitor_op, false);
}

WORD("^b", bitxor, 2, 1)
val_t bitxor_op(val_t x, val_t y) { return x ^ y; }
OP(bitxor) {
  return func_op2(cp, treq, T_INT, T_INT, bitxor_op, false);
}

WORD("<<b", shiftl, 2, 1)
val_t shiftl_op(val_t x, val_t y) { return x << y; }
OP(shiftl) {
  return func_op2(cp, treq, T_INT, T_INT, shiftl_op, false);
}

WORD(">>b", shiftr, 2, 1)
val_t shiftr_op(val_t x, val_t y) { return x >> y; }
OP(shiftr) {
  return func_op2(cp, treq, T_INT, T_INT, shiftr_op, false);
}

WORD("~b", complement, 1, 1)
val_t complement_op(val_t x) { return ~x; }
OP(complement) {
  return func_op1(cp, treq, T_INT, T_INT, complement_op, complement_op);
}

WORD("not", not, 1, 1)
val_t not_op(val_t x) { return !x; }
OP(not) {
  return func_op1(cp, treq, T_SYMBOL, T_SYMBOL, not_op, not_op);
}

WORD(">", gt, 2, 1)
val_t gt_op(val_t x, val_t y) { return x > y; }
OP(gt) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, gt_op, false);
}

WORD(">=", gte, 2, 1)
val_t gte_op(val_t x, val_t y) { return x >= y; }
OP(gte) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, gte_op, false);
}

WORD("<", lt, 2, 1)
val_t lt_op(val_t x, val_t y) { return x < y; }
OP(lt) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, lt_op, false);
}

WORD("<=", lte, 2, 1)
val_t lte_op(val_t x, val_t y) { return x <= y; }
OP(lte) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, lte_op, false);
}

WORD("==", eq, 2, 1)
WORD("=:=", eq_s, 2, 1)
val_t eq_op(val_t x, val_t y) { return x == y; }
OP(eq) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, eq_op, false);
}
OP(eq_s) {
  return func_op2(cp, treq, T_SYMBOL, T_SYMBOL, eq_op, false);
}

WORD("!=", neq, 2, 1)
WORD("!:=", neq_s, 2, 1)
val_t neq_op(val_t x, val_t y) { return x != y; }
OP(neq) {
  return func_op2(cp, treq, T_INT, T_SYMBOL, neq_op, false);
}
OP(neq_s) {
  return func_op2(cp, treq, T_SYMBOL, T_SYMBOL, neq_op, false);
}

WORD("->f", to_float, 1, 1)
OP(to_float) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, to_float);

  CHECK(!check_type(treq.t, T_FLOAT), FAIL);

  alt_set_t alt_set = 0;
  CHECK(reduce_arg(c, 0, &alt_set, REQ(int)));
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  if(is_var(p)) {
    res = var(T_FLOAT, c);
  } else {
    res = float_val(p->value.integer);
  }
  res->value.type = T_FLOAT;
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

WORD("trunc", trunc, 1, 1)
OP(trunc) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, to_float);

  CHECK(!check_type(treq.t, T_INT), FAIL);

  alt_set_t alt_set = 0;
  CHECK(reduce_arg(c, 0, &alt_set, REQ(float)));
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  if(is_var(p)) {
    res = var(T_INT, c);
  } else {
    res = int_val(p->value.flt);
  }
  res->value.type = T_INT;
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

WORD("pushr", pushr, 2, 1)
OP(pushr) {
  cell_t *c = *cp;

  // lower to compose
  c = expand(c, 1);
  c->expr.arg[2] = &nil_cell;
  c->op = OP_compose;
  *cp = c;
  return RETRY;
}

WORD("|", alt, 2, 1)
OP(alt) {
  cell_t *c = *cp;
  PRE(c, alt);
  uint8_t a = new_alt_id(1);
  cell_t *r0 = id(c->expr.arg[0], as_single(a, 0));
  cell_t *r1 = id(c->expr.arg[1], as_single(a, 1));
  r0->alt = r1;
  store_lazy(cp, r0, 0);
  return RETRY;
}

cell_t *map_assert(cell_t *c, cell_t *t, cell_t *v) {
  cell_t *nc;
  assert_error(is_list(c));
  if(NOT_FLAG(c->value, VALUE_ROW)) {
    nc = copy_expand(c, 1);
    nc->value.ptr[list_size(nc) - 1] = 0;
    v->value.type = T_LIST; // *** update trace type?
  } else {
    nc = copy(c);
  }
  TRAVERSE(nc, ptrs) {
    if(*p) {
      cell_t *np = closure_alloc(2);
      np->op = OP_assert;
      np->expr.arg[0] = ref(*p);
      np->expr.arg[1] = ref(t);
      *p = np;
    }
  }
  cell_t **left = &nc->value.ptr[list_size(nc) - 1];
  if(NOT_FLAG(c->value, VALUE_ROW)) {
    *left = v;
  } else {
    // slip in v as an extra arg to assert
    assert_error((*left)->op == OP_assert);
    (*left)->size++;
    (*left)->expr.arg[2] = v;
  }
  FLAG_SET(nc->value, VALUE_ROW);
  return nc;
}

WORD("!", assert, 2, 1)
OP(assert) {
  cell_t *c = *cp;
  response rsp;
  PRE(c, assert);

  cell_t *res = NULL;
  alt_set_t alt_set = 0;
  csize_t in = closure_in(c);
  CHECK(reduce_arg(c, 1, &alt_set, REQ(symbol, SYM_True)));
  cell_t *p = clear_ptr(c->expr.arg[1]);

  CHECK((!is_var(p) ||
         value_in_integer(p)) &&
        p->value.integer != SYM_True, FAIL);

  if(in == 3) {
    // use var from map_assert
    res = c->expr.arg[2];
    c->size--;
  } else if(is_var(p) && !value_in_integer(p)) { // TODO clean this up
    CHECK(treq.delay_assert, DELAY_ARG);
    res = var(treq.t, c);
  }

  CHECK(AND0(reduce_arg(c, 0, &alt_set, treq),
             fail_if(as_conflict(alt_set))));
  clear_flags(c);
  cell_t *q = c->expr.arg[0];

  if(is_var(p)) {
    if(!value_in_integer(p)) {
      if(is_list(q)) {
        res = map_assert(q, p, res);
      } else {
        res->value.type = q->value.type;
        FLAG_SET(res->value, VALUE_VAR);
      }
    } else if(is_var(q)) {
      /* The `success dependency` of p must still be carried
         through the assert, because replacing the assert with
         q will lose this dependency e.g. in `True otherwise !`
         the resulting value would not depend on the result of
         `otherwise`. */
      res = var(q->value.type, c);
      if(FLAG(q->value.tc, TC_VALUE)) {
        set_var_value(res, q->value.integer);
      }
    }
    res->value.alt_set = alt_set;
    res->alt = c->alt;
    trace_store_row_assert(c, res);
  }

  if(!res) {
    res = mod_alt(ref(q), c->alt, alt_set);
  }

  store_reduced(cp, res);
  return SUCCESS;

 abort:
  drop(res);
  return abort_op(rsp, cp, treq);
}

WORD("id", id, 1, 1)
OP(id) {
  cell_t *c = *cp;
  response rsp;
  PRE(c, id);
  alt_set_t alt_set = c->expr.alt_set;

  if(alt_set || c->alt || c->pos) {
    CHECK(AND0(reduce_arg(c, 0, &alt_set, treq),
               fail_if(as_conflict(alt_set))));
    clear_flags(c);

    store_reduced(cp, mod_alt(ref(c->expr.arg[0]), c->alt, alt_set));
    return SUCCESS;
  } else {
    *cp = CUT(c, expr.arg[0]);
    return RETRY;
  }

 abort:
  return abort_op(rsp, cp, treq);
}

WORD("drop", drop, 2, 1)
OP(drop) {
  cell_t *c = *cp;
  PRE(c, drop);
  *cp = CUT(c, expr.arg[0]);
  return RETRY;
}

WORD("swap", swap, 2, 2)
OP(swap) {
  cell_t *c = *cp;
  PRE(c, swap);
  cell_t *d = c->expr.arg[2];
  store_lazy_dep(d, c->expr.arg[0], 0);
  store_lazy(cp, c->expr.arg[1], 0);
  return RETRY;
}

cell_t *id(cell_t *c, alt_set_t as) {
  if(!c) return NULL;
  cell_t *i = closure_alloc(1);
  i->op = OP_id;
  i->expr.arg[0] = c;
  i->expr.alt_set = as;
  return i;
}

WORD("dup", dup, 1, 2)
OP(dup) {
  cell_t *c = *cp;
  PRE(c, dup);
  cell_t *d = c->expr.arg[1];
  store_lazy_dep(d, ref(c->expr.arg[0]), 0);
  store_lazy(cp, c->expr.arg[0], 0);
  return RETRY;
}

// outputs required from the left operand given the rignt operand
csize_t function_compose_out(cell_t *c, csize_t arg_in, csize_t out) {
  c = clear_ptr(c);
  return csub(function_in(c) + csub(out, function_out(c, true)), arg_in);
}

// inputs required from the right operand given the left operand
csize_t function_compose_in(cell_t *c, csize_t req_in, csize_t arg_in, bool row) {
  c = clear_ptr(c);
  return csub(req_in, function_in(c)) + function_out(c, row) + arg_in;
}

static
response func_compose_ap(cell_t **cp, type_request_t treq, bool row) {
  cell_t *c = *cp;
  response rsp;
  CONTEXT("%s: %C", row ? "compose" : "ap", c);
  PRE_NO_CONTEXT(c, compose_ap);

  const csize_t
    in = closure_in(c) - 1,
    arg_in = in - row,
    n = closure_args(c),
    out = closure_out(c);

  cell_t *p = NULL;
  int pos = c->pos ? c->pos : c->expr.arg[in]->pos;

  alt_set_t alt_set = 0;
  if(row) {
    CHECK(reduce_arg(c, 0, &alt_set, REQ(list, treq.in, 0)));
    p = clear_ptr(c->expr.arg[0]);
  }
  CHECK(AND0(reduce_arg(c, in, &alt_set,
                        REQ(list,
                            function_compose_in(p, out ? 0 : treq.in, arg_in, false /*_1_*/),
                            treq.out + out)),
             fail_if(as_conflict(alt_set))));

  clear_flags(c);
  if(row) {
    placeholder_extend(&c->expr.arg[0], treq.in, function_compose_out(c->expr.arg[in], arg_in, treq.out + out));
    p = clear_ptr(c->expr.arg[0]);
  }
  placeholder_extend(&c->expr.arg[in], function_compose_in(p, treq.in, arg_in, true /*_1_*/), treq.out + out);
  // *** _1_ don't know if/why this works

  list_iterator_t it;

  // *** prevent leaking outside variables into lists
  if(pos) {
    int skip = TWEAK(-1, "to disable ap barrier %C -> %E", c, trace_expr_entry(pos));
    cell_t *e = trace_expr_entry(pos);
    RANGEUP(i, row, in) {
      if(skip == (int)i) continue;
      cell_t **pa = &c->expr.arg[i];
      if(is_var(*pa)) {
        // *pa = id(*pa, 0);
        switch_entry(e, *pa); // ***
      } else {
        (*pa)->pos = pos;
      }
    }
  }

  reverse_ptrs((void **)c->expr.arg, in);
  it.array = c->expr.arg;
  it.index = 0;
  it.size = in - row;
  it.row = row;
  cell_t *l = compose(it, ref(c->expr.arg[in])); // TODO prevent leaking outside variables
  reverse_ptrs((void **)c->expr.arg, in);

  bool is_nil = c->expr.arg[in] == &nil_cell;
  if(!is_nil) insert_root(&c->expr.arg[in]);
  it = list_begin(l);
  COUNTUP(i, out) {
    cell_t **x = list_next(&it, false);
    if(!x) {
      drop(l);
      LOG("null quote output");
      ABORT(FAIL);
    }
    cell_t *d = c->expr.arg[n-1-i];
    store_lazy_dep(d, ref(*x), alt_set);
    if(d) d->pos = pos; // ***
  }
  if(!is_nil) remove_root(&c->expr.arg[in]);

  cell_t *res = list_rest(it);
  drop(l);
  store_reduced(cp, mod_alt(res, c->alt, alt_set));
  (*cp)->pos = pos; // ***
  ASSERT_REF();
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

WORD("pushl", ap, 2, 1)
WORD("popr", ap, 1, 2)
OP(ap) {
  return func_compose_ap(cp, treq, false);
}

WORD(".", compose, 2, 1)
OP(compose) {
  return func_compose_ap(cp, treq, true);
}

WORD("print", print, 2, 1)
OP(print) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = 0;
  PRE(c, print);

  CHECK(!check_type(treq.t, T_SYMBOL), FAIL);

  alt_set_t alt_set = 0;
  CHECK(AND0(reduce_arg(c, 0, &alt_set, REQ(symbol)),
             reduce_arg(c, 1, &alt_set, REQ(any)),
             fail_if(as_conflict(alt_set))));
  clear_flags(c);

  if(c->alt) {
    drop(c->alt);
    c->alt = 0;
  }

  cell_t *p = c->expr.arg[0], *q = c->expr.arg[1];
  if(is_var(p) || is_var(q)) {
    res = var(T_SYMBOL, c);
  } else if(p->value.integer == SYM_IO) {
    show_one(q);
    res = ref(p);
  } else {
    ABORT(FAIL);
  }
  store_reduced(cp, res);
  return SUCCESS;

 abort:
  drop(res);
  return abort_op(rsp, cp, treq);
}

bool is_list_var(cell_t *c) {
  return is_row_list(c) && is_placeholder(c->value.ptr[0]);
}

response func_type(cell_t **cp, type_request_t treq, uint8_t type) {
  cell_t *c = *cp;
  response rsp;
  PRE(c, type);

  CHECK(!check_type(treq.t, type), FAIL);

  alt_set_t alt_set = 0;
  type_request_t atr = REQ(t, type);
  CHECK(reduce_arg(c, 0, &alt_set, atr));
  clear_flags(c);

  *cp = mod_alt(ref(c->expr.arg[0]), ref(c->alt), 0);
  drop(c);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

// annotations to work around inference failures

WORD("int_t", int_t, 1, 1)
OP(int_t) {
  return func_type(cp, treq, T_INT);
}

WORD("symbol_t", symbol_t, 1, 1)
OP(symbol_t) {
  return func_type(cp, treq, T_SYMBOL);
}

WORD("otherwise", otherwise, 2, 1)
OP(otherwise) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = NULL;
  PRE(c, otherwise);

  alt_set_t alt_set = 0;

  response rsp0 = reduce(&c->expr.arg[0], REQ(any));

  if(rsp0 == FAIL) {
    CHECK(reduce_arg(c, 1, &alt_set, treq));
    clear_flags(c);
    res = mod_alt(ref(c->expr.arg[1]), c->alt, alt_set);
  } else {
    cell_t *p = clear_ptr(c->expr.arg[0]);
    CHECK(!is_var(p), FAIL);
    res = var(T_ANY, c);
    CHECK(AND0(rsp0,
               reduce_arg(c, 1, &alt_set, treq)));
    clear_flags(c);
    cell_t *q = c->expr.arg[1];
    update_var_from_value(res, q);
  }

  store_reduced(cp, res);
  return SUCCESS;

abort:
  return abort_op(rsp, cp, treq);
}
