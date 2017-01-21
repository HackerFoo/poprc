/* Copyright 2012-2016 Dustin DeWeese
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
#include <assert.h>
#include <stdio.h>
#include "rt_types.h"
#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/print.h"
#include "gen/test.h"

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

cell_t *_op2(val_t (*op)(val_t, val_t), cell_t *x, cell_t *y) {
  csize_t size = min(val_size(x),
                     val_size(y));
  cell_t *res = vector(size);
  for(csize_t i = 0; i < size; ++i)
    res->value.integer[i] = op(x->value.integer[i],
                               y->value.integer[i]);
  res->size = size + 1;
  return res;
}

bool func_op2(cell_t **cp, type_t t, type_t arg_type, type_t res_type, val_t (*op)(val_t, val_t)) {
  cell_t *c = *cp;
  cell_t *res = 0;
  assert(!is_marked(c));

  if(t != T_ANY && t != res_type) goto fail;

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, arg_type) ||
     !reduce_arg(c, 1, &alt_set, arg_type) ||
     as_conflict(alt_set)) goto fail;
  clear_flags(c);

  cell_t *p = c->expr.arg[0], *q = c->expr.arg[1];
  res = is_var(p) || is_var(q) ? var(t) : _op2(op, p, q);
  res->value.type |= res_type;
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  return true;

 fail:
  fail(cp, t);
  return false;
}

// WORD("+", add, 2, 1)
val_t add_op(val_t x, val_t y) { return x + y; }
bool func_add(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_INT, add_op); }


// WORD("*", mul, 2, 1)
val_t mul_op(val_t x, val_t y) { return x * y; }
bool func_mul(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_INT, mul_op); }

// WORD("-", sub, 2, 1)
val_t sub_op(val_t x, val_t y) { return x - y; }
bool func_sub(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_INT, sub_op); }

// WORD(">", gt, 2, 1)
val_t gt_op(val_t x, val_t y) { return x > y; }
bool func_gt(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, gt_op); }

// WORD(">=", gte, 2, 1)
val_t gte_op(val_t x, val_t y) { return x >= y; }
bool func_gte(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, gte_op); }

// WORD("<", lt, 2, 1)
val_t lt_op(val_t x, val_t y) { return x < y; }
bool func_lt(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, lt_op); }

// WORD("<=", lte, 2, 1)
val_t lte_op(val_t x, val_t y) { return x <= y; }
bool func_lte(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, lte_op); }

// WORD("==", eq, 2, 1)
// WORD("=:=", eq_s, 2, 1)
val_t eq_op(val_t x, val_t y) { return x == y; }
bool func_eq(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, eq_op); }
bool func_eq_s(cell_t **cp, type_t t) { return func_op2(cp, t, T_SYMBOL, T_SYMBOL, eq_op); }

// WORD("!=", neq, 2, 1)
// WORD("!:=", neq_s, 2, 1)
val_t neq_op(val_t x, val_t y) { return x != y; }
bool func_neq(cell_t **cp, type_t t) { return func_op2(cp, t, T_INT, T_SYMBOL, neq_op); }
bool func_neq_s(cell_t **cp, type_t t) { return func_op2(cp, t, T_SYMBOL, T_SYMBOL, neq_op); }

// WORD(".", compose, 2, 1)
bool func_compose(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, T_LIST) ||
     !reduce_arg(c, 1, &alt_set, T_LIST) ||
     as_conflict(alt_set)) goto fail;
  clear_flags(c);

  cell_t *res = compose_nd(ref(c->expr.arg[0]), ref(c->expr.arg[1]));
  store_reduced(cp, mod_alt(res, c->alt, alt_set));
  ASSERT_REF();
  return true;

 fail:
  fail(cp, t);
  return false;
}

// WORD("pushl", pushl, 2, 1)
bool func_pushl(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(*cp));

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 1, &alt_set, T_LIST)) goto fail;
  clear_flags(c);

  cell_t *q = c->expr.arg[1];
  bool rvar = is_var(q);
  cell_t *res = pushl_nd(ref(c->expr.arg[0]), ref(q));
  if(rvar) res->value.type |= T_VAR;
  drop(res->alt);
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  store_reduced(cp, res);
  ASSERT_REF();
  return true;

 fail:
  fail(cp, t);
  return false;
}

// WORD("pushr", pushr, 2, 1)
bool func_pushr(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, T_LIST)) goto fail;
  clear_flags(c);

  cell_t *p = c->expr.arg[0];

  int n = list_size(p);
  cell_t *res = expand(ref(p), 1);
  memmove(res->value.ptr+1, res->value.ptr, sizeof(cell_t *)*n);
  res->value.ptr[0] = ref(c->expr.arg[1]);
  res->value.alt_set = alt_set;
  drop(res->alt);
  res->alt = c->alt;

  store_reduced(cp, res);
  ASSERT_REF();
  return true;

 fail:
  fail(cp, t);
  return false;
}

// WORD//("'", quote, 1, 1)
/*
bool func_quote(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t res = { .size = 2, .value.ptr = {ref(c->expr.arg[0])} };
  store_reduced(cp, &res);
  return true;
}
*/

// WORD("popr", popr, 1, 2)
bool func_popr(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp, *d = c->expr.arg[1];
  assert(!is_marked(*cp));

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, T_LIST)) goto fail;
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  if(list_size(p) == 0) goto fail;

  // adds an extra output dep to the placeholder, and puts the dep in front of it
  // [P[in|out] -> [P[in|out+d], d]
  // also marks result as a variable
  cell_t *res;
  cell_t **l = p->value.ptr;
  cell_t *res_d;
  if(is_placeholder(*l)) {
    //closure_set_ready(l[0], true);
    /* just hand out a variable, let the compiler track it
    *l = expand_deps_inplace(*l, 1); // *** no shift here
    res_d = ref((*l)->expr.arg[closure_in(*l)] = dep(ref(*l)));
    */
    res_d = var(T_ANY);
    res = mod_alt(ref(p), c->alt, alt_set);
  } else if(closure_is_ready(*l)) {
    /* drop the right list element */
    res_d = ref(*l);
    res = closure_alloc(closure_args(p)-1);
    res->func = func_value;
    csize_t elems = list_size(res);
    res->value.type = T_LIST | (is_var(p) ? T_VAR : 0);
    for(csize_t i = 0; i < elems; ++i) {
      res->value.ptr[i] = ref(l[i+1]);
    }
    res->value.alt_set = alt_set;
    res->alt = c->alt;
  } else goto fail;

  store_lazy_dep(d, res_d, alt_set);
  store_reduced(cp, res);
  ASSERT_REF();
  return true;

 fail:
  fail(cp, t);
  return false;
}

// WORD("|", alt, 2, 1)
bool func_alt(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  uint8_t a = new_alt_id(1);
  cell_t *r0 = id(c->expr.arg[0], as_single(a, 0));
  cell_t *r1 = id(c->expr.arg[1], as_single(a, 1));
  r0->alt = r1;
  store_lazy(cp, c, r0, 0);
  return false;
}

// WORD_DISABLED("||", alt2, 2, 1)
bool func_alt2(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t *r0 = id(ref(c->expr.arg[0]), 0);
  cell_t *r1 = ref(c->expr.arg[1]);
  r0->alt = r1;
  *cp = r0;
  drop(c);
  return false;
}


cell_t *map_assert(cell_t *c, cell_t *t) {
  if(!(is_list(c) && list_size(c) > 0)) return var(c->value.type);
  cell_t *nc = copy(c);
  traverse(nc, {
      cell_t *np = closure_alloc(2);
      np->func = func_assert;
      np->expr.arg[0] = ref(*p);
      np->expr.arg[1] = ref(t);
      *p = np;
    }, PTRS);
  return nc;
}

// WORD("!", assert, 2, 1)
bool func_assert(cell_t **cp, type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 1, &alt_set, T_SYMBOL)) goto fail;
  cell_t *p = clear_ptr(c->expr.arg[1]);

  if(!(p->value.integer[0] == SYM_True || is_var(p))) goto fail;
  if(is_var(p) && t != T_LIST) trace(c, 0, tt_reduction, 0); // *** HACKy, e.g. t == T_ANY
  if(!reduce_arg(c, 0, &alt_set, t) ||
     as_conflict(alt_set)) goto fail;
  clear_flags(c);
  cell_t *res;
  if(is_var(p)) {
    res = map_assert(c->expr.arg[0], p);
    res->value.alt_set = alt_set;
    res->alt = c->alt;
  } else {
    res = mod_alt(ref(c->expr.arg[0]), c->alt, alt_set);
  }
  store_reduced(cp, res);
  return true;
fail:
  fail(cp, t);
  return false;
}

// WORD("id", id, 1, 1)
bool func_id(cell_t **cp, type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  alt_set_t alt_set = c->expr.alt_set;

  //if(alt_set || c->alt) {
    if(!reduce_arg(c, 0, &alt_set, t) ||
       as_conflict(alt_set)) goto fail;
    clear_flags(c);

    cell_t *p = c->expr.arg[0];
    store_reduced(cp, mod_alt(ref(p), c->alt, alt_set));
    return true;
    /*
  } else {
    // makes tracing hard because of the silent rewrite
    cell_t *p = ref(c->expr.arg[0]);
    drop(c);
    *cp = p;
    return false;
  }
    */
 fail:
  fail(cp, t);
  return false;
}

// WORD("drop", drop, 2, 1)
bool func_drop(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t *p = ref(c->expr.arg[0]);
  drop(c);
  *cp = p;
  return false;
}

// WORD("swap", swap, 2, 2)
bool func_swap(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t *d = c->expr.arg[2];
  store_lazy_dep(d, c->expr.arg[0], 0);
  store_lazy(cp, c, c->expr.arg[1], 0);
  return false;
}

cell_t *id(cell_t *c, alt_set_t as) {
  cell_t *i = closure_alloc(1);
  i->func = func_id;
  i->expr.arg[0] = c;
  i->expr.alt_set = as;
  return i;
}

// WORD("dup", dup, 1, 2)
bool func_dup(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t *d = c->expr.arg[1];
  store_lazy_dep(d, ref(c->expr.arg[0]), 0);
  store_lazy(cp, c, c->expr.arg[0], 0);
  return false;
}

bool func_ap(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));

  const csize_t
    in = closure_in(c),
    n = closure_args(c),
    out = closure_out(c);

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, in-1, &alt_set, T_LIST)) goto fail;
  clear_flags(c);

  cell_t *l = c->expr.arg[in-1];
  COUNTDOWN(i, in-1) {
    l = pushl_nd(c->expr.arg[i], l);
  }

  csize_t ln = list_size(l);
  //cell_t *ph = 0;
  if(ln && is_placeholder(l->value.ptr[ln-1])) {
    //ph = l->value.ptr[ln-1];
    ln--;
  }

  csize_t stop = min(ln, out);
  csize_t i;
  for(i = 0; i < stop; i++) {
    store_lazy_dep(c->expr.arg[n-1-i], ref(l->value.ptr[i]), alt_set);
  }

  for(; i < out; i++) {
    cell_t *p = c->expr.arg[n-1-i];
    drop(c);
    store_fail(p, p->alt);
  }

  if(out > ln) goto fail; // todo: handle placeholders

  /* drop the right list elements */
  cell_t *res = closure_alloc(closure_args(l) - out);
  csize_t elems = list_size(res);
  res->value.type = T_LIST;
  for(csize_t i = 0; i < elems; ++i)
    res->value.ptr[i] = ref(l->value.ptr[i+(n-in)]);

  res->func = func_value;
  res->value.alt_set = alt_set;
  res->alt = c->alt;
  c->alt = 0;
  store_lazy(cp, c, res, 0);
  drop(l);
  return false;
fail:
  fail(cp, t);
  return false;
}

// WORD("print", print, 2, 1)
bool func_print(cell_t **cp, type_t t) {
  cell_t *c = *cp;
  cell_t *res = 0;
  assert(!is_marked(c));

  if(t != T_ANY && t != T_SYMBOL) goto fail;

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, T_SYMBOL) ||
     !reduce_arg(c, 1, &alt_set, T_ANY) ||
     as_conflict(alt_set)) goto fail;
  clear_flags(c);

  if(c->alt) {
    drop(c->alt);
    c->alt = 0;
  }

  cell_t *p = c->expr.arg[0], *q = c->expr.arg[1];
  if(is_var(p) || is_var(q)) {
    res = var(t);
  } else if(p->value.integer[0] == SYM_IO) {
    show_one(q);
    res = ref(p);
  } else goto fail;
  store_reduced(cp, res);
  return true;

 fail:
  drop(res);
  fail(cp, t);
  return false;
}

// WORD("is_nil", is_nil, 1, 1)
bool func_is_nil(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(*cp));

  if(t != T_ANY && t != T_SYMBOL) goto fail;

  alt_set_t alt_set = 0;
  if(!reduce_arg(c, 0, &alt_set, T_LIST)) goto fail;
  clear_flags(c);

  cell_t *p = c->expr.arg[0];
  cell_t *res;
  if(is_var(p)) {
    res = var(T_SYMBOL);
  } else {
    res = symbol(list_size(p) == 0 ? SYM_True : SYM_False);
  }

  res->value.alt_set = alt_set;
  res->alt = c->alt;
  store_reduced(cp, res);
  return true;

 fail:
  fail(cp, t);
  return false;
}
