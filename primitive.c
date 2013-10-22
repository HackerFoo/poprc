/* Copyright 2012-2013 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include "rt_types.h"
#include "gen/rt.h"
#include "gen/primitive.h"

   /*-----------------------------------------------,
    |          VARIABLE NAME CONVENTIONS            |
    |-----------------------------------------------|
    |                                               |
    |  cell_t *c = closure being reduced            |
    |  cell_t *d = first dep                        |
    |    (second closure returned, lower on stack)  |
    |  cell_t *e = second dep                       |
    |  cell_t *f = ...                              |
    |  cell_t *p = arg[0], leftmost arg             |
    |  cell_t *q = arg[1]                           |
    |  bool s = success                             |
    |  cell_t *res = result to be stored in c       |
    |  cell_t *res_X = result to be stored in X     |
    |                                               |
    '-----------------------------------------------*/

/* must be in ascending order */
word_entry_t word_table[] = {
  {"!", func_assert, 2, 1},
  {"'", func_quote, 1, 1},
  {"*", func_mul, 2, 1},
  {"+", func_add, 2, 1},
  {"-", func_sub, 2, 1},
  {".", func_compose, 2, 1},
  {"<", func_lt, 2, 1},
  {"<=", func_lte, 2, 1},
  {"==", func_eq, 2, 1},
  {">", func_gt, 2, 1},
  {">=", func_gte, 2, 1},
  {"_", func_placeholder, 0, 1},
  {"cut", func_cut, 1, 1},
  {"drop", func_drop, 2, 1},
  {"dup", func_dup, 1, 2},
  //{"fib", func_fib, 1, 1},
  {"id", func_id, 1, 1},
  {"ift", func_ift, 3, 1},
  {"popr", func_popr, 1, 2},
  {"pushl", func_pushl, 2, 1},
  {"pushr", func_pushr, 2, 1},
  {"select", func_select, 2, 1},
  {"swap", func_swap, 2, 2},
  {"|", func_alt, 2, 1},
  {"||", func_alt2, 2, 1}
};
const int word_table_length = LENGTH(word_table);

builder_entry_t builder_table[] = {
  {"add", build_add, 2, 1},
  {"mul", build_mul, 2, 1},
  {"sub", build_sub, 2, 1},
  {"gt", build_gt, 2, 1},
  {"gte", build_gte, 2, 1},
  {"lt", build_lt, 2, 1},
  {"lte", build_lte, 2, 1},
  {"eq", build_eq, 2, 1},
  {"compose", build_compose, 2, 1},
  {"pushl", build_pushl, 2, 1},
  {"pushr", build_pushr, 2, 1},
  {"popr", build_popr, 1, 2},
  {"quote", build_quote, 1, 1},
  {"alt", build_alt, 2, 1},
  {"alt2", build_alt2, 2, 1},
  {"assert", build_assert, 2, 1},
  {"id", build_id, 1, 1},
  {"ift", build_ift, 3, 1},
  {"cut", build_cut, 1, 1},
  {"select", build_select, 2, 1},
};

const int builder_table_length = LENGTH(builder_table);

static const type_t _op2_types[] = {T_INT, T_INT, T_INT};
cell_t *_op2(intptr_t (*op)(intptr_t, intptr_t), cell_t *x, cell_t *y) {
  int size = min(val_size(x),
		 val_size(y));
  cell_t *res = vector(size);
  int i;
  for(i = 0; i < size; ++i)
    res->val[i] = op(x->val[i],
		     y->val[i]);
  res->size = size + 1;
  return res;
}

bool func_op2(cell_t **cp, type_rep_t t, intptr_t (*op)(intptr_t, intptr_t)) {
  cell_t *res = 0;
  cell_t *const c = clear_ptr(*cp, 3);
  alt_set_t alt_set = 0;
  static const int n = 2;
  cell_t *arg[n];

  if(t != T_ANY && t != _op2_types[0]) goto fail; //***
  if(!function_preamble(c, &alt_set, arg, (type_t * const)_op2_types, &res, n))
     goto fail;

  if(!res) res = _op2(op, arg[0], arg[1]);

  function_epilogue(cp, alt_set, res, n);
  return true;

 fail:
  fail(cp);
  return false;
}

cell_t *build11(reduce_t f, cell_t *x) {
  cell_t *r = closure_alloc(1);
  r->func = f;
  r->arg[0] = x;
  return r;
}

two_cells_t build12(reduce_t f, cell_t *x) {
  two_cells_t r;
  r.a = closure_alloc(2);
  r.b = dep(r.a);
  r.a->func = f;
  r.a->arg[0] = x;
  r.a->arg[1] = r.b;
  r.a->n = 1;
  r.a->out = 1;
  return r;
}

cell_t *build21(reduce_t f, cell_t *x, cell_t *y) {
  cell_t *r = closure_alloc(2);
  r->func = f;
  r->arg[0] = x;
  r->arg[1] = y;
  return r;
}

two_cells_t build22(reduce_t f, cell_t *x, cell_t *y) {
  two_cells_t r;
  r.a = closure_alloc(3);
  r.b = dep(r.a);
  r.a->func = f;
  r.a->arg[0] = x;
  r.a->arg[1] = y;
  r.a->arg[2] = r.b;
  r.a->out = 1;
  return r;
}

cell_t *build31(reduce_t f, cell_t *x, cell_t *y, cell_t *z) {
  cell_t *r;
  r = closure_alloc(4);
  r->func = f;
  r->arg[0] = x;
  r->arg[1] = y;
  r->arg[2] = z;
  return r;
}

two_cells_t build32(reduce_t f, cell_t *x, cell_t *y, cell_t *z) {
  two_cells_t r;
  r.a = closure_alloc(4);
  r.b = dep(r.a);
  r.a->func = f;
  r.a->arg[0] = x;
  r.a->arg[1] = y;
  r.a->arg[2] = z;
  r.a->arg[3] = r.b;
  r.a->out = 1;
  return r;
}

three_cells_t build23(reduce_t f, cell_t *x, cell_t *y) {
  three_cells_t r;
  r.a = closure_alloc(4);
  r.b = dep(r.a);
  r.c = dep(r.a);
  r.a->func = f;
  r.a->arg[0] = x;
  r.a->arg[1] = y;
  r.a->arg[2] = r.b;
  r.a->arg[3] = r.c;
  r.a->out = 2;
  return r;
}

three_cells_t build33(reduce_t f, cell_t *x, cell_t *y, cell_t *z) {
  three_cells_t r;
  r.a = closure_alloc(5);
  r.b = dep(r.a);
  r.c = dep(r.a);
  r.a->func = f;
  r.a->arg[0] = x;
  r.a->arg[1] = y;
  r.a->arg[2] = z;
  r.a->arg[3] = r.b;
  r.a->arg[4] = r.c;
  r.a->out = 2;
  return r;
}

intptr_t add_op(intptr_t x, intptr_t y) { return x + y; }
bool func_add(cell_t **cp, type_rep_t t) { return func_op2(cp, t, add_op); }
cell_t *build_add(cell_t *x, cell_t *y) {
  return build21(func_add, x, y);
}

intptr_t mul_op(intptr_t x, intptr_t y) { return x * y; }
bool func_mul(cell_t **cp, type_rep_t t) { return func_op2(cp, t, mul_op); }
cell_t *build_mul(cell_t *x, cell_t *y) {
  return build21(func_mul, x, y);
}

intptr_t sub_op(intptr_t x, intptr_t y) { return x - y; }
bool func_sub(cell_t **cp, type_rep_t t) { return func_op2(cp, t, sub_op); }
cell_t *build_sub(cell_t *x, cell_t *y) {
  return build21(func_sub, x, y);
}

intptr_t gt_op(intptr_t x, intptr_t y) { return x > y; }
bool func_gt(cell_t **cp, type_rep_t t) { return func_op2(cp, t, gt_op); }
cell_t *build_gt(cell_t *x, cell_t *y) {
  return build21(func_gt, x, y);
}

intptr_t gte_op(intptr_t x, intptr_t y) { return x >= y; }
bool func_gte(cell_t **cp, type_rep_t t) { return func_op2(cp, t, gte_op); }
cell_t *build_gte(cell_t *x, cell_t *y) {
  return build21(func_gte, x, y);
}

intptr_t lt_op(intptr_t x, intptr_t y) { return x < y; }
bool func_lt(cell_t **cp, type_rep_t t) { return func_op2(cp, t, lt_op); }
cell_t *build_lt(cell_t *x, cell_t *y) {
  return build21(func_lt, x, y);
}

intptr_t lte_op(intptr_t x, intptr_t y) { return x <= y; }
bool func_lte(cell_t **cp, type_rep_t t) { return func_op2(cp, t, lte_op); }
cell_t *build_lte(cell_t *x, cell_t *y) {
  return build21(func_lte, x, y);
}

intptr_t eq_op(intptr_t x, intptr_t y) { return x == y; }
bool func_eq(cell_t **cp, type_rep_t t) { return func_op2(cp, t, eq_op); }
cell_t *build_eq(cell_t *x, cell_t *y) {
  return build21(func_eq, x, y);
}

/* compose doesn't handle placeholders correctly */
/* [_] [+] . popr */
bool func_compose(cell_t **cp, type_rep_t t) {
  cell_t *const c = clear_ptr(*cp, 3);

  if(!(reduce(&c->arg[0], T_LIST) &&
       reduce(&c->arg[1], T_LIST))) goto fail;
  c->alt = closure_split(c, 2);
  if(bm_conflict(c->arg[0]->alt_set,
		 c->arg[1]->alt_set)) goto fail;
  cell_t *res = compose_nd(ref(c->arg[0]), ref(c->arg[1]));
  res->alt_set = alt_set_ref(c->arg[0]->alt_set |
			     c->arg[1]->alt_set);
  drop(res->alt);
  res->alt = c->alt;
  store_reduced(cp, res);
  return true;

 fail:
  fail(cp);
  return false;
}
cell_t *build_compose(cell_t *x, cell_t *y) {
  return build21(func_compose, x, y);
}

bool func_pushl(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!(reduce(&c->arg[1], T_LIST))) goto fail;
  c->alt = closure_split1(c, 1);
  cell_t *q = c->arg[1];
  bool rvar = is_var(q);
  cell_t *res = pushl_nd(ref(c->arg[0]), ref(q));
  if(rvar) res->type |= T_VAR;
  drop(res->alt);
  res->alt = c->alt;
  res->alt_set = alt_set_ref(c->arg[1]->alt_set);
  store_reduced(cp, res);
  return true;

  fail:
    fail(cp);
    return false;
}
cell_t *build_pushl(cell_t *x, cell_t *y) {
  return build21(func_pushl, x, y);
}

bool func_pushr(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[0], T_LIST)) goto fail;
  cell_t *res;
  c->alt = closure_split1(c, 0);
  cell_t *p = c->arg[0];
  if(!type_match(T_LIST, p)) goto fail;
  p->type |= T_LIST;
  alt_set_t alt_set = c->arg[0]->alt_set;
  alt_set_ref(alt_set);
  alt_set_drop(c->arg[0]->alt_set);

  int n = list_size(p);
  res = expand(ref(p), 1);
  memmove(res->ptr+1, res->ptr, sizeof(cell_t *)*n);
  res->ptr[0] = ref(c->arg[1]);
  drop(res->alt);
  res->alt = c->alt;
  res->alt_set = alt_set;

  store_reduced(cp, res);
  return true;

 fail:
  fail(cp);
  return false;
}
cell_t *build_pushr(cell_t *x, cell_t *y) {
  return build21(func_pushr, x, y);
}

bool func_quote(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t res = { .size = 2, .ptr = {ref(c->arg[0])} };
  store_reduced(cp, &res);
  return true;
}
cell_t *build_quote(cell_t *x) {
  return build11(func_quote, x);
}

bool func_popr(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *res;
  int elems;
  cell_t *alt = 0;
  cell_t *d = c->arg[1];
  if(!reduce(&c->arg[0], T_LIST)) goto fail;

  if(c->arg[0]->alt) {
    alt = closure_alloc(2);
    alt->out = 1;
    alt->func = func_popr;
    alt->arg[0] = ref(c->arg[0]->alt);
    alt->arg[1] = d ? dep(ref(alt)) : 0;
    c->alt = alt;
    if(d) d->alt = conc_alt(alt->arg[1], d->alt);
  }

  cell_t *p = c->arg[0];
  if(!is_list(p) ||
     list_size(p) == 0) goto fail;
  if(is_placeholder(p->ptr[0])) {
    ++p->size;
    cell_t *h = expand_inplace_dep(p->ptr[0], 1); // *** no shift here
    p->ptr[0] = h->arg[closure_in(h)] = dep(ref(h));
    p->ptr[1] = h;
  }

  if(d) {
    drop(c);
    d->func = func_id;
    d->arg[0] = ref(p->ptr[0]);
    d->arg[1] = (cell_t *)alt_set_ref(c->arg[0]->alt_set);
    c->arg[1] = p->ptr[0];
  }

  /* drop the right list element */
  res = closure_alloc(closure_args(p)-1);
  elems = list_size(res);
  res->type = T_LIST;
  int i;
  for(i = 0; i < elems; ++i)
    res->ptr[i] = ref(p->ptr[i+1]);

  res->alt_set = alt_set_ref(p->alt_set);
  res->alt = c->alt;
  store_reduced(cp, res);
  return true;

 fail:
  if(d) {
    drop(c);
    store_fail(d, d->alt);
  }
  c->arg[1] = 0;
  fail(cp);
  return false;
}
two_cells_t build_popr(cell_t *x) {
  return build12(func_popr, x);
}

bool func_alt(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  uint8_t a = new_alt_id(2);
  cell_t *r0 = id(c->arg[0]);
  r0->arg[1] = (cell_t *)bm(a, 0);
  cell_t *r1 = id(c->arg[1]);
  r1->arg[1] = (cell_t *)bm(a, 1);
  r0->alt = r1;
  store_lazy(cp, c, r0);
  return false;
}
cell_t *build_alt(cell_t *x, cell_t *y) {
  return build21(func_alt, x, y);
}

bool func_alt2(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *r0 = id(ref(c->arg[0]));
  r0->arg[1] = 0;
  cell_t *r1 = id(ref(c->arg[1]));
  r1->arg[1] = 0;
  r0->alt = r1;
  *cp = r0;
  drop(c);
  return false;
}
cell_t *build_alt2(cell_t *x, cell_t *y) {
  return build21(func_alt2, x, y);
}

bool func_assert(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[1], T_INT)) goto fail;
  cell_t *p = c->arg[1];
  if(!((type_match(T_INT, p) && p->val[0]) ||
       is_var(p))) goto fail;
  if(!reduce(&c->arg[0], t)) goto fail;
  c->alt = closure_split(c, 2);
  if(bm_conflict(c->arg[0]->alt_set,
		 c->arg[1]->alt_set)) goto fail;
  store_reduced(cp, mod_alt(ref(c->arg[0]), c->alt,
			    c->arg[0]->alt_set |
			    c->arg[1]->alt_set));
  return true;
 fail:
  fail(cp);
  return false;
}
cell_t *build_assert(cell_t *x, cell_t *y) {
  return build21(func_assert, x, y);
}

/*
bool type_check(cell_t **cp, type_t type) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[0], type)) goto fail;
  c->alt = closure_split1(c, 0);
  cell_t *p = get(c->arg[0]);
  if(!((p->type == type) ||
       is_var(p))) goto fail;
  store_reduced(c, mod_alt(c->arg[0], c->alt,
			   c->arg[0]->alt_set));
  return true;
 fail:
  fail(cp);
  return false;
}
*/
bool func_id(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  alt_set_t alt_set = (alt_set_t)c->arg[1];
  if(alt_set || c->alt) {
    if(!reduce(&c->arg[0], t)) goto fail;
    c->alt = closure_split1(c, 0);
    cell_t *p = c->arg[0];
    if(p->alt) alt_set_ref(alt_set);
    if(bm_conflict(alt_set, c->arg[0]->alt_set)) goto fail;
    store_reduced(cp, mod_alt(ref(p), c->alt, alt_set | p->alt_set));
    alt_set_drop(alt_set);
    return true;
  } else {
    cell_t *p = ref(c->arg[0]);
    drop(c);
    *cp = p;
    return false;
  }

 fail:
  fail(cp);
  return false;
}
cell_t *build_id(cell_t *x) {
  return build11(func_id, x);
}

bool func_drop(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *p = ref(c->arg[0]);
  drop(c);
  *cp = p;
  return false;
}

bool func_swap(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d = c->arg[2];
  c->func = func_id;
  if(d) {
    drop(c);
    d->func = func_id;
    d->arg[0] = c->arg[0];
    d->arg[1] = 0;
  } else drop(c->arg[0]);
  cell_t *q = c->arg[0] = c->arg[1];
  c->arg[1] = c->arg[2] = 0;
  *cp = ref(q);
  drop(c);
  return false;
}

cell_t *id(cell_t *c) {
  cell_t *i = func(func_id, 1, 1);
  arg(&i, c);
  return i;
}

bool func_dup(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d = c->arg[1];
  if(d) store_lazy_dep(c, d, ref(c->arg[0]));
  store_lazy(cp, c, c->arg[0]);
  return false;
}

void trace_expand_select(cell_t *c, cell_t *x, type_t t) {
  while(reduce(&x, t)) {
    if(!is_var(x)) trace(x, 0, tt_force);
    trace(c, x, tt_select);
    if(!is_var(x)) break;
    x = x->alt;
  }
}

bool func_cut(cell_t **cp, type_rep_t t) {
  cell_t *c = *cp;
  if(!reduce(&c->arg[0], t)) goto fail;
  cell_t *p = c->arg[0];
  bool v = is_var(p);
  cell_t *alt = p->alt;
  p->alt = 0;
  store_reduced(cp, ref(p));
  if(v) trace_expand_select(c, alt, t);
  drop(alt);
  return true;

 fail:
  fail(cp);
  return false;
}
cell_t *build_cut(cell_t *x) {
  return build11(func_cut, x);
}

/* args w/alts not handled correctly, */
/* should probably cut them, or hide select */
bool func_select(cell_t **cp, type_rep_t t) {
  cell_t *c = *cp;
  cell_t *p;
  if(reduce(&c->arg[0], t)) {
    p = c->arg[0];
  } else if(reduce(&c->arg[1], t)) {
    drop(c->arg[0]);
    p = c->arg[1];
    c->func = func_id;
    c->size = 1;
    c->arg[0] = p;
  } else goto fail;

  store_reduced(cp, ref(p));
  return true;

 fail:
  fail(cp);
  return false;
}
cell_t *build_select(cell_t *x, cell_t *y) {
  return build21(func_select, x, y);
}

cell_t *build(char *str, unsigned int n);

bool peg_func(cell_t **cp, int N_IN, int N_OUT,
	      char *src, size_t size) {
  cell_t *c = clear_ptr(*cp, 3);   
  cell_t *d[N_OUT-1];
  int i;
  FOREACH(d, i) {
    d[i] = c->arg[N_IN+N_OUT-2-i];
  }
  cell_t *b = build(src, size);
  cell_t *p = b->ptr[N_OUT-1];

  i = N_IN;
  while(i--) arg(&p, c->arg[i]);

  closure_shrink(c, 1);
  c->func = func_id;
  c->arg[0] = ref(p);
  c->arg[1] = 0;
  c->arg[2] = 0;
  FOREACH(d, i) {
    if(d[i]) {
      drop(c);
      d[i]->func = func_id;
      d[i]->arg[0] = ref(b->ptr[i]);
      d[i]->arg[1] = 0;
    }
  }
  drop(b);
  return false;
}

bool peg_func2(cell_t **cp, int N_IN, int N_OUT, cell_t *f) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d[N_OUT-1];
  int i;
  FOREACH(d, i) {
    d[i] = c->arg[N_IN+N_OUT-2-i];
  }

  cell_t *b = f;
  cell_t *p = b->ptr[N_OUT-1];

  i = N_IN;
  while(i--) arg(&p, c->arg[i]);

  closure_shrink(c, 1);
  c->func = func_id;
  c->arg[0] = ref(b->ptr[N_OUT-1]);
  c->arg[1] = 0;
  c->arg[2] = 0;
  FOREACH(d, i) {
    if(d[i]) {
      drop(c);
      d[i]->func = func_id;
      d[i]->arg[0] = ref(b->ptr[i]);
      d[i]->arg[1] = 0;
    }
  }
  drop(b);
  return false;
}

bool func_ift(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[0], T_INT) ||
     c->arg[0]->val[0] > 1) {
    fail(cp);
    return false;
  }
  if(is_var(c->arg[0])) {
    drop(c->arg[0]); // need to add assertions
    cell_t *res = id(c->arg[1]);
    res->alt = id(c->arg[2]);
    store_lazy(cp, c, res); // ***
  } else if(c->arg[0]->val[0]) {
    drop(c->arg[0]);
    drop(c->arg[2]);
    store_lazy(cp, c, c->arg[1]);
  } else {
    drop(c->arg[0]);
    drop(c->arg[1]);
    store_lazy(cp, c, c->arg[2]);
  }
  return false;
}
cell_t *build_ift(cell_t *x, cell_t *y, cell_t *z) {
  return build31(func_ift, x, y, z);
}

#if 0
bool func_fib(cell_t **cp, type_rep_t t) {
  char src[] = "dup 2 < "
    "[1 swap drop] "
    "[dup 1- fib swap 2- fib +] "
    "ifte pushl head";
    /*
    "[dup 2 >= ! swap force dup 1- fib "
    "swap 2- fib + force swap drop] "
    "[dup 2 < ! force drop] "
    "| pushl head cut";
    */
  return peg_func(cp, 1, 1, src, sizeof(src));
}
#endif
