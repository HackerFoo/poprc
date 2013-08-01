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
word_entry_t word_table[28] = {
  {"!", func_assert, 1, 1},
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
  {"cut", func_cut, 1, 1},
  {"dip11", func_dip11, 3, 2},
  {"dip12", func_dip12, 3, 3},
  {"dip21", func_dip21, 4, 2},
  {"drop", func_drop, 2, 1},
  {"dup", func_dup, 1, 2},
  {"fib", func_fib, 1, 1},
  {"force", func_force, 2, 2},
  {"head", func_head, 1, 1},
  {"id", func_id, 1, 1},
  {"ifte", func_ifte, 3, 1},
  {"popr", func_popr, 1, 2},
  {"pushl", func_pushl, 2, 1},
  {"pushr", func_pushr, 2, 1},
  {"swap", func_swap, 2, 2},
  {"|", func_alt, 2, 1},
  {"||", func_alt2, 2, 1}
};

bool func_op2(cell_t **cp, intptr_t (*op)(intptr_t, intptr_t)) {
  cell_t *res;
  cell_t *c = clear_ptr(*cp, 3);
  if(!(reduce(&c->arg[0]) &&
       reduce(&c->arg[1]))) goto fail;
  c->alt = closure_split(c, 2);
  if(bm_conflict(c->arg[0]->alt_set,
		 c->arg[1]->alt_set)) goto fail;
  cell_t *p = get(c->arg[0]);
  cell_t *q = get(c->arg[1]);
  if(!(p->type == T_INT || p->type == T_VAR) ||
     !(q->type == T_INT || q->type == T_VAR)) goto fail;
  if(p->type == T_VAR ||
     q->type == T_VAR) {
    res = var();
    if(!is_var(p)) trace_store(p);
    if(!is_var(q)) trace_store(q);
  } else {
    int val_size = min(p->val_size,
		       q->val_size);
    res = vector(val_size);
    res->type = T_INT;
    res->func = func_reduced;
    int i;
    for(i = 0; i < val_size; ++i)
      res->val[i] = op(p->val[i], q->val[i]);
    res->val_size = val_size;
  }
  res->alt_set =
    alt_set_ref(c->arg[0]->alt_set |
		c->arg[1]->alt_set);
  res->alt = c->alt;
  drop(c->arg[0]);
  drop(c->arg[1]);
  store_reduced(c, res);
  return true;

 fail:
  fail(cp);
  return false;
}

intptr_t add_op(intptr_t x, intptr_t y) { return x + y; }
bool func_add(cell_t **cp) { return func_op2(cp, add_op); }

intptr_t mul_op(intptr_t x, intptr_t y) { return x * y; }
bool func_mul(cell_t **cp) { return func_op2(cp, mul_op); }

intptr_t sub_op(intptr_t x, intptr_t y) { return x - y; }
bool func_sub(cell_t **cp) { return func_op2(cp, sub_op); }

intptr_t gt_op(intptr_t x, intptr_t y) { return x > y; }
bool func_gt(cell_t **cp) { return func_op2(cp, gt_op); }

intptr_t gte_op(intptr_t x, intptr_t y) { return x >= y; }
bool func_gte(cell_t **cp) { return func_op2(cp, gte_op); }

intptr_t lt_op(intptr_t x, intptr_t y) { return x < y; }
bool func_lt(cell_t **cp) { return func_op2(cp, lt_op); }

intptr_t lte_op(intptr_t x, intptr_t y) { return x <= y; }
bool func_lte(cell_t **cp) { return func_op2(cp, lte_op); }

intptr_t eq_op(intptr_t x, intptr_t y) { return x == y; }
bool func_eq(cell_t **cp) { return func_op2(cp, eq_op); }

bool func_compose(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *res;
  if(!(reduce(&c->arg[0]) && reduce(&c->arg[1]))) goto fail;
  c->alt = closure_split(c, 2);
  if(bm_conflict(c->arg[0]->alt_set,
		 c->arg[1]->alt_set)) goto fail;
  cell_t *p = get(c->arg[0]), *q = get(c->arg[1]);
  if(!is_list(p) || !is_list(q)) goto fail;
  res = compose_nd(ref(p), ref(q));
  res->alt_set =
    alt_set_ref(c->arg[0]->alt_set |
		c->arg[1]->alt_set);
  res->alt = c->alt;
  drop(c->arg[0]);
  drop(c->arg[1]);
  store_reduced(c, res);
  return true;

 fail:
  fail(cp);
  return false;
}

bool func_pushl(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!(reduce(&c->arg[1]))) goto fail;
  c->alt = closure_split1(c, 1);
  cell_t *q = get(c->arg[1]);
  if(!(is_list(q) || is_var(q))) goto fail;
  cell_t *p = get(c->arg[0]);
  cell_t *res;
  if(is_var(q)) {
    if(!is_var(p)) trace_store(p);
    res = var();
  } else {
    res = pushl_nd(ref(p), ref(q));
    drop(res->alt);
  }
  res->alt = c->alt;
  res->alt_set = alt_set_ref(c->arg[1]->alt_set);
  drop(c->arg[0]);
  drop(c->arg[1]);
  store_reduced(c, res);
  return true;

  fail:
    fail(cp);
    return false;
}

bool func_pushr(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[0])) goto fail;
  cell_t *res;
  c->alt = closure_split1(c, 0);
  cell_t *p = ref(get(c->arg[0]));
  drop(c->arg[0]);
  cell_t *q = c->arg[1];
  alt_set_t alt_set = c->arg[0]->alt_set;
  alt_set_ref(alt_set);
  alt_set_drop(c->arg[0]->alt_set);
  int n = list_size(p);
  res = expand(p, 1);
  memmove(res->ptr+1, res->ptr, sizeof(cell_t *)*n);
  res->ptr[0] = q;
  res->alt = c->alt;
  res->alt_set = alt_set;
  store_reduced(c, res);
  return true;

 fail:
  fail(cp);
  return false;
}

bool func_quote(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t res[2] = {{ .ptr = {c->arg[0]} }};
  store_reduced(c, res);
  return true;
}

bool func_popr(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *res;
  int res_n, elems;
  cell_t *alt = 0;
  cell_t *d = is_hole(c->arg[1]) ? 0 : c->arg[1];
  if(!reduce(&c->arg[0])) goto fail;

  if(c->arg[0]->alt) {
    alt = closure_alloc(2);
    alt->func = func_popr;
    alt->arg[0] = ref(c->arg[0]->alt);
    alt->arg[1] = d ? dep(ref(alt)) : hole;
    c->alt = alt;
    if(d) d->alt = conc_alt(alt->arg[1], d->alt);
  }

  cell_t *p = get(c->arg[0]);
  if(!(is_list(p) &&
       list_size(p) > 0)) goto fail;

  if(d) {
    drop(c);
    d->func = func_id;
    d->arg[0] = ref(p->ptr[0]);
    d->arg[1] = (cell_t *)alt_set_ref(c->arg[0]->alt_set);
  }

  /* drop the right list element */
  elems = list_size(p) - 1;
  res_n = calculate_list_size(elems);
  res = closure_alloc_cells(res_n);
  int i;
  for(i = 0; i < elems; ++i)
    res->ptr[i] = ref(p->ptr[i+1]);

  res->alt_set = alt_set_ref(p->alt_set);
  res->alt = c->alt;
  drop(c->arg[0]);
  store_reduced(c, res);
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

bool func_alt(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  uint8_t a = new_alt_id(2);
  cell_t *r0 = id(ref(c->arg[0]));
  r0->arg[1] = (cell_t *)bm(a, 0);
  cell_t *r1 = id(ref(c->arg[1]));
  r1->arg[1] = (cell_t *)bm(a, 1);
  r0->alt = r1;
  *cp = r0;
  drop(c);
  return false;
}

bool func_alt2(cell_t **cp) {
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

bool func_assert(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  if(!reduce(&c->arg[0])) goto fail;
  c->alt = closure_split1(c, 0);
  cell_t *p = get(c->arg[0]);
  if(!p->type == T_INT || p->val[0] == 0) goto fail;
  store_reduced(c, mod_alt(c->arg[0], c->alt,
			   c->arg[0]->alt_set));
  return true;
 fail:
  fail(cp);
  return false;
}

bool func_id(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  alt_set_t alt_set = (alt_set_t)c->arg[1];
  if(alt_set || c->alt) {
    if(!reduce(&c->arg[0])) goto fail;
    c->alt = closure_split1(c, 0);
    cell_t *p = c->arg[0];
    if(p->alt) alt_set_ref(alt_set);
    if(bm_conflict(alt_set, c->arg[0]->alt_set)) goto fail;
    store_reduced(c, mod_alt(p, c->alt, alt_set | p->alt_set));
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

bool func_force(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d = is_hole(c->arg[2]) ? 0 : c->arg[2];
  if(!(reduce(&c->arg[0]) & reduce(&c->arg[1]))) goto fail;
  cell_t *alt = c->alt = closure_split(c, 2);
  if(alt) {
    if(d) d->alt = alt->arg[2] = dep(ref(alt));
    else alt->arg[2] = hole;
  }
  cell_t *p = c->arg[0], *q = c->arg[1];
  if(bm_conflict(p->alt_set, q->alt_set)) goto fail;
  alt_set_t alt_set = p->alt_set | q->alt_set;
  store_reduced(c, mod_alt(p, alt, alt_set));
  if(d) {
    drop(c);
    store_reduced(d, mod_alt(q, d->alt, alt_set));
  } else drop(q);
  return true;

 fail:
  if(d) {
    drop(c);
    store_fail(d, d->alt);
  }
  c->arg[2] = 0;
  fail(cp);
  return false;
}

bool func_drop(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *p = ref(c->arg[0]);
  drop(c);
  *cp = p;
  return false;
}

bool func_swap(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d = is_hole(c->arg[2]) ? 0 : c->arg[2];
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
  cell_t *i = func(func_id, 1);
  arg(i, c);
  return i;
}

bool func_dup(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *d = is_hole(c->arg[1]) ? 0 : c->arg[1];
  if(!reduce(&c->arg[0])) goto fail;
  cell_t *p = c->arg[0];
  if(d) {
    drop(c);
    store_reduced(d, ref(p));
  }
  store_reduced(c, p);
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

bool func_cut(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *p = c->arg[0];
  if(!reduce(&p)) goto fail;
  drop(p->alt);
  p->alt = 0;
  store_reduced(c, p);
  return true;

 fail:
  fail(cp);
  return false;
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
  while(i--) arg(p, c->arg[i]);

  closure_shrink(c, 1);
  c->func = func_id;
  c->arg[0] = ref(p);
  c->arg[1] = 0;
  c->arg[2] = 0;
  FOREACH(d, i) {
    if(!is_hole(d[i])) {
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

  i = N_IN;
  while(i--) {
    b = arg_nd(b->ptr[N_OUT-1], c->arg[i], b);
  }

  closure_shrink(c, 1);
  c->func = func_id;
  c->arg[0] = ref(b->ptr[N_OUT-1]);
  c->arg[1] = 0;
  c->arg[2] = 0;
  FOREACH(d, i) {
    if(!is_hole(d[i])) {
      drop(c);
      d[i]->func = func_id;
      d[i]->arg[0] = ref(b->ptr[i]);
      d[i]->arg[1] = 0;
    }
  }
  drop(b);
  return false;
}

bool func_ifte(cell_t **cp) {
  char src[] = "[] pushl pushl swap pushr"
    "[0 == ! force drop swap force drop]"
    "[1 == ! force drop force drop] || . popr cut swap drop";
  //  "[1 == ! force drop force drop] | . popr swap drop";
  return peg_func(cp, 3, 1, src, sizeof(src));
}

bool func_dip11(cell_t **cp) {
  char src[] = "swap pushr pushl popr "
    "swap popr swap drop swap";
  cell_t *f = build(src, sizeof(src));
  return peg_func2(cp, 3, 2, f);
}

bool func_dip12(cell_t **cp) {
  char src[] = "swap pushr pushl popr swap pushl"
    "[swap] . popr swap popr swap popr swap drop";
  return peg_func(cp, 3, 3, src, sizeof(src));
}

bool func_dip21(cell_t **cp) {
  char src[] = "swap pushr pushl pushl popr "
    "swap popr swap drop swap";
  return peg_func(cp, 4, 2, src, sizeof(src));
}

bool func_head(cell_t **cp) {
  char src[] = "popr swap drop";
  return peg_func(cp, 1, 1, src, sizeof(src));
}

bool func_fib(cell_t **cp) {
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
