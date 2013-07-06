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
#include <alloca.h>
#include "rt_types.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "alloca_cells.h"

   /*-----------------------------------------------*
    *          VARIABLE NAME CONVENTIONS            *
    *-----------------------------------------------*
    *                                               *
    *  cell_t *c = closure being reduced            *
    *  cell_t *d = first dep                        *
    *    (second closure returned, lower on stack)  *
    *  cell_t *e = second dep                       *
    *  cell_t *f = ...                              *
    *  cell_t *p = arg[0], leftmost arg             *
    *  cell_t *q = arg[1]                           *
    *  bool s = success                             *
    *  cell_t *res = result to be stored in c       *
    *  cell_t *res_X = result to be stored in X     *
    *                                               *
    *-----------------------------------------------*/

/* must be in ascending order */
word_entry_t word_table[24] = {
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
  {"dip11", func_dip11, 3, 2},
  {"dip12", func_dip12, 3, 3},
  {"dip21", func_dip21, 4, 2},
  {"drop", func_drop, 2, 1},
  {"dup", func_dup, 1, 2},
  {"force", func_force, 2, 2},
  {"id", func_id, 1, 1},
  {"ifte", func_ifte, 3, 1},
  {"popr", func_popr, 1, 2},
  {"pushl", func_pushl, 2, 1},
  {"pushr", func_pushr, 2, 1},
  {"swap", func_swap, 2, 2},
  {"|", func_alt, 2, 1}
};

#define FUNC_OP2(__op__)			\
  do {						\
    cell_t *c = *cp;				\
    bool s = reduce(&c->arg[0]) &		\
      reduce(&c->arg[1]);			\
    cell_t *res;				\
    cell_t *alt = closure_split(c, 2);		\
    s &= !bm_conflict(c->arg[0]->alt_set,	\
		      c->arg[1]->alt_set);	\
    cell_t *p = get(c->arg[0]);			\
    cell_t *q = get(c->arg[1]);			\
    if(s) {					\
      int val_size = min(p->val_size,		\
                         q->val_size);		\
      res = vector(val_size);			\
      res->type = T_INT;			\
      res->func = func_reduced;			\
      int i;					\
      for(i = 0; i < val_size; ++i)		\
	res->val[i] = p->val[i] __op__		\
	  q->val[i];				\
      res->val_size = val_size;			\
    } else {					\
      res = alloca_cells(1);			\
    }						\
    res->alt_set = c->arg[0]->alt_set |		\
      c->arg[1]->alt_set;			\
    res->alt = alt;				\
    drop(c->arg[0]);				\
    drop(c->arg[1]);				\
    store_reduced(c, res, s);			\
    return s ? r_success : r_fail;		\
  } while(0)

result_t func_add(cell_t **cp) { FUNC_OP2(+); }
result_t func_mul(cell_t **cp) { FUNC_OP2(*); }
result_t func_sub(cell_t **cp) { FUNC_OP2(-); }
result_t func_gt(cell_t **cp) { FUNC_OP2(>); }
result_t func_gte(cell_t **cp) { FUNC_OP2(>=); }
result_t func_lt(cell_t **cp) { FUNC_OP2(<); }
result_t func_lte(cell_t **cp) { FUNC_OP2(<=); }
result_t func_eq(cell_t **cp) { FUNC_OP2(==); }


result_t func_compose(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *res, *alt;
  bool s = reduce(&c->arg[0]) & reduce(&c->arg[1]);
  alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  cell_t *p = get(c->arg[0]), *q = get(c->arg[1]);
  if(s) {
    res = compose_nd(ref(p), ref(q));
  } else {
    res = alloca_cells(1);
  }
  drop(c->arg[0]);
  drop(c->arg[1]);
  res->alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  res->alt = alt;
  store_reduced(c, res, s);
  return s ? r_success : r_fail;
}

result_t func_pushl(cell_t **cp) {
  cell_t *c = *cp;
  bool s = reduce(&c->arg[1]);
  cell_t *alt = closure_split1(c, 1);
  cell_t *res;
  if(s) {
    res = pushl_nd(ref(get(c->arg[0])), ref(get(c->arg[1])));
  } else {
    res = alloca_cells(1);
  }
  drop(c->arg[0]);
  drop(c->arg[1]);
  res->alt = alt;
  store_reduced(c, res, s);
  return s ? r_success : r_fail;
}

result_t func_pushr(cell_t **cp) {
  cell_t *c = *cp;
  bool s = reduce(&c->arg[0]);
  cell_t *res;
  cell_t *alt = closure_split1(c, 0);
  cell_t *p = get_(c->arg[0]);
  cell_t *q = get_(c->arg[1]);
  alt_set_t alt_set = c->arg[0]->alt_set;
  if(s) {
    int n = list_size(p);
    res = expand(p, 1);
    memmove(res->ptr+1, res->ptr, sizeof(cell_t *)*n);
    res->ptr[0] = q;
  } else {
    res = alloca_cells(1);
    drop(p);
    drop(q);
  }
  res->alt = alt;
  res->alt_set = alt_set;
  store_reduced(c, res, s);
  return s ? r_success : r_fail;
}

result_t func_quote(cell_t **cp) {
  cell_t *c = *cp;
  cell_t res[2] = {{ .ptr = {c->arg[0]} }};
  store_reduced(c, res, true);
  return r_success;
}

cell_t *collapse_id(cell_t *c) {
  cell_t *p = c;
  while(p->func == func_id) p = p->arg[0];
  return p;
}

result_t func_popr(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *res, *res_d;
  int res_n, elems;
  cell_t *p;
  cell_t *d = c->arg[1];
  bool s = reduce(&c->arg[0]) &&
    list_size(p = get(c->arg[0])) > 0;
  if(s) {
    d->func = func_id;
    d->arg[0] = ref(p->ptr[0]);
    d->arg[1] = (cell_t *)p->alt_set;

    /* drop the right list element */
    elems = list_size(p) - 1;
    res_n = calculate_list_size(elems);
    res = closure_alloc_cells(res_n);
    int i;
    for(i = 0; i < elems; ++i)
      res->ptr[i] = ref(p->ptr[i+1]);

    res->alt_set = p->alt_set;
  } else {
    res = alloca_cells(1);
    res_d = alloca_cells(1);
    store_reduced(d, res_d, s);
  }
  /*
  if(sp && pr && pr->alt) {
    cell_t *w = quote(ref(pr->alt));
    p->alt = conc_alt(w, p->alt);
    w->alt_set = p->alt_set;
  }
  */
  if(c->arg[0]->alt) {
    cell_t *alt;
    alt = closure_alloc(2);
    alt->func = func_popr;
    alt->arg[0] = ref(c->arg[0]->alt);
    alt->arg[1] = dep(alt);
    res->alt = ref(alt);
    d->alt = conc_alt(ref(alt->arg[1]), d->alt);
  }

  drop(c->arg[0]);
  store_reduced(c, res, s);
  if(d->n) drop(c);
  drop(d);
  return s ? r_success : r_fail;
}

bool is_alt(cell_t *c) {
  return c->func == func_alt;
}

cell_t *alt() {
  cell_t *c = func(func_alt, 2);
  c->arg[2] = (cell_t *)(intptr_t)alt_cnt++;
  return c;
}

result_t func_alt(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *res;
  bool s = reduce(&c->arg[0]);
  cell_t *p = c->arg[0];
  uint8_t id = (intptr_t)c->arg[2];
  res = alloca_copy_if(get(p), s);
  res->alt_set = p->alt_set | bm(id, c->arg[1] ? 0 : 1);
  if(p->alt) {
    res->alt = closure_alloc(3);
    res->alt->func = func_alt;
    res->alt->arg[0] = ref(p->alt);
    res->alt->arg[1] = c->arg[1];
    res->alt->arg[2] = c->arg[2];
  } else if (c->arg[1]) {
    res->alt = closure_alloc(3);
    res->alt->func = func_alt;
    res->alt->arg[0] = c->arg[1];
    res->alt->arg[1] = 0;
    res->alt->arg[2] = c->arg[2];
  }
  store_reduced(c, res, s);
  drop(p);
  return s ? r_success : r_fail;
}

result_t func_assert(cell_t **cp) {
  cell_t *c = *cp;
  bool s = reduce(&c->arg[0]);
  cell_t *p = get(c->arg[0]);
  s &= p->type == T_INT && p->val[0] != 0;
  cell_t *res = alloca_copy_if(c->arg[0], s);
  res->alt = closure_split1(c, 0);
  drop(c->arg[0]);
  store_reduced(c, res, s);
  return s ? r_success : r_fail;
}

/* this function is problematic */
cell_t *get_(cell_t *c) {
  cell_t *p = ref(get(c));
  drop(c);
  return p;
}

result_t func_id(cell_t **cp) {
  cell_t *c = *cp;
  alt_set_t alt_set = (alt_set_t)c->arg[1];
  if(alt_set) {
    bool s = reduce(&c->arg[0]) &&
      !bm_conflict(alt_set, c->arg[0]->alt_set);
    cell_t *p = c->arg[0];
    store_reduced(c, mod_alt(p, conc_alt(c->alt, ref(p->alt)), alt_set | p->alt_set), s);
    return s ? r_success : r_fail;
  } else {
    cell_t *p = ref(c->arg[0]);
    drop(c);
    *cp = p;
    return r_retry;
  }
}

result_t func_force(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *d = c->arg[2];
  bool s = reduce(&c->arg[0]) & reduce(&c->arg[1]);
  cell_t *alt = closure_split(c, 2);
  cell_t *d_alt = alt ? alt->arg[2] = dep(alt) : 0;
  cell_t *p = c->arg[0], *q = c->arg[1];
  s &= !bm_conflict(p->alt_set, q->alt_set);
  alt_set_t alt_set = p->alt_set | q->alt_set;
  store_reduced(c, mod_alt(p, ref(alt), alt_set), s);
  store_reduced(d, mod_alt(q, ref(d_alt), alt_set), s);
  if(d->n) drop(c);
  drop(d);
  return s ? r_success : r_fail;
}

result_t func_drop(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *p = ref(c->arg[0]);
  drop(c);
  *cp = p;
  return r_retry;
}

result_t func_swap(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *d = c->arg[2];
  /* if d->arg[0] != c, then d is being reduced */
  bool reduce_d = d->n && d->arg[0] != c;
  c->func = func_id;
  if(d->n) {
    d->func = func_id;
    d->arg[0] = c->arg[0];
    d->arg[1] = 0;
  } else drop(c->arg[0]);
  cell_t *q = c->arg[0] = c->arg[1];
  c->arg[1] = c->arg[2] = 0;
  *cp = ref(q);
  drop(c);
  if(d->n) drop(c);
  drop(d);
  return reduce_d ? r_success : r_retry;
}

cell_t *id(cell_t *c) {
  cell_t *i = func(func_id, 1);
  arg(i, c);
  return i;
}

result_t func_dup(cell_t **cp) {
  cell_t *c = *cp;
  cell_t *d = c->arg[1];
  bool s = reduce(&c->arg[0]);
  cell_t *p = get_(c->arg[0]);
  store_reduced(d, ref(p), s);
  store_reduced(c, p, s);
  if(d->n) drop(c);
  drop(d);
  return s ? r_success : r_fail;
}

cell_t *build(char *str, unsigned int n);
#define BUILD(x) build((x), sizeof(x))

result_t func_ifte(cell_t **cp) {
  cell_t *c = *cp;
  bool s;
  char code[] = "[] pushl pushl swap pushr"
    "[0 == ! force drop swap force drop]"
    "[1 == ! force drop force drop] | . popr swap drop";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[0]);
  drop(b);
  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  s = reduce(&p);
  store_reduced(c, p, s);
  return s ? r_success : r_fail;
}

result_t func_dip11(cell_t **cp) {
  cell_t *c = *cp;
  bool s = true;
  cell_t *d = c->arg[3];
  char code[] = "swap pushr pushl popr "
    "swap popr swap drop swap";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[1]);
  cell_t *q = ref(b->ptr[0]);
  drop(b);

  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  
  s &= reduce(&p);
  s &= reduce(&q);
  store_reduced(c, p, s);
  store_reduced(d, q, s);
  if(d->n) drop(c);
  drop(d);
  return s ? r_success : r_fail;
}

result_t func_dip12(cell_t **cp) {
  cell_t *c = *cp;
  bool s = true;
  cell_t *other2 = c->arg[3];
  cell_t *other1 = c->arg[4];
  char code[] = "swap pushr pushl popr swap pushl"
    "[swap] . popr swap popr swap popr swap drop swap";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[2]);
  cell_t *q = ref(b->ptr[1]);
  cell_t *r = ref(b->ptr[0]);
  drop(b);
  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  
  s &= reduce(&p);
  s &= reduce(&q);
  s &= reduce(&r);

  store_reduced(c, p, s);
  store_reduced(other1, q, s);
  store_reduced(other2, r, s);

  drop(c);
  drop(c);
  drop(other1);
  drop(other2);
  return s ? r_success : r_fail;
}

result_t func_dip21(cell_t **cp) {
  cell_t *c = *cp;
  bool s = true;
  cell_t *other = c->arg[4];
  char code[] = "swap pushr pushl pushl popr "
    "swap popr swap drop swap";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[1]);
  cell_t *q = ref(b->ptr[0]);
  drop(b);
  arg(p, c->arg[3]);
  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  
  s &= reduce(&p);
  s &= reduce(&q);

  store_reduced(c, p, s);
  store_reduced(other, q, s);

  drop(c);
  drop(other);
  return s ? r_success : r_fail;
}
