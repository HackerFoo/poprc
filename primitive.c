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

/* must be in ascending order */
word_entry_t word_table[21] = {
  {"!", func_assert, 1, 1},
  //  {"$", func_apply, 2, 1}, // ***
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
  {"id", func_id, 1, 1},
  {"popr", func_popr, 1, 2},
  {"pushl", func_pushl, 2, 1},
  //  {"pushr", func_pushr, 2, 1},
  {"swap", func_swap, 2, 2},
  {"|", func_alt, 2, 1}
};

#define FUNC_OP2(__op__)			\
  do {						\
    bool s = reduce(c->arg[0]) &		\
      reduce(c->arg[1]);			\
    cell_t *res;				\
    int res_n;					\
    cell_t *alt = closure_split(c, 2);		\
    s &= !bm_conflict(c->arg[0]->alt_set,	\
		      c->arg[1]->alt_set);	\
    cell_t *p = get(c->arg[0]);			\
    cell_t *q = get(c->arg[1]);			\
    if(s) {					\
      int val_size = min(p->val_size,		\
                         q->val_size);		\
      res = vector(val_size);			\
      res_n = closure_cells(res);		\
      res->type = T_INT;			\
      res->func = func_reduced;			\
      int i;					\
      for(i = 0; i < val_size; ++i)		\
	res->val[i] = p->val[i] __op__		\
	  q->val[i];				\
      res->val_size = val_size;			\
    } else {					\
      res = alloca_cells(res_n = 1);		\
    }						\
    res->alt_set = c->arg[0]->alt_set |		\
      c->arg[1]->alt_set;			\
    res->alt = alt;				\
    unref(c->arg[0]);				\
    unref(c->arg[1]);				\
    return store_reduced(c, res, res_n, s);		\
  } while(0)

bool func_add(cell_t *c) { FUNC_OP2(+); }
bool func_mul(cell_t *c) { FUNC_OP2(*); }
bool func_sub(cell_t *c) { FUNC_OP2(-); }
bool func_gt(cell_t *c) { FUNC_OP2(>); }
bool func_gte(cell_t *c) { FUNC_OP2(>=); }
bool func_lt(cell_t *c) { FUNC_OP2(<); }
bool func_lte(cell_t *c) { FUNC_OP2(<=); }
bool func_eq(cell_t *c) { FUNC_OP2(==); }


bool func_compose(cell_t *c) {
  cell_t *res, *alt;
  int res_n;
  bool s = reduce(c->arg[0]) & reduce(c->arg[1]);
  alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  cell_t *p = get(c->arg[0]), *q = get(c->arg[1]);
  if(s) {
    res = compose(p, q);
    res_n = closure_cells(res);
  } else {
    res = alloca_cells(res_n = 1);
  }
  res->alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  res->alt = alt;
  store_reduced(c, res, res_n, s);
  if(!s) {
    unref(c->arg[0]);
    unref(c->arg[1]);
  }
  return s;
}
/*
// *** broken
bool func_apply(cell_t *c) {
  cell_t res = { .val = {0} };
  bool s = reduce(c->arg[1]);
  res.alt = closure_split1(c, 1);
  res.alt_set = c->arg[1]->alt_set;
  if(s) {
    cell_t *n = closure_alloc(3);
    n->func = func_concat;
    n->arg[0] = c->arg[0];
    n->arg[1] = ref(c->arg[1]->ptr[0]);
    n->arg[2] = (cell_t *)res.alt_set;
    unref(c->arg[1]);
    s = reduce(n);
    //copy_val(&res, n);
    unref(n);
  } else {
    unref(c->arg[0]); // ***
    unref(c->arg[1]);
  }
  return store_reduced(c, &res, 1, s);
}
*/
bool func_pushl(cell_t *c) {
  bool s = reduce(c->arg[1]);
  cell_t *alt = closure_split1(c, 1);
  int res_n;
  cell_t *res;
  if(s) {
    res = pushl(get(c->arg[0]), get(c->arg[1]));
    res_n = closure_cells(res);
  } else {
    res = alloca_cells(res_n = 1);
    unref(c->arg[0]);
    unref(c->arg[1]);
  }
  res->alt = alt;
  return store_reduced(c, res, res_n, s);
}

/*
bool func_pushr(cell_t *c) {
  bool s = reduce(c->arg[0]);
  cell_t *alt = closure_split1(c, 0);
  cell_t *p = c->arg[0];
  cell_t *q = c->arg[1];
  int n = list_size(p);
  p = expand_list(p, 1);
  int i;
  memmove(&p->ptr[1], &p->ptr[0], sizeof(cell_t *)*n);
  p->ptr[0] = q;
  if(s) {
    res.ptr[0] = pushl(ref(p->ptr[0]), q); // ***
    res.ptr[0]->alt_set = res.alt_set;
  } else unref(q);
  unref(p);
  return store_reduced(c, &res, 1, s);
}
*/
bool func_quote(cell_t *c) {
  cell_t res = { .ptr = {c->arg[0]} };
  return store_reduced(c, &res, 1, true);
}

bool func_popr(cell_t *c) {
  cell_t *res0, *res1;
  int res0_n, res1_n, elems;
  bool s, sp;
  cell_t *p = c->arg[0], *pr;
  cell_t *q = c->arg[1];
  if((sp = reduce(p) &&
      list_size(p = get(p)) > 0) &&
     reduce(p->ptr[0]) &&
     !bm_conflict(p->alt_set,
		  p->ptr[0]->alt_set)) {
    pr = get(p->ptr[0]);
    s = true;
    res1 = alloca_cells(res1_n = 1);
    res1->type = T_INDIRECT;
    res1->val[0] = (intptr_t)ref(pr);
    res1->alt_set = p->alt_set | pr->alt_set;

    /* drop the right list element */
    elems = list_size(p) - 1;
    res0_n = calculate_list_size(elems);
    res0 = closure_alloc_cells(res0_n);
    int i;
    for(i = 0; i < elems; ++i)
      res0->ptr[i] = ref(p->ptr[i+1]);

    res0->alt_set = res1->alt_set;
  } else {
    s = false;
    res0 = alloca_cells(res0_n = 1);
    res1 = alloca_cells(res1_n = 1);
  }

  if(sp && p->ptr[0]->alt) {
    cell_t *w = quote(ref(pr->alt));
    p->alt = conc_alt(w, p->alt);
    w->alt_set = p->alt_set;
  }

  if(p->alt) {
    cell_t *alt;
    alt = closure_alloc(2);
    alt->func = func_popr;
    alt->arg[0] = ref(p->alt);
    alt->arg[1] = dep(alt);
    res0->alt = ref(alt);
    res1->alt = ref(alt->arg[1]);
  }

  unref(p);
  unref(q);
  unref(c); /* dump ref from tail to c */
  store_reduced(q, res1, res1_n, s);
  return store_reduced(c, res0, res0_n, s);
}

bool is_alt(cell_t *c) {
  return c->func == func_alt;
}

cell_t *alt() {
  cell_t *c = func(func_alt, 2);
  c->arg[2] = (cell_t *)(intptr_t)alt_cnt++;
  return c;
}

bool func_alt(cell_t *c) {
  cell_t *res;
  int res_n;
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  uint8_t id = (intptr_t)c->arg[2];
  res = alloca_copy_if(get(p), res_n, s);
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
  unref(p);
  return store_reduced(c, res, res_n, s);
}

bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]);
  s &= c->arg[0]->val[0] != 0;
  int res_n;
  cell_t *res = alloca_copy_if(c->arg[0], res_n, s);
  res->alt = closure_split1(c, 0);
  unref(c->arg[0]);
  return store_reduced(c, res, res_n, s);
}

cell_t *get_(cell_t *c) {
  cell_t *p = ref(get(c));
  unref(c);
  return p;
}

bool func_id(cell_t *c) {
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  p = get_(p);
  int n = closure_cells(p);
  store_reduced(c, p, n, s);
  return s;
}

bool func_drop(cell_t *c) {
  cell_t *res;
  int res_n;
  bool s = reduce(c->arg[0]) &
    reduce(c->arg[1]);
  cell_t *alt = closure_split(c, 2);
  if(s) s = !bm_conflict(c->arg[0]->alt_set,
			 c->arg[1]->alt_set);
  res = alloca_copy_if(c->arg[0], res_n, s);
  res->alt_set = c->arg[0]->alt_set |
    c->arg[1]->alt_set;
  res->alt = alt;
  unref(c->arg[1]);
  cell_t *t = c->arg[0];
  store_reduced(c, ref_all(res), closure_cells(c->arg[0]), s);
  unref(t);
  return s;
}

bool func_swap(cell_t *c) {
  cell_t *other = c->arg[2];
  /* if other->arg[0] != c, then other is being reduced */
  bool reduce_other = other->arg[0] != c;
  c->func = func_id;
  other->func = func_id;
  other->arg[0] = c->arg[0];
  c->arg[0] = c->arg[1];
  c->arg[1] = 0;
  unref(c);
  unref(other);
  return reduce_other || func_id(c);
}

cell_t *id(cell_t *c) {
  cell_t *i = func(func_id, 1);
  arg(i, c);
  return i;
}

bool func_dup(cell_t *c) {
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  p = get_(p);
  int n = closure_cells(p);
  unref(c->arg[1]);
  store_reduced(c->arg[1], dup(p), n, s);
  unref(c);
  store_reduced(c, p, n, s);
  return s;
}

cell_t *build(char *str, unsigned int n);
#define BUILD(x) build((x), sizeof(x))

bool func_dip11(cell_t *c) {
  bool s = true;
  cell_t *other = c->arg[3];
  char code[] = "swap pushr pushl popr "
    "swap popr swap drop swap";
  cell_t *n = BUILD(code);
  arg(n, c->arg[2]);
  arg(n, c->arg[1]);
  arg(n, c->arg[0]);
  
  s &= reduce(n);
  cell_t *next = n->next;
  n->next = 0;
  s &= reduce(next);
  store_reduced(other, n, closure_cells(n), s);
  store_reduced(c, next, closure_cells(next), s);
  unref(n);
  unref(next);
  unref(c);
  unref(other);
  return s;
}

bool func_dip12(cell_t *c) {
  bool s = true;
  cell_t *other1 = c->arg[3];
  cell_t *other2 = c->arg[4];
  char code[] = "swap pushr pushl popr "
    "' swap . popr swap popr swap popr swap drop";
  cell_t *n = BUILD(code);
  arg(n, c->arg[2]);
  arg(n, c->arg[1]);
  arg(n, c->arg[0]);
  
  s &= reduce(n);
  cell_t *n2 = n->next;
  n->next = 0;

  s &= reduce(n2);
  cell_t *n3 = n2->next;
  n2->next = 0;

  s &= reduce(n3);

  store_reduced(other2, n, closure_cells(n), s);
  store_reduced(other1, n2, closure_cells(n2), s);
  store_reduced(c, n3, closure_cells(n3), s);

  unref(n);
  unref(n2);
  unref(n3);

  unref(c);
  unref(other1);
  unref(other2);
  return s;
}

bool func_dip21(cell_t *c) {
  bool s = true;
  cell_t *other = c->arg[4];
  char code[] = "swap pushr pushl pushl popr "
    "swap popr swap drop swap";
  cell_t *n = BUILD(code);
  arg(n, c->arg[3]);
  arg(n, c->arg[2]);
  arg(n, c->arg[1]);
  arg(n, c->arg[0]);
  
  s &= reduce(n);
  cell_t *next = n->next;
  n->next = 0;
  s &= reduce(next);
  store_reduced(other, n, closure_cells(n), s);
  store_reduced(c, next, closure_cells(next), s);
  unref(n);
  unref(next);
  unref(c);
  unref(other);
  return s;
}
