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

/* must be in ascending order */
word_entry_t word_table[24] = {
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
    bool s = reduce(c->arg[0]) &		\
      reduce(c->arg[1]);			\
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
    unref(c->arg[0]);				\
    unref(c->arg[1]);				\
    return store_reduced(c, res, s);		\
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
  bool s = reduce(c->arg[0]) & reduce(c->arg[1]);
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
  cell_t *res;
  if(s) {
    res = pushl_nd(ref(get(c->arg[0])), ref(get(c->arg[1])));
  } else {
    res = alloca_cells(1);
  }
  unref(c->arg[0]);
  drop(c->arg[1]);
  res->alt = alt;
  return store_reduced(c, res, s);
}

bool func_pushr(cell_t *c) {
  bool s = reduce(c->arg[0]);
  cell_t *res;
  cell_t *alt = closure_split1(c, 0);
  cell_t *p = get_(c->arg[0]);
  cell_t *q = get_(c->arg[1]);
  uintptr_t alt_set = c->arg[0]->alt_set;
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
  return store_reduced(c, res, s);
}

bool func_quote(cell_t *c) {
  cell_t res[2] = {{ .ptr = {c->arg[0]} }};
  return store_reduced(c, res, true);
}

bool func_popr(cell_t *c) {
  cell_t *res0, *res1;
  int res0_n, elems;
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
    res1 = alloca_cells(1);
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
    res0 = alloca_cells(1);
    res1 = alloca_cells(1);
  }

  if(sp && p->ptr[0]->alt) {
    cell_t *w = quote(ref(get(p->ptr[0])));
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

  drop(c->arg[0]);
  unref(q);
  unref(c); /* dump ref from tail to c */
  store_reduced(q, res1, s);
  return store_reduced(c, res0, s);
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
  cell_t *p = c->arg[0];
  bool s = reduce(p);
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
  unref(p);
  return s;
}

/*
bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]) & reduce(c->arg[1]);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  cell_t *alt = closure_split(c, 2);
  uintptr_t alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  cell_t *p = get(c->arg[1]);
  s &= p->type == T_INT && p->val[0] != 0;
  unref(c->arg[1]);
  cell_t *res = mod_alt(c->arg[0], alt, alt_set);
  return store_reduced(c, res, s);
}
*/

bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]);
  cell_t *p = get(c->arg[0]);
  s &= p->type == T_INT && p->val[0] != 0;
  cell_t *res = alloca_copy_if(c->arg[0], s);
  res->alt = closure_split1(c, 0);
  unref(c->arg[0]);
  return store_reduced(c, res, s);
}

/* this function is problematic */
cell_t *get_(cell_t *c) {
  cell_t *p = ref(get(c));
  unref(c);
  return p;
}

bool func_id(cell_t *c) {
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  store_reduced(c, p, s);
  return s;
}

bool func_force(cell_t *c) {
  cell_t *other = c->arg[2];
  bool s = reduce(c->arg[0]) & reduce(c->arg[1]);
  cell_t *alt = closure_split(c, 2);
  cell_t *other_alt = alt ? dep(alt) : 0;
  cell_t *p = c->arg[0], *q = c->arg[1];
  s &= !bm_conflict(p->alt_set, q->alt_set);
  uintptr_t alt_set = p->alt_set | q->alt_set;
  store_reduced(c, mod_alt(p, alt, alt_set), s);
  store_reduced(other, mod_alt(q, other_alt, alt_set), s);
  unref(c);
  unref(other);
  return s;
}

bool func_drop(cell_t *c) {
  cell_t *res;
  bool s = reduce(c->arg[0]) &
    reduce(c->arg[1]);
  cell_t *alt = closure_split(c, 2);
  if(s) s = !bm_conflict(c->arg[0]->alt_set,
			 c->arg[1]->alt_set);
  res = alloca_copy_if(c->arg[0], s);
  res->alt_set = c->arg[0]->alt_set |
    c->arg[1]->alt_set;
  res->alt = alt;
  drop(c->arg[1]);
  cell_t *t = c->arg[0];
  store_reduced(c, res, s);
  drop(t);
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
  unref(c->arg[1]);
  store_reduced(c->arg[1], ref(p), s);
  unref(c);
  store_reduced(c, p, s);
  return s;
}

cell_t *build(char *str, unsigned int n);
#define BUILD(x) build((x), sizeof(x))

bool func_ifte(cell_t *c) {
  bool s;
  char code[] = "[] pushl pushl swap pushr"
    "[0 == ! drop swap drop]"
    "[1 == ! drop drop] | . popr swap drop";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[0]);
  drop(b);
  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  s = reduce(p);
  return store_reduced(c, p, s);
}

bool func_dip11(cell_t *c) {
  bool s = true;
  cell_t *other = c->arg[3];
  char code[] = "swap pushr pushl popr "
    "swap popr swap drop swap";
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[1]);
  cell_t *q = ref(b->ptr[0]);
  drop(b);

  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  
  s &= reduce(p);
  s &= reduce(q);
  store_reduced(c, p, s);
  store_reduced(other, q, s);
  unref(c);
  unref(other);
  return s;
}

bool func_dip12(cell_t *c) {
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
  
  s &= reduce(p);
  s &= reduce(q);
  s &= reduce(r);

  store_reduced(c, p, s);
  store_reduced(other1, q, s);
  store_reduced(other2, r, s);

  unref(c);
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
  cell_t *b = BUILD(code);
  cell_t *p = ref(b->ptr[1]);
  cell_t *q = ref(b->ptr[0]);
  drop(b);
  arg(p, c->arg[3]);
  arg(p, c->arg[2]);
  arg(p, c->arg[1]);
  arg(p, c->arg[0]);
  
  s &= reduce(p);
  s &= reduce(q);

  store_reduced(c, p, s);
  store_reduced(other, q, s);

  unref(c);
  unref(other);
  return s;
}
