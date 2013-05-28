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
    cell_t res = { .type = T_INT };		\
    bool s = reduce(c->arg[0]) &		\
      reduce(c->arg[1]);			\
    res.alt = closure_split(c, 2);		\
    s &= !bm_conflict(c->arg[0]->alt_set,	\
		      c->arg[1]->alt_set);	\
    res.alt_set = c->arg[0]->alt_set |		\
      c->arg[1]->alt_set;			\
    if(s) {					\
      res.val_size = min(c->arg[0]->val_size,	\
                         c->arg[1]->val_size);	\
      int i;					\
      for(i = 0; i < res.val_size; ++i)		\
	res.val[0] = c->arg[0]->val[i] __op__	\
	  c->arg[1]->val[i];			\
      res.val_size = 1;				\
    }						\
    deref(c->arg[0]);				\
    deref(c->arg[1]);				\
    return to_ref(c, &res, 1, s);		\
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
  if(s) {
    res = compose(c->arg[0], c->arg[1]);
    res_n = closure_cells(res);
  } else {
    res = alloca_cells(res_n = 1);
  }
  res->alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  res->alt = alt;
  to_ref(c, res, res_n, s);
  if(s) closure_free(res);
  else deref(c->arg[1]);
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
    deref(c->arg[1]);
    s = reduce(n);
    //copy_val(&res, n);
    deref(n);
  } else {
    deref(c->arg[0]); // ***
    deref(c->arg[1]);
  }
  return to_ref(c, &res, 1, s);  
}
*/
bool func_pushl(cell_t *c) {
  bool s = reduce(c->arg[1]);
  cell_t *alt = closure_split1(c, 1);
  int res_n;
  cell_t *res;
  if(s) {
    res = pushl(c->arg[0], c->arg[1]);
    res_n = closure_cells(res);
    res->alt = alt;
    to_ref(c, res, res_n, true);
    closure_free(res);
    return true;
  } else {
    res = alloca_cells(res_n = 1);
    res->alt = alt;
    deref(c->arg[0]);
    deref(c->arg[1]);
    return to_ref(c, res, res_n, false);
  }
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
  } else deref(q);
  deref(p);
  return to_ref(c, &res, 1, s);
}
*/
bool func_quote(cell_t *c) {
  cell_t res = { .ptr = {c->arg[0]} };
  res.alt = closure_split1(c, 0);
  return to_ref(c, &res, 1, true);
}

bool func_popr(cell_t *c) {
  cell_t *res, *res_other;
  int res_n, res_other_n, elems;
  bool s = true, sh;
  cell_t *head = c->arg[0];
  cell_t *tail = c->arg[1];
  if(!(sh = reduce(head)) | !reduce(head->ptr[0]) ||
     bm_conflict(head->alt_set,
		 head->ptr[0]->alt_set)) {
    s = false;
    res_n = res_other_n = 1;
    res = alloca_cells(1);
    res_other = alloca_cells(1);
  } else {
    /* copy right list element into other */
    res_other = alloca_copy(head->ptr[0], res_other_n);
    res_other->alt_set |= head->alt_set;

    /* drop the right list element */
    elems = list_size(head) - 1;
    res_n = calculate_list_size(elems);
    res = alloca_cells(res_n);
    int i;
    for(i = 0; i < elems; ++i)
      res->ptr[i] = ref(head->ptr[i+1]);

    res->alt_set = res_other->alt_set;
  }

  if(sh && head->ptr[0]->alt) {
    cell_t *q = quote(ref(head->ptr[0]->alt));
    head->alt = conc_alt(q, head->alt);
    q->alt_set = head->alt_set;
  }

  if(head->alt) {
    cell_t *alt;
    alt = closure_alloc(2);
    alt->func = func_popr;
    alt->arg[0] = ref(head->alt);
    alt->arg[1] = dep(alt);
    res->alt = ref(alt);
    res_other->alt = ref(alt->arg[1]);
  }

  deref(head);
  deref(tail);
  deref(c); /* dump ref from tail to c */
  to_ref(tail, res_other, res_other_n, s);
  return to_ref(c, res, res_n, s);
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
  res = alloca_copy_if(p, res_n, s);
  res->alt_set = p->alt_set | bm(id, c->arg[1] ? 0 : 1);
  if(p->alt) {
    res->alt = closure_alloc(2);
    res->alt->func = func_alt;
    res->alt->arg[0] = ref(p->alt);
    res->alt->arg[1] = c->arg[1];
    res->alt->arg[2] = c->arg[2];
  } else if (c->arg[1]) {
    res->alt = closure_alloc(2);
    res->alt->func = func_alt;
    res->alt->arg[0] = c->arg[1];
    res->alt->arg[1] = 0;
    res->alt->arg[2] = c->arg[2];
  }
  deref(p);
  return to_ref(c, res, res_n, s);
}

bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]);
  s &= c->arg[0]->val[0] != 0;
  int res_n;
  cell_t *res = alloca_copy_if(c->arg[0], res_n, s);
  res->alt = closure_split1(c, 0);
  deref(c->arg[0]);
  return to_ref(c, res, res_n, s);
}

bool func_id(cell_t *c) {
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  int n = closure_cells(p);
  to_ref(c, ref_all(p), n, s);
  deref(p);
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
  deref(c->arg[1]);
  to_ref(c, ref_all(res), closure_cells(c->arg[0]), s);
  deref(c->arg[0]);
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
  deref(c);
  deref(other);
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
  int n = closure_cells(p);
  to_ref(c->arg[1], ref_all(p), n, s);
  deref(c->arg[1]);
  to_ref(c, ref_all(p), n, s);
  deref(p);
  deref(c);
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
  to_ref(other, n, closure_cells(n), s);
  to_ref(c, next, closure_cells(next), s);
  deref(n);
  deref(next);
  deref(c);
  deref(other);
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

  to_ref(other2, n, closure_cells(n), s);
  to_ref(other1, n2, closure_cells(n2), s);
  to_ref(c, n3, closure_cells(n3), s);

  deref(n);
  deref(n2);
  deref(n3);

  deref(c);
  deref(other1);
  deref(other2);
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
  to_ref(other, n, closure_cells(n), s);
  to_ref(c, next, closure_cells(next), s);
  deref(n);
  deref(next);
  deref(c);
  deref(other);
  return s;
}
