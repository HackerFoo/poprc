/* Copyright 2012 Dustin DeWeese
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

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "rt_types.h"
#include "rt.h"

// make sure &cells > 255
cell_t cells[1<<16];
cell_t *cells_ptr;
uint8_t alt_cnt = 0;

// #define CHECK_CYCLE

#define show(x) printf(#x " = %d\n", (int)(x))
#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))
#define FOREACH(a, i) for(i = 0; i < LENGTH(a); i++)

bool is_cell(void *p) {
  return p >= (void *)&cells && p < (void *)(&cells+1);
}

bool is_closure(void *p) {
  return is_cell(p) && !is_cell(((cell_t *)p)->func);
}

bool closure_is_ready(cell_t *c) {
  assert(is_closure(c));
  return !((intptr_t)c->func & 0x1);
}

void closure_set_ready(cell_t *c, bool r) {
  assert(is_closure(c));
  c->func = (reduce_t *)(((intptr_t)c->func & ~0x1) | !r);
}

/* propagate alternatives down to root of expression tree */
/* [TODO] distribute splitting into args */
/* or combine reduce and split, so each arg is reduced */
/* just before split, and alts are stored then zeroed */
cell_t *closure_split(cell_t *c, unsigned int s) {
  cell_t *split(cell_t *c, cell_t *t, unsigned int mask, unsigned int s) {
    unsigned int i, j;
    cell_t *p = t, *cn;

    for(i = mask;
	i != 0;
	i = (i - 1) & mask) {
      cn = closure_alloc(s);
      cn->func = c->func;
      //cn->n = c->n;
      for(j = 0; j < s; j++) {
	cn->arg[j] = (1<<j) & i ?
	  c->arg[j]->alt :
	  mark_ptr(c->arg[j]);
      }
      cn->alt = p;
      p = cn;
    }
    return p;
  }

  assert(is_closure(c) && closure_is_ready(c));
  unsigned int i, n = 0;
  unsigned int alt_mask = 0;

  cell_t *p = c->alt;

  /* calculate a mask for args with alts */
  i = s;
  alt_mask = 0;
  while(i--) {
    alt_mask <<= 1;
    if(is_marked(c->arg[i])) {
      /* clear marks as we go */
      c->arg[i] = clear_ptr(c->arg[i]);
    } else if(c->arg[i] &&
	      c->arg[i]->alt) {
      alt_mask |= 1;
      n++;
    }
  }

  if(n == 0) return p;

  p = split(c, c->alt, alt_mask, s);

  for(i = 0; i < s; i++) {
    if((1<<i) & alt_mask) {
      if(c->arg[i]->n > c->arg[i]->alt->n)
        c->arg[i]->alt->n = c->arg[i]->n;
      c->arg[i]->n += (1 << (n-1)) - 1;
      c->arg[i]->alt->n += (1 << (n-1)) - 1;
    } else {
      c->arg[i]->n += (1 << n) - 1;
    }
  }

  return p;
}

cell_t *closure_split1(cell_t *c, int n) {
  cell_t *a = 0;
  if(c->arg[n]->alt) {
    a = copy(c);
    a->arg[n] = 0;
    ref_args(a);
    a->arg[n] = c->arg[n]->alt;
  }
  c->arg[n] = clear_ptr(c->arg[n]);
  return a;
}

bool reduce(cell_t *c) {
  c = clear_ptr(c);
  if(c) {
    assert(is_closure(c) &&
	   closure_is_ready(c));
    return c->func(c);
  } else return false;
}

cell_t *cells_next() {
  cell_t *p = cells_ptr;
  assert(is_cell(p) && !is_closure(p) && is_cell(cells_ptr->next));
  cells_ptr = cells_ptr->next;
  return p;
}

#ifdef CHECK_CYCLE
bool check_cycle() {
  int i = 0;
  cell_t *start = cells_ptr, *ptr = start;
  while(ptr->next != start) {
    if(i > LENGTH(cells)) return false;
    i++;
    assert(is_cell(ptr->next->next));
    ptr = ptr->next;
  }
  return true;
}
#else
bool check_cycle() {
  return true;
}
#endif

void cells_init() {
  int i;
  
  // zero the cells
  bzero(&cells, sizeof(cells));

  // set up doubly-linked pointer ring
  for(i = 0; i < LENGTH(cells); i++) {
    cells[i].prev = &cells[i-1];
    cells[i].next = &cells[i+1];
  }
  cells[0].prev = &cells[LENGTH(cells)-1];
  cells[LENGTH(cells)-1].next = &cells[0];

  cells_ptr = &cells[0];
  assert(check_cycle());
  alt_cnt = 0;
}

int alloc_cnt = 0;

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  cell_t *prev = c->prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  prev->next = next;
  next->prev = prev;
  alloc_cnt++;
  assert(check_cycle());
}

cell_t *closure_alloc(int args) {
  return closure_alloc_cells(calculate_cells(args));
}

cell_t *closure_alloc_cells(int size) {
  cell_t *ptr = cells_next(), *c = ptr;
  cell_t *mark = ptr;
  int cnt = 0;

  // search for contiguous chunk
  while(cnt < size) {
    if(is_cell(ptr) && !is_closure(ptr)) {
      cnt++;
      ptr++;
    } else {
      cnt = 0;
      c = ptr = cells_next();
      assert(c != mark);
    }
  }

  // remove the found chunk
  int i;
  for(i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  bzero(c, sizeof(cell_t)*size);
  assert(check_cycle());
  return c;
}

int calculate_cells(int args) {
  return 1 + ((sizeof(cell_t *) * (args - LENGTH(((cell_t *)0)->arg)) + sizeof(cell_t) - 1) / sizeof(cell_t));
}

int closure_cells(cell_t *c) {
  return calculate_cells(closure_args(c));
}

void closure_shrink(cell_t *c, int s) {
  int i, size = closure_cells(c);
  if(size > s) {
    assert(is_closure(c));
    for(i = s; i < size; i++) {
      c[i].prev = &c[i-1];
      c[i].next = &c[i+1];
    }
    c[s].prev = cells_ptr->prev;
    cells_ptr->prev->next = &c[s];
    c[size-1].next = cells_ptr;
    cells_ptr->prev = &c[size-1];
    assert(check_cycle());
  }
}

void closure_free(cell_t *c) {
  closure_shrink(c, 0);
}

bool func_reduced(cell_t *c) {
  assert(is_closure(c));
  return c->type != T_FAIL;
}

cell_t *val(intptr_t x) {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_INT;
  c->val = x;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_PTR;
  c->ptr = x;
  return c;
}

bool is_reduced(cell_t *c) {
  return c->func == func_reduced;
}

// max offset is 255
bool is_offset(cell_t *c) {
  return !((intptr_t)c & ~0xff);
}

// args must be >= 1
cell_t *func(reduce_t *f, int args) {
  assert(args >= 0);
  cell_t *c = closure_alloc(args);
  assert(c->func == 0);
  c->func = f;
  c->arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, false);
  return c;
}

bool func_add(cell_t *c) {
  cell_t res = { .type = T_INT };
  bool s = reduce(c->arg[0]) &&
    reduce(c->arg[1]);
  res.alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  res.alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  res.val = s ? c->arg[0]->val + c->arg[1]->val : 0;
  deref(c->arg[0]);
  deref(c->arg[1]);
  return to_ref(c, &res, s);
}

int closure_args(cell_t *c) {
  assert(is_closure(c));
  cell_t **p = c->arg;
  int n = 0;
  if(c->func == func_alt) return 2;
  if(is_reduced(c)) {
    /* skip over value data */
    p += LENGTH(c->arg);
    n += LENGTH(c->arg);
  } else if(is_offset(*p)) {
    intptr_t o = (intptr_t)(*p) + 1;
    p += o;
    n += o;
  }
  /* args must be cells */
  while(is_closure(*p)) {
    p++;
    n++;
  }
  return n;
}

int closure_next_child(cell_t *c) {
  assert(is_closure(c));
  return is_offset(c->arg[0]) ? (intptr_t)c->arg[0] : 0;
}

void arg(cell_t *c, cell_t *a) {
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  int i = closure_next_child(c);
  if(!is_cell(c->arg[i])) {
    c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0) closure_set_ready(c, closure_is_ready(a));
  } else {
    arg(c->arg[i], a);
    if(closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)c->arg; // decrement offset
    }
  }
}

cell_t *copy(cell_t *c) {
  int size = closure_cells(c);
  cell_t *new = closure_alloc_cells(size);
  memcpy(new, c, size * sizeof(cell_t));
  return new;
}

cell_t *ref_args(cell_t *c) {
  int n = closure_args(c);
  while(n--)
    if(is_closure(c->arg[n]))
      c->arg[n] = ref(c->arg[n]);
  return c;
}

bool to_ref(cell_t *c, cell_t *r, bool s) {
  closure_shrink(c, 1);
  r->n = c->n;
  if(r->type == T_PTR) refn(r->ptr, c->n);
  memcpy(c, r, sizeof(cell_t));
  c->func = func_reduced;
  if(!s) c->type = T_FAIL;
  if(c->alt) c->alt->n = c->n;
  return s;
}

cell_t *next(cell_t *c) {
  assert(is_reduced(c));
  if(!c->next) return 0;
  else return refn(c->next, c->n - 1);
} 

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, int n) {
  if(!c || n <= 0) return c;
  assert(is_closure(c));
  c->n += n;
  if(is_reduced(c)) {
    refn(c->next, n);
    if(c->type == T_PTR)
      refn(c->ptr, n);
  }
  return c;
}

cell_t *dup(cell_t *c) {
  assert(is_closure(c));
  if(closure_is_ready(c)) return ref(c);
  int args = closure_args(c);
  cell_t *tmp = copy(c);
  /* c->arg[<i] are empty and c->arg[>i] are ready, *
   * so no need to copy                             *
   * c->arg[i] only needs copied if filled          */
  int i = closure_next_child(c);
  if(is_closure(c->arg[i])) tmp->arg[i] = dup(c->arg[i]);
  while(++i < args) tmp->arg[i] = ref(c->arg[i]);
  return tmp;
}

bool is_nil(cell_t *c) {
  return !c;
}

void *clear_ptr(void *p) {
  return (void *)((intptr_t)p & ~1);
}

void *mark_ptr(void *p) {
  return (void *)((intptr_t)p | 1);
}

bool is_marked(void *p) {
  return (intptr_t)p & 1;
}

void deref(cell_t *c) {
  //return;
  if(c) {
    assert(is_closure(c));
    //printf("DEREF(%d) to %d\n", (int)(c - &cells[0]), c->n);
    if(!c->n) closure_free(c);
    else --c->n;
  }
}

void drop(cell_t *c) {
  if(!is_closure(c)) return;
  if(!c->n) {
    if(is_reduced(c)) {
      if(c->type == T_PTR)
	drop(c->ptr);
    } else {
      int i = closure_args(c) - 1;
      while(i--) drop(c->arg[i]);
    }
    closure_free(c);
  } else --c->n;
}

cell_t *pushl(cell_t *a, cell_t *b) {
  if(b == 0) {
    return a;
  }
  assert(is_closure(a) && is_closure(b));
  if(closure_is_ready(b)) {
    cell_t *n = closure_alloc(2);
    n->func = func_concat;
    n->arg[0] = a;
    n->arg[1] = b;
    closure_set_ready(n, closure_is_ready(a));
    return n;
  } else {
    arg(b, a);
    return b;
  }
}

bool func_append(cell_t *c) {
  cell_t res = { .type = T_PTR };
  bool s = reduce(c->arg[0]) && reduce(c->arg[1]);
  res.alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  res.alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  if(s) {
    res.ptr = closure_alloc(2);
    res.ptr->func = func_concat;
    res.ptr->arg[0] = c->arg[0]->ptr;
    res.ptr->arg[1] = c->arg[1]->ptr;
  }
  deref(c->arg[0]);
  deref(c->arg[1]);
  return to_ref(c, &res, s);
}

bool func_pushl(cell_t *c) {
  cell_t res = { .type = T_PTR };
  cell_t *p = c->arg[1];
  bool s = reduce(p);
  res.alt = closure_split1(c, 1);
  res.alt_set = p->alt_set;
  res.ptr = s ? pushl(c->arg[0], p->ptr) : NULL;
  deref(p);
  return to_ref(c, &res, s);
}

bool func_quote(cell_t *c) {
  cell_t res = { .type = T_PTR,
		 .ptr = c->arg[0] };
  return to_ref(c, &res, true);
}

#undef MK_APPEND
#define MK_APPEND(fname, field)			\
  cell_t *fname(cell_t *a, cell_t *b) {		\
    if(!a) return b;				\
    if(!b) return a;				\
						\
    cell_t *p = a, *r;				\
    while((r = p->field)) p = r;		\
    p->field = b;				\
    return a;					\
  }

MK_APPEND(conc_alt, alt);
/* append is destructive to a! */
MK_APPEND(append, next);

bool func_popr(cell_t *c) {
  cell_t res = { .val = 0 };
  bool s = true;
  cell_t *p = c->arg[0];
  if(!reduce(p)) {
    deref(p);
    s = false;
  } else if(!reduce(p->ptr)) {
    deref(p->ptr);
    deref(p);
    s = false;
  } else {
    res.type = p->ptr->type;
    res.val = p->ptr->val;
    res.alt_set = p->alt_set | p->ptr->alt_set;
    s &= !bm_conflict(p->alt_set,
		      p->ptr->alt_set);
    if(p->ptr->alt)
      conc_alt(p, quote(p->ptr->alt));
    res.next = quote(p->ptr->next);
    deref(p->ptr);
    deref(p);
  }
  res.alt = closure_split1(c, 0);
  return to_ref(c, &res, s);
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
  cell_t res = { .val = 0 };
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  uint8_t id = (intptr_t)c->arg[2];
  res.alt_set = p->alt_set | bm(id, c->arg[1] ? 0 : 1);
  //assert((alt_set & ~0x0000000300000003) == 0);
  if(p->alt) {
    res.alt = closure_alloc(2);
    res.alt->func = func_alt;
    res.alt->arg[0] = p->alt;
    res.alt->arg[1] = c->arg[1];
    res.alt->arg[2] = c->arg[2];
  } else if (c->arg[1]) {
    res.alt = closure_alloc(2);
    res.alt->func = func_alt;
    res.alt->arg[0] = c->arg[1];
    res.alt->arg[1] = 0;
    res.alt->arg[2] = c->arg[2];
  }
  res.type = p->type;
  res.val = p->val;
  res.next = p->next;
  deref(p);
  return to_ref(c, &res, s);
}

bool func_concat(cell_t *c) {
  cell_t res = { .val = 0 };
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  res.alt_set = p->alt_set;
  res.alt = closure_split1(c, 0);
  if(p->next) {
    res.next = closure_alloc(2);
    res.next->func = func_concat;
    res.next->n = c->n;
    res.next->arg[0] = p->next;
    res.next->arg[1] = c->arg[1];
  } else {
    res.next = refn(c->arg[1], c->n);
  }
  res.type = p->type;
  res.val = p->val;
  deref(p);
  return to_ref(c, &res, s);
}

cell_t *compose(cell_t *a, cell_t *b) {
  assert(is_closure(a) && is_closure(b));
  if(is_nil(a)) return b;
  if(is_nil(b)) return a;
  cell_t *h = a->arg[0];
  cell_t *bp = compose(a->arg[1], b);
  return pushl(h, bp);
}

bool func_compose(cell_t *c) {
  c->ptr = compose(c->arg[0], c->arg[1]);
  return true;
}

bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]);
  cell_t res = { .val = c->arg[0]->val,
		 .type = c->arg[0]->type };
  res.alt = closure_split1(c, 0);
  deref(c->arg[0]);
  return to_ref(c, &res, s && res.val);
}

#define BM_SIZE (sizeof(intptr_t) * 4)

intptr_t bm(int k, int v) {
  assert(k < BM_SIZE);
  return ((intptr_t)1 << (k + BM_SIZE)) |
    (((intptr_t)v & 1) << k);
}

intptr_t bm_intersect(intptr_t a, intptr_t b) {
  return a & b;
}
  
intptr_t bm_union(intptr_t a, intptr_t b) {
  return a | b;
}
  
intptr_t bm_conflict(intptr_t a, intptr_t b) {
  return ((a & b) >> BM_SIZE) &
    ((a ^ b) & (((intptr_t)1<<BM_SIZE)-1));
}

void alloc_test() {
  int i, j;
  cell_t *a[30];
  for(j = 0; j < 50; j++) {
    for(i = 0; i < LENGTH(a); i++) {
      a[i] = func(func_add, 9);
    }
    for(i = 0; i < LENGTH(a); i++) {
      closure_free(a[i]);
    }
  }
}

#define addr(x) printf("addr(" #x ") = %d\n", (int)(x - &cells[0]))

void test0() {
  cell_t *a, *b, *c, *d, *e, *f, *g, *h, *i, *j;

  g = func(func_add, 2);
  e = func(func_add, 2);
  c = func(func_add, 2);

  h = val(10);
  b = val(2);

  a = val(1);

  arg(e, c);
  arg(e, a);
  arg(e, b);
  //show(closure_args(e));
  f = dup(e);
  d = val(4);
  arg(e, d);
  arg(f, dup(a));
  arg(g, h);
  arg(g, f);

  /* leakiness below */
  i = func(func_quote, 1);
  arg(i, g);
  j = func(func_popr, 1);
  arg(j, i);

  show_eval(e);
  show_eval(j);
}

void print_sexpr_help(cell_t *);

void print_list_help(cell_t *c) {
  if(is_nil(c)) {
    printf(" ] ");
    return;
  }
  // assert(is_cons(c));
  print_sexpr_help(c->arg[0]);
  print_list_help(c->arg[1]); 
}

void print_list(cell_t *c) {
  printf(" [");
  print_list_help(c);
}

void print_sexpr_help(cell_t *r) {
  if(!is_closure(r)) {
    printf(" ?");
  } else if(is_reduced(r)) {
    if(r->type == T_INT) {
      printf(" %d", (int)r->val);
    } else if(r->type == T_PTR) {
      printf(" [");
      print_sexpr_help(r->ptr);
      printf(" ]");
    } else if(r->type == T_FAIL) {
      printf(" (FAIL)");
    }
  } else {

    intptr_t f = ((intptr_t)r->func) & ~1;
    char *n;
    int i;
    int args = closure_args(r);

# define CASE(x) if(f == (intptr_t)func_##x) n = #x

    CASE(add);
    CASE(pushl);
    CASE(popr);
    CASE(assert);
    CASE(alt);
    CASE(quote);
    CASE(append);
    CASE(id);

# undef CASE

    printf(" (%s", n);

    for(i = 0; i < args; i++) {
      print_sexpr_help(r->arg[i]);
    }

    printf(")");
  }
}

void print_sexpr(cell_t *r) {
  print_sexpr_help(r);
  printf("\n");
}

void show_eval(cell_t *c) {
  printf("[");
  show_alt(c);
  printf(" ]\n");
}

void show_one(cell_t *c) {
  if(c->type == T_PTR) {
    printf(" [");
    show_alt(c->ptr);
    printf(" ]");
  } else {
    printf(" %d", (int)c->val);
  }
  deref(c);
}

void show_list(cell_t *c) {
  cell_t *n = c->next;
  show_one(c);
  if(n) show_alt(n);
}

void show_alt(cell_t *c) {
  cell_t *p = c, *t, *a;
  /* skip initial failures */
  if(!p) return;
  while(p && !reduce(p)) {
    t = p;
    p = p->alt;
    deref(t);
  }
  if(p) {
    a = p->alt;
    if(!a) {
      /* one */
      show_list(p);
    } else {
      /* many */
      printf(" {");
      show_list(p);
      while(a) {
	if(reduce(a)) {
	  printf(" |");
	  t = a->alt;
	  show_list(a);
	} else { 
	  t = a->alt;
	  deref(a);
	}
	a = t;
      }
      printf(" }");
    }
  } else {
    /* none */
    printf(" {}");
  }
}

void test1() {
  cell_t *a, *b, *c, *d, *e, *z, *y;
  cell_t *a_, *b_, *e_;
  // 1 [2 +] pushl popr

  a = func(func_add, 2);
  e = val(2);
  arg(a, e);
  b = quote(a);

  a_ = func(func_add, 2);
  e_ = alt();
  arg(e_, val(3));
  arg(e_, val(7));
  y = alt();
  arg(y, e_);
  arg(y, val(20));
  arg(a_, y);
  b_ = quote(a_);
 
  z = alt();
  arg(z, b_);
  arg(z, b);

  c = func(func_pushl, 2);
  arg(c, z);
  arg(c, val(1));

  d = func(func_popr, 1);
  arg(d, c);

  show_eval(d);
}

void test2() {
  cell_t *a, *b, *c, *d, *e, *f, *g, *h, *i, *k, *l, *m, *n;

  a = func(func_add, 2);
  b = val(2);
  //e = func(func_assert, 1);
  //arg(e, val(0));
  e = val(1);
  k = alt();
  arg(k, b);
  arg(k, e);
  c = val(20);
  d = val(10);
  l = alt();
  arg(l, c);
  arg(l, d);
  arg(a, k);
  arg(a, l);
  // a = (1 | 2) + (10 | 20)

  f = func(func_add, 2);
  g = val(300);
  h = val(200);
  i = val(100);
  m = alt();
  arg(m, g);
  arg(m, h);
  n = alt();
  arg(n, i);
  arg(n, m);

  arg(f, n);
  arg(f, a);

  // f = (100 | 200 | 300) + a
  show_eval(ref(a));
  show_eval(ref(n));
  show_eval(f);
}

void test3() {
  cell_t *a, *b, *c, *d;
  a = val(2);
  b = val(5);
  c = func(func_add, 2);
  arg(c, a);
  arg(c, b);
  d = ref_args(copy(c));

  show_eval(c);
  show_eval(d);
}

void test4() {
  cell_t *a, *b, *c, *d, *e, *f, *g;

  a = val(1);
  b = val(2);
  
  c = alt();
  arg(c, a);
  arg(c, b);
      
  d = val(10);
  f = alt();
  arg(f, val(20));
  arg(f, val(30));
  e = alt();
  arg(e, d);
  arg(e, f);

  g = func(func_add, 2);
  arg(g, c);
  arg(g, e);

  show_eval(g);
}

bool func_id(cell_t *c) {
  cell_t *p = c->arg[0];
  if(reduce(p)) {
    cell_t *a = closure_split1(c, 0);
    c->func = func_reduced;
    c->alt_set = p->alt_set;
    c->type = p->type;
    c->val = p->val;
    c->next = p->next;
    c->alt = a;
    deref(p);
    return true;
  } else {
    deref(p);
    return false;
  }
}

void test5() {
  cell_t *p = func(func_id, 1);

# define V(x) arg(p, val(x));
# define P arg(p, func(func_add, 2));

  // 5 40 + 300 + 2000 10000 + +
  // + -> P, x -> V(x), reverse order
  P P V(10000) V(2000) P V(300) P V(40) V(5) 

  show_eval(p);
}

void test6() {
  cell_t *a, *b, *c, *d, *e;
  a = alt();
  arg(a, val(2));
  arg(a, val(1));
  b = alt();
  arg(b, val(20));
  arg(b, val(10));
  c = func(func_add, 2);
  arg(c, b);
  arg(c, a);

  e = alt();
  arg(e, val(100));
  arg(e, val(200));
  d = func(func_add, 2);
  arg(d, c);
  arg(d, e);

  show_eval(d);
}

void test7() {
  cell_t *a, *b;
  a = alt();
  arg(a, val(1));
  arg(a, val(2));

  b = func(func_add, 2);
  arg(b, a);
  arg(b, val(3));

  show_eval(b);
}

void test8() {
  cell_t *a, *b, *c, *d, *e;
  c = alt();
  arg(c, val(42));
  arg(c, val(51));
  a = func(func_quote, 1);
  arg(a, c);
  e = func(func_quote, 1);
  arg(e, val(123));
  d = alt();
  arg(d, a);
  arg(d, e);
  b = func(func_popr, 1);
  arg(b, d);
  
  show_eval(b);
}

void test9() {
  cell_t *a, *b, *c, *d, *e, *f, *g;

  a = func(func_append, 2);
  arg(a, quote(val(7)));
  arg(a, quote(val(6)));
  b = func(func_append, 2);
  arg(b, quote(val(5)));
  arg(b, a);
  //b = a;
  c = func(func_pushl, 2);
  arg(c, b);
  arg(c, val(4));

  g = func(func_pushl, 2);
  arg(g, ref(b));
  arg(g, val(3));

  d = func(func_add, 2);
  arg(d, val(2));
  e = func(func_pushl, 2);
  arg(e, quote(d));
  arg(e, val(1));

  f = func(func_popr, 1);
  arg(f, c);

  show_eval(f);
  show_eval(g);
  show_eval(e);
}

void test10() {
  cell_t *a = quote(val(42));
  cell_t *b = ref(a);
  cell_t *c = func(func_popr, 1);
  arg(c, a);
  cell_t *d = func(func_popr, 1);
  arg(d, b);

  show_eval(c);
  show_eval(d);
}

void test11() {
  cell_t *a = func(func_append, 2);
  arg(a, quote(val(2)));
  arg(a, quote(val(1)));
  cell_t *b = func(func_popr, 1);
  arg(b, a);
  show_eval(b);
  //show_eval(a);
}

cell_t *id(cell_t *c) {
  cell_t *i = func(func_id, 1);
  arg(i, c);
  return i;
}

void test12() {
  cell_t *a = alt();
  arg(a, val(2));
  arg(a, val(1));

  cell_t *b = func(func_add, 2);
  arg(b, ref(a));
  arg(b, a);
 
  cell_t *e = alt();
  arg(e, val(10));
  arg(e, val(20));

  cell_t *d = func(func_add, 2);
  arg(d, b);
  arg(d, e);

  show_eval(d);
}

void test13() {
  cell_t *a = alt();
  arg(a, quote(val(1)));
  arg(a, quote(val(2)));
  show_eval(a);
}

void test14() {
  intptr_t a = 0, b = 0, c = 0;
  a = bm(1,0) | bm(2, 1) | bm(3, 0);
  b = bm(1,0) | bm(2, 0) | bm(4, 1);
  c = bm(2,1) | bm(4, 0) | bm(3, 0);

  show(bm_conflict(a, b));
  show(bm_conflict(b, c));
  show(bm_conflict(c, a));
}

void test15() {
  char table[] =
    "alligator SNAP   "
    "cat       MEOW   "
    "dog       WOOF   "
    "fish      SPLASH "
    "goat      NOOO   "
    "horse     NEIGH  "
    "mouse     EEK    "
    "parrot    SQUAWK "
    "rat       SCUTTLE"
    "zebra     ???    ";
  char *e = lookup(table, 17, sizeof(table)/17, "rat");
  char x[18] = "no match";
  if(e) strncpy(x, e, 17);
  printf("result = \"%s\"\n", x);
}

void test16() {
  char str[] = "1 2 add";
  /*
  cell_t *a = word("1");
  cell_t *b = word("2");
  cell_t *c = word("add");
  arg(c, a);
  arg(c, b);
  show_eval(c);
  */
  eval(str, sizeof(str));
}

void check_free() {
  int i;
  for(i = 1; i < LENGTH(cells); i++) {
    if(is_closure(&cells[i])) {
      printf("LEAK: %d ", i);
      print_sexpr(&cells[i]);
    }
  }
}

void (*tests[])(void) = {
  test0, test1, test2, test3,
  test4, test5, test6, test7,
  test8, test9, test10, test11,
  test12, test13, test14, test15,
  test16
};

int main(int argc, char *argv[]) {
  unsigned int test_number;
  if(argc != 2) return -1;
  if(strcmp("all", argv[1]) == 0) {
    int i;
    FOREACH(tests, i) {
      printf("_________________________________"
	     "(( test %-3d ))"
	     "_________________________________\n", i);
      cells_init();
      tests[i]();
      check_free();
    }
  } else if(strcmp("eval", argv[1]) == 0) {
    char s[1024];
    bzero(s, sizeof(s));
    if(fgets(s, sizeof(s), stdin)) {
      cells_init();
      eval(s, sizeof(s));
      check_free();
    }
  } else {
    test_number = atoi(argv[1]);
    if(test_number >= LENGTH(tests)) return -2;
    cells_init();
    //alloc_test();
    tests[test_number]();
    check_free();
  }
  printf("allocated %ld bytes\n", alloc_cnt * sizeof(cell_t));
  return 0;
}

void *lookup(void *table, unsigned int width, unsigned int rows, char *key) {
  unsigned int low = 0, high = rows, pivot;
  int c;
  void *entry;
  int key_length = strnlen(key, width);
  while(high > low) {
    pivot = low + ((high - low) >> 1);
    entry = table + width * pivot;
    c = strncmp(key, entry, key_length);
    if(c == 0) return entry;
    if(c < 0) high = pivot;
    else low = pivot + 1;
  }
  return 0;
}

bool is_num(char *str) {
  return str[0] >= '0' && str[0] <= '9';
}

word_entry_t word_table[] = {
  {"!", func_assert, 1, 1},
  {"'", func_quote, 1, 1},
  {"+", func_add, 2, 1},
  {"++", func_append, 2, 1},
  {".", func_compose, 2, 1},
  {"id", func_id, 1, 1},
  {"popr", func_popr, 1, 2},
  {"pushl", func_pushl, 2, 1},
  {"|", func_alt, 2, 1}
};

cell_t *word(char *w) {
  unsigned int in, out;
  return word_parse(w, &in, &out);
}

cell_t *word_parse(char *w,
		   unsigned int *in,
		   unsigned int *out) {
  cell_t *c;
  if(is_num(w)) {
    c = val(atoi(w));
    *in = 0;
    *out = 1;
  } else {
    word_entry_t *e =
      lookup(word_table,
	     WIDTH(word_table),
	     LENGTH(word_table),
	     w);
    if(!e) return NULL;
    c = func(e->func, e->in);
    if(e->func == func_alt)
      c->arg[2] = (cell_t *)(intptr_t)alt_cnt++;
    *in = e->in;
    *out = e->out;
  }
  return c;
}

char *rtok(char *str, char *p) {
  if(--p < str) return NULL;

  /* remove trailing spaces */
  while(*p == ' ') {
    if(p > str) p--;
    else return NULL;
  }

  /* move to start of token */
  while(*p != ' ') {
    if(p > str) p--;
    else return p;
  }

  /* put a null before the token */
  if(p >= str) *p = '\0';

  return ++p;
}

char *parse_word(char *str, char *p, cell_t **r) {
  unsigned int in = 0, out = 0;
  char *tok = rtok(str, p);
  if(!tok) return false;
  cell_t *c = word_parse(tok, &in, &out);
  *r = pushl(c, *r);
  return tok;
}

cell_t *build(char *str, char *end) {
  cell_t *r = 0;
  char *p = end+1;
  while((p = parse_word(str, p-1, &r)));
  return r;
}

void eval(char *str, int n) {
  char *p = str;
  while(*p != '\0' && n--) {
    if(*p == '\n') {
      *p = '\0';
      break;
    }
    p++;
  }
  cell_t *c = build(str, p);
  show_eval(c);
}
