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
cell_t cells[1<<10];
cell_t *cells_ptr;
cell_t *nil = 0;

// #define CHECK_CYCLE

#define show(x) printf(#x " = %d\n", x)
#define LENGTH(_a) (sizeof(_a) / sizeof((_a)[0]))

cell_t *nul;

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
cell_t *closure_split(cell_t *c) {
  cell_t *split(cell_t *c, cell_t *t, unsigned int mask, unsigned int n, unsigned int s) {
    unsigned int i, j;
    cell_t *p = t, *cn;

    for(i = 0; i < s; i++) {
      if(mask & (1<<i)) {
	refn(c->arg[i], (1 << (n-1)) - 1);
	refn(c->arg[i]->alt, (1 << (n-1))); // ***
      } else {
	refn(c->arg[i], (1 << n) - 1);
      }
    }

    for(i = mask;
	i != 0;
	i = i - 1 & mask) {
      cn = closure_alloc(s);
      cn->func = c->func;
      for(j = 0; j < s; j++) {
	cn->arg[j] = (1<<j) & i ? c->arg[j]->alt : c->arg[j];
      }
      cn->alt = p;
      p = cn;
    }
    return p;
  }

  //printf("__closure_split:");
  //print_sexpr(c);
  assert(is_closure(c) && closure_is_ready(c));
  unsigned int i, j, s = closure_args(c);
  unsigned int alt_mask = 0;
  unsigned int alts = 0;

  cell_t *t, *p = c->alt;
  
  /* split alts in chain as needed */
  while(p) {
    unsigned int n = 0, mask = 0;
    t = p->alt;
    i = s; while(i--) {
      mask <<= 1;
      if(p->arg[i] == c->arg[i] && p->arg[i]->alt) {
	n++;
	mask |= 1;
      }
    }
    p->alt = split(p, t, mask, n, s);
    p = t;
  }

  i = s; while(i--) {
    alt_mask <<= 1;
    if(c->arg[i] != 0 && c->arg[i]->alt != 0) {
      alts++;
      alt_mask |= 1;
    }
  }

  p = split(c, c->alt, alt_mask, alts, s);

  for(i = 0; i < s; i++) {
    if(!c->arg[i]->alt) continue;
    deref(c->arg[i]->alt);
    c->arg[i]->alt = 0;
  }

  return p;
}

cell_t *closure_split1(cell_t *c, int n) {
  cell_t *a = 0;
  if(c->arg[n]->alt) {
    a = copy(c);
    a->arg[n] = c->arg[n]->alt;
  }
  return a;
}

bool __reduce(reduce_t f, cell_t *c) {
  cell_t *t, *p = c;
  //print_sexpr(c);
  while(p) {
    if(f(p)) {
      if(p != c) {
	to_ref(c, p->val, p->next, p->alt);
	deref(p);
      }
      return true;
    }
    t = p;
    p = p->alt;
    if(t != c) deref(t);
  }
  c->alt = 0;
  return false;
}

bool reduce(cell_t *c) {
  return c && c->func(c);
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

  // cells[0] is the nil cell
  //cells[0].func = func_nil;

  // set up doubly-linked pointer ring
  for(i = 0; i < LENGTH(cells); i++) {
    cells[i].prev = &cells[i-1];
    cells[i].next = &cells[i+1];
  }
  cells[0].prev = &cells[LENGTH(cells)-1];
  cells[LENGTH(cells)-1].next = &cells[0];

  cells_ptr = &cells[0];
  assert(check_cycle());
}

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  cell_t *prev = c->prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  prev->next = next;
  next->prev = prev;
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
  //printf("closure_shrink(&cells[%d], %d)\n", (unsigned int)(c - &cells[0]), s);
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
  //printf("closure_free(&cells[%d])\n", (unsigned int)(c - &cells[0]));
  closure_shrink(c, 0);
}

FUNC(nil) {
  return false;
}
/*
bool func_val(cell_t *c, void *r) {
  assert(is_closure(c));
  intptr_t val = (intptr_t)c->arg[0];
  *(intptr_t *)r = val;
  to_ref(c, val);
  return true;
}
*/
FUNC(ref_reduced) {
  assert(is_closure(c));
  return true;
}

FUNC(ref_reduced_ptr) {
  assert(is_closure(c));
  return true;
}

FUNC(ref_failed) {
  return false;
}

FUNC(ref) {
  intptr_t val;
  cell_t *r = c->ptr;
  bool ret = reduce(r);
  if(ret) {
    c->func = r->func;
    c->val = r->val;
    c->alt = r->alt;
    c->next = refn(r->next, c->n - 1);
    deref(r);
  } else {
     c->func = func_ref_failed;
  }
  return ret;
}

cell_t *val(intptr_t x) {
  cell_t *c = closure_alloc(1);
  c->func = func_ref_reduced;
  c->n = 1;
  c->val = x;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(1);
  c->func = func_ref_reduced_ptr;
  c->n = 1;
  c->ptr = x;
  return c;
}

/*
bool is_val(cell_t *c) {
  return c->func == func_val;
}
*/
bool is_ref(cell_t *c) {
  return c->func == func_ref ||
         c->func == func_ref_reduced ||
         c->func == func_ref_reduced_ptr ||
         c->func == func_ref_failed;
}

// max offset is 255
bool is_offset(cell_t *c) {
  return !((intptr_t)c & ~0xff);
}

// args must be >= 1
cell_t *func(reduce_t *f, int args) {
  assert(args >= 0);
  //int size = calculate_cells(args);
  cell_t *c = closure_alloc(args);
  assert(c->func == 0);
  c->func = f;
  c->arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, false);
  return c;
}

FUNC(add) {
  FUNC(f) {
    intptr_t z;
    bool s = reduce(c->arg[0]) && reduce(c->arg[1]);
    if(s) {
      cell_t *alt = closure_split(c);
      z = c->arg[0]->val + c->arg[1]->val;
      deref(c->arg[0]);
      deref(c->arg[1]);
      to_ref(c, z, 0, alt);
    } else {
      deref(c->arg[0]);
      deref(c->arg[1]);
    }
    return s;
  }
  return __reduce(func_f, c);
}

int closure_args(cell_t *c) {
  assert(is_closure(c));
  //if(is_val(c)) return 1;
  if(is_ref(c)) return 2;
  cell_t **p = c->arg;
  int n = 0;
  if(is_offset(*p)) {
    intptr_t o = (intptr_t)(*p) + 1;
    p += o;
    n += o;
  }
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

void to_ref(cell_t *c, intptr_t x, cell_t *n, cell_t *a) {
  if(is_ref(c)) return;
  closure_shrink(c, 1);
  c->func = func_ref_reduced;
  c->n = 1;
  c->val = x;
  c->next = n;
  c->alt = a;
}

void to_ref_ptr(cell_t *c, cell_t *x, cell_t *n, cell_t *a) {
  if(is_ref(c)) return;
  closure_shrink(c, 1);
  c->func = func_ref_reduced_ptr;
  c->n = 1;
  c->ptr = x;
  c->next = n;
  c->alt = a;
}

void to_fail(cell_t *c) {
  closure_shrink(c,1);
  c->func = func_ref_failed;
  c->n = 1;
  c->val = 0;
  c->next = 0;
  c->alt = 0;
}

cell_t *next(cell_t *c) {
  assert(is_ref(c));
  if(!c->next) return 0;
  else return refn(c->next, c->n - 1);
} 

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, int n) {
  if(!c || n <= 0) return c;
  assert(is_closure(c));
  if(is_ref(c)) {
    c->n += n;
    refn(c->next, n);
  } else {
    /* copy closure to a new location,         */
    /* then convert the closure to a reference */
    cell_t *new = copy(c);
    closure_shrink(c, 1);
    c->func = func_ref;
    c->n = n+1;
    c->ptr = new;
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
/*
bool is_cons(cell_t *c) {
  return c->func == func_cons;
}

cell_t *cons(cell_t *h, cell_t *t) {
  assert(is_nil(t) || is_cons(t));
  cell_t *c = closure_alloc(2);
  c->func = func_cons;
  c->arg[0] = h;
  c->arg[1] = t;
  return c;
}
*/
void deref(cell_t *c) {
  if(!c /*is_closure(c)*/) return;
  assert(is_closure(c));
  if(is_ref(c)) {
    //printf("DEREF(%d) to %d\n", (int)(c - &cells[0]), (int)*(intptr_t *)&c->arg[0]-1);
    if(!--c->n) {
      if(c->func == func_ref ||
	 c->func == func_ref_reduced_ptr)
	deref(c->ptr);
      closure_free(c);
    }
  } else {
    //printf("DEREF(%d) ", (int)(c - &cells[0]));
    //print_sexpr(c);
    closure_free(c);
  }
}

void drop(cell_t *c) {
  if(!is_closure(c)) return;
  if(is_ref(c)) {
    if(!--*(intptr_t *)&c->arg[0]) {
      if(c->func == func_ref)
	drop(c->ptr);
      closure_free(c);
    }
    /*
  } else if(is_val(c)) {
    closure_free(c);
    */
  } else {
    int i = closure_args(c) - 1;
    while(i--) drop(c->arg[i]);
    closure_free(c);
  }
}
/*
// to do: reduce arg
FUNC(head) {
  FUNC(f) {
    cell_t *x = c->arg[0];
    if (!is_cons(x)) return false;
    drop(x->arg[1]);
    c->ptr = x->arg[0];
    return true;
  }
  return __reduce(func_f, c);
}

FUNC(tail) {
  FUNC(f) {
    cell_t *x = c->arg[0];
    if(!is_cons(x)) return false;
    drop(x->arg[0]);
    c->ptr = x->arg[1];
    return true;
  }
  return __reduce(func_f, c);
}
*/

cell_t *pushl(cell_t *a, cell_t *b) {
  /* b is reduced, a is not */
  assert(is_closure(a) && is_closure(b));
  if(b == 0) {
    return a;
  }
  if(closure_is_ready(b)) {
    cell_t *n = closure_alloc(2);
    n->func = func_concat;
    n->arg[0] = b;
    n->arg[1] = a;
    return n;
  } else {
    arg(b, a);
    return b;
  }
}

FUNC(append) {
  FUNC(f) {
    cell_t *z;
    bool s = reduce(c->arg[0]) && reduce(c->arg[1]);
    if(s) {
      cell_t *alt = closure_split(c);
      z = closure_alloc(2);
      z->func = func_concat;
      z->arg[0] = ref(c->arg[0]->ptr);
      z->arg[1] = ref(c->arg[1]->ptr);
      deref(c->arg[0]);
      deref(c->arg[1]);
      to_ref_ptr(c, z, 0, alt);
    } else {
      deref(c->arg[0]);
      deref(c->arg[1]);
    }
    return s;
  }
  return __reduce(func_f, c);
}

FUNC(pushl) {
  cell_t *p = c->arg[1];
  bool ret = reduce(p);
  if(ret) {
    cell_t *alt = closure_split1(c, 1);
    cell_t *v = pushl(c->arg[0], ref(p->ptr));
    to_ref_ptr(c, v, 0, alt);
  }
  deref(p);
  return ret;
}

FUNC(quote) {
  to_ref_ptr(c, c->arg[0], 0, 0);
  return true;
}
/*
FUNC(cons) {
  to_ref_ptr(c, cons(c->arg[0], c->arg[1]), 0, 0);
  return true;
}
*/
#undef MK_APPEND
#define MK_APPEND(fname, field)			\
  cell_t *fname(cell_t *a, cell_t *b) {		\
    if(!a) return b;				\
    if(!b) return a;				\
						\
    cell_t *p = a, *r;				\
    while(r = p->field) p = r;			\
    p->field = b;				\
    return a;					\
  }

MK_APPEND(conc_alt, alt);
/* append is destructive to a! */
MK_APPEND(append, next);

FUNC(popr) {
  FUNC(f) {
    cell_t *p = c->arg[0];
    if(!reduce(p) || !reduce(p->ptr)) {
      deref(p);
      return false;
    }
    if(p->ptr->alt) {
      conc_alt(p, quote(p->ptr->alt));
    }
    cell_t *a = closure_split1(c, 0);
    to_ref(c, p->ptr->val, quote(p->ptr->next), a);
    deref(p);
    return true;
  }
  return __reduce(func_f, c);
}

FUNC(alt) {
  FUNC(f) {
    cell_t *p = c->arg[0];
    bool ret = reduce(p);
    cell_t *a = 0;
    if(p->alt) {
      a = closure_alloc(2);
      a->func = func_alt;
      a->arg[0] = p->alt;
      a->arg[1] = c->arg[1];
    } else if (c->arg[1]) {
      a = closure_alloc(2);
      a->func = func_alt;
      a->arg[0] = c->arg[1];
      a->arg[1] = 0;
    }
    if(p->func == func_ref_reduced_ptr) {
      to_ref_ptr(c, ref(p->ptr), p->next, a);
    } else {
      to_ref(c, p->val, p->next, a);
    }
    deref(p);
    return ret;
  }
  return __reduce(func_f, c);
}

FUNC(concat) {
  FUNC(f) {
    cell_t *p = c->arg[0];
    bool ret = reduce(p);
    cell_t *a = closure_split1(c, 0);
    cell_t *n = 0;
    if(p->next) {
      n = closure_alloc(2);
      n->func = func_concat;
      n->arg[0] = p->next;
      n->arg[1] = c->arg[1];
    } else {
      n = c->arg[1];
    }
    if(p->func == func_ref_reduced_ptr) {
      to_ref_ptr(c, ref(p->ptr), n, a);
    } else {
      to_ref(c, p->val, n, a);
    }
    deref(p);
    return ret;
  }
  return __reduce(func_f, c);
}

cell_t *compose(cell_t *a, cell_t *b) {
  assert(is_closure(a) && is_closure(b));
  //  assert((is_cons(a) || is_nil(a)) &&
  //	 (is_cons(b) || is_nil(b)));
  if(is_nil(a)) return b;
  if(is_nil(b)) return a;
  cell_t *h = a->arg[0];
  cell_t *bp = compose(a->arg[1], b);
  return pushl(h, bp);
}

FUNC(compose) {
  c->ptr = compose(c->arg[0], c->arg[1]);
  return true;
}

FUNC(assert) {
  FUNC(f) {
    bool ret = reduce(c->arg[0]);
    intptr_t v = c->val;
    deref(c->arg[0]);
    return ret && v;
  }
  return __reduce(func_f, c);
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
  show(closure_args(e));
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

  intptr_t ret;
  reduce(e);
  show((int)e->val);
  deref(e);
  reduce(j);
  show((int)j->val);
  deref(j);
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
    return;
    //} else if(is_cons(r) || is_nil(r)) {
    //print_list(r);
    //return;
  } else if(r->func == func_ref) {
    //print_sexpr_help(r->arg[1]);
    printf(" (ref");
    print_sexpr_help((cell_t *)r->val);
    printf(")");
    return;
  } else if(r->func == func_ref_reduced) {
    printf(" %d", (int)r->val);
    return;
  } else if(r->func == func_ref_failed) {
    printf(" (ref FAIL)");
  } else if(r->func == func_nil) {
    printf(" nil");
    return;
  }

  intptr_t f = ((intptr_t)r->func) & ~1;
  char *n;
  int i;
  int args = closure_args(r);
  /*
  if(is_val(r)) {
    printf(" %d", (int)(intptr_t)r->arg[0]);
    return;
  }
  */
# define CASE(x) if(f == (intptr_t)func_##x) n = #x

  CASE(add);
  CASE(pushl);
  CASE(popr);
  CASE(assert);

# undef CASE

  printf(" (%s", n);

  for(i = 0; i < args; i++) {
    print_sexpr_help(r->arg[i]);
  }

  printf(")");
}

void print_sexpr(cell_t *r) {
  print_sexpr_help(r);
  printf("\n");
}

void show_eval(cell_t *c) {
  printf("[");
  show_eval_help(c);
  printf("\n");
  deref(c);
}

void show_eval_help(cell_t *c) {
  cell_t *t, *p = c;
  while(p) {
    t = p;
    if(reduce(p)) {
      if(p->func == func_ref_reduced_ptr) {
	printf(" [");
	show_eval_help(p->ptr);
      } else {
	printf(" %d", (int)p->val);
      }
      p = p->next;
      if(t != c) deref(t);
    } else {
      if(t != c) deref(t);
      printf(" FAIL");
      break;
    }
  }
  printf(" ]");
}

void test1() {
  int cnt;
  intptr_t x;
  cell_t *a, *b, *c, *d, *e, *p, *t, *z, *y;
  cell_t *a_, *b_, *c_, *e_;
  // 1 [2 +] pushl popr
  /*
  a = func(func_add, 2);
  e = val(2);
  arg(a, e);
  b = quote(a);

  a_ = func(func_add, 2);
  e_ = func(func_alt, 2);
  arg(e_, val(3));
  arg(e_, val(7));
  y = func(func_alt, 2);
  arg(y, e_);
  arg(y, val(20));
  arg(a_, y);
  b_ = quote(a_);
 
  z = func(func_alt, 2);
  arg(z, b_);
  arg(z, b);
  */
  c = func(func_pushl, 2);
  //arg(c, z);
  arg(c, quote(val(2)));
  arg(c, val(1));

  d = func(func_popr, 1);
  arg(d, c);
  //print_sexpr(d);

  show_all(d);
}

void test2() {
  int cnt;
  intptr_t x;
  cell_t *a, *b, *c, *d, *e, *f, *g, *h, *i, *k, *l, *m, *n, *p, *t;

  a = func(func_add, 2);
  b = val(2);
  //e = func(func_assert, 1);
  //arg(e, val(0));
  e = val(1);
  k = func(func_alt, 2);
  arg(k, b);
  arg(k, e);
  c = val(20);
  d = val(10);
  l = func(func_alt, 2);
  arg(l, c);
  arg(l, d);
  arg(a, k);
  arg(a, l);
  // a = (2 | 23) + (3 | 7)

  f = func(func_add, 2);
  g = val(300);
  h = val(200);
  i = val(100);
  m = func(func_alt, 2);
  arg(m, g);
  arg(m, h);
  n = func(func_alt, 2);
  arg(n, i);
  arg(n, m);
  arg(f, n);
  arg(f, a);
  // f = (31 | 63 | 127) + a
  
  addr(a);
  addr(b);
  addr(c);
  addr(d);
  addr(e);
  addr(f);
  addr(g);
  addr(h);
  addr(i);
  addr(k);
  addr(l);
  addr(m);
  addr(n);

  show_all(f);
}

void test3() {
  cell_t *a, *b, *c, *d;
  a = val(2);
  b = val(5);
  c = func(func_add, 2);
  arg(c, a);
  arg(c, b);
  d = ref_args(copy(c));
  intptr_t x, y;
  reduce(c);
  show((int)c->val);
  deref(c);
  reduce(d);
  show((int)d->val);
  deref(d);
}

void test4() {
  int cnt;
  intptr_t x;
  cell_t *a, *b, *c, *d, *e, *f, *g, *p, *t;

  a = val(1);
  b = val(2);
  
  c = func(func_alt, 2);
  arg(c, a);
  arg(c, b);
      
  d = val(10);
  f = func(func_alt, 2);
  arg(f, val(20));
  arg(f, val(30));
  e = func(func_alt, 2);
  arg(e, d);
  arg(e, f);

  g = func(func_add, 2);
  arg(g, c);
  arg(g, e);

  show_all(g);
}

FUNC(id) {
  cell_t *p = c->arg[0];
  bool ret = reduce(p);
  to_ref(c, p->val, p->next, p->alt);
  deref(p);
  return true;
}

void test5() {
  cell_t *p = func(func_id, 1);
  intptr_t x;
# define V(x) arg(p, val(x));
# define P arg(p, func(func_add, 2));

  // 5 40 + 300 + 2000 10000 + +
  // + -> P, x -> V(x), reverse order
  P P V(10000) V(2000) P V(300) P V(40) V(5) 

    reduce(p);
  show((int)p->val);
  deref(p);
}

void test6() {
  cell_t *a, *b, *c, *d, *e, *p, *t;
  int cnt;
  a = func(func_alt, 2);
  arg(a, val(2));
  arg(a, val(1));
  b = func(func_alt, 2);
  arg(b, val(20));
  arg(b, val(10));
  c = func(func_add, 2);
  arg(c, b);
  arg(c, a);

  e = func(func_alt, 2);
  arg(e, val(100));
  arg(e, val(200));
  d = func(func_add, 2);
  arg(d, c);
  arg(d, e);

  show_all(d);
}

void show_all(cell_t *c) {
  cell_t *p = c, *t;
  int cnt = 0;

  while(p) {
    t = p;
    if(reduce(p))
      printf("%d\n", (int)p->val);
    p = p->alt;
    deref(t);
    cnt++;
  }
  deref(p);
  show(cnt);
}

void test7() {
  cell_t *a, *b;
  a = func(func_alt, 2);
  arg(a, val(1));
  arg(a, val(2));
  b = func(func_add, 2);
  arg(b, a);
  arg(b, val(3));

  show_all(b);
}

void test8() {
  cell_t *a, *b, *c, *d, *e;
  c = func(func_alt, 2);
  arg(c, val(42));
  arg(c, val(51));
  a = func(func_quote, 1);
  arg(a, c);
  e = func(func_quote, 1);
  arg(e, val(123));
  d = func(func_alt, 2);
  arg(d, a);
  arg(d, e);
  b = func(func_popr, 1);
  arg(b, d);
  
  show_all(b);
}

void test9() {
  cell_t *a, *b, *c, *d, *e, *f, *g;
  /*
  a = func(func_append, 2);
  arg(a, quote(val(2)));
  arg(a, quote(val(1)));
  b = func(func_append, 2);
  arg(b, quote(val(3)));
  arg(b, a);
  c = func(func_pushl, 2);
  arg(c, b);
  arg(c, val(4));
  */
  /*
  g = func(func_pushl, 2);
  arg(g, ref(b));
  arg(g, val(5));
  */
  d = func(func_add, 2);
  arg(d, val(2));
  e = func(func_pushl, 2);
  arg(e, quote(d));
  arg(e, val(1));

  //f = func(func_popr, 1);
  //arg(f, c);

  //show_eval(f);
  //show_eval(g);
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
  //show_all(a);
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
  test8, test9, test10, test11
};

int main(int argc, char *argv[]) {
  unsigned int test_number;
  if(argc != 2) return -1;
  test_number = atoi(argv[1]);
  if(test_number >= LENGTH(tests)) return -2;
  cells_init();
  alloc_test();
  tests[test_number]();
  check_free();
  return 0;
}
