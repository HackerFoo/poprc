#include "rt_types.h"
#include "gen/rt.h"
#include "gen/test.h"
#include "gen/eval.h"
#include "gen/primitive.h"

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

void check_free() {
  int i;
  for(i = 1; i < LENGTH(cells); i++) {
    if(is_closure(&cells[i])) {
      printf("LEAK: %d ", i);
      print_sexpr(&cells[i]);
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
  cell_t *b = func(func_popr, 2);
  cell_t *c = dep(ref(b));
  arg(b, ref(c));
  arg(b, a);
  show_eval(b);
  show_eval(c);
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
  cell_t *d = func(func_alt, 2);
  arg(d, val(1));
  arg(d, val(2));
  //cell_t *d = val(1);
  cell_t *a = func2(func_popr, 1, 2);
  arg(a, quote(d));
  show_eval(a);

  /*
  show_eval(b);
  show_eval(a);
  */
}

void test17() {
  cell_t *a = quote(val(1));
  cell_t *b = quote(val(2));
  cell_t *c = quote(val(3));
  cell_t *d = func(func_alt, 2);
  arg(d, a);
  arg(d, b);
  cell_t *e = func(func_append, 2);
  arg(e, c);
  arg(e, d);
  show_eval(e);
}

void test18() {
  cell_t *a = func(func_concat, 2);
  arg(a, val(2));
  arg(a, val(1));
  cell_t *b = func(func_concat, 2);
  arg(b, val(3));
  arg(b, a);
  show_eval(quote(b));
}

cell_t *reverse(cell_t *c) {
  cell_t *prev = 0, *p = c, *t;
  while(p) {
    t = p->next;
    p->next = prev;
    prev = p;
    p = t;
  }
  return prev;
}

void test19() {
  cell_t *l = 0;
  l = func2(func_popr, 1, 2);
  l = compose1(quote(val(1)), l, 0);
  //l = compose(val(2), l);
  show_eval(l);
}

void (*tests[20])(void) = {
  test0, test1, test2, test3,
  test4, test5, test6, test7,
  test8, test9, test10, test11,
  test12, test13, test14, test15,
  test16, test17, test18, test19
};

