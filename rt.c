#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

typedef struct cell cell_t;
typedef int (*cell_func_t)(cell_t *);
struct cell {
  union {
    cell_func_t func;
    cell_t *prev;
  };
  union {
    intptr_t val;
    cell_t *in[1];
    cell_t *next;
  };
};

// make sure &cells > 255
#define STRIDE 1
cell_t cells[1024];
cell_t *cells_ptr;

// #define CHECK_CYCLE

#define show(x)\
printf(#x " = %d\n", x)

#define LENGTH(_a) (sizeof(_a) / sizeof((_a)[0]))

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
  c->func = (cell_func_t)(((intptr_t)c->func & ~0x1) | !r);
}

int closure_val(cell_t *c) {
  assert(is_closure(c));
  printf("val(%d)\n", (int)c->val);
  return c->val;
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

  // set up doubly-linked pointer ring
  for(i = 0; i < LENGTH(cells); i++) {
    cells[i].prev = &cells[(i-STRIDE) % LENGTH(cells)];
    cells[i].next = &cells[(i+STRIDE) % LENGTH(cells)];
  }
  //cells[0].prev = &cells[LENGTH(cells)-1];
  //cells[LENGTH(cells)-1].next = &cells[0];

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

cell_t *closure_alloc(int size) {
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
  /*
  while(cells_ptr >= &c[0] && cells_ptr < &c[size])
    cells_next();
  */
  // remove the found chunk
  int i;
  for(i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  bzero(c, sizeof(cell_t)*size);
  assert(check_cycle());
  return c;
}

void closure_free(cell_t *c) {
  int i, size = 1 + ((sizeof(cell_t *) * (closure_size(c) - 1) + sizeof(cell_t) - 1) / sizeof(cell_t));
  assert(is_closure(c));
  for(i = 0; i < size; i++) {
    c[i].prev = &c[i-1];
    c[i].next = &c[i+1];
  }
  c[0].prev = cells_ptr->prev;
  cells_ptr->prev->next = &c[0];
  c[size-1].next = cells_ptr;
  cells_ptr->prev = &c[size-1];
  assert(check_cycle());
}

cell_t *val(int x) {
  cell_t *c = closure_alloc(1);
  c->func = closure_val;
  c->val = x;
  return c;
}

bool is_val(cell_t *c) {
  return c->func == closure_val;
}

// max offset is 255
bool is_offset(cell_t *c) {
  return !((intptr_t)c & ~0xff);
}

// args must be >= 1
cell_t *func(int (*f)(cell_t *), int args) {
  assert(args >= 0);
  int size = 1 + ((sizeof(cell_t *) * (args - 1) + sizeof(cell_t) - 1) / sizeof(cell_t));
  cell_t *c = closure_alloc(size);
  assert(c->func == 0);
  c->func = f;
  c->in[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, false);
  return c;
}

int consume(cell_t *c) {
  int x = force(c);
  closure_free(c);
  return x;
}

int add(cell_t *c) {
  int x = force(c->in[0]);
  int y = force(c->in[1]);
  printf("add(%d, %d)\n", x, y);
  c->val = x + y;
  c->func = closure_val;
  return c->val;
}

int closure_size(cell_t *c) {
  assert(is_closure(c));
  if(is_val(c)) return 0;
  cell_t **p = c->in;
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
  return is_offset(c->in[0]) ? (intptr_t)c->in[0] : 0;
}

// destructive composition
void comp(cell_t *c, cell_t *a) {
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  int i = closure_next_child(c);
  if(!is_cell(c->in[i])) {
    c->in[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->in[i] = a;
    if(i == 0) closure_set_ready(c, closure_is_ready(a));
  } else {
    comp(c->in[i], a);
    if(closure_is_ready(c->in[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)c->in; // decrement offset
    }
  }
}

cell_t *dup(cell_t *c) {
  assert(is_closure(c));
  if(closure_is_ready(c)) return c;
  int size = closure_size(c);
  cell_t *tmp = func(c->func, size);
  memcpy(&tmp->in, &c->in, sizeof(c->in[0]) * size);
  // c->in[<i] are empty and c->in[>i] are ready, so no need to copy
  // c->in[i] only needs copied if filled
  int i = closure_next_child(c);
  if(is_closure(c->in[i])) tmp->in[i] = dup(c->in[i]);
  return tmp;
}

inline
int force(cell_t *c) {
  assert(closure_is_ready(c));
  c->func(c);
}
/*
int closure_max(int pmax, cell_t *c) {
  int n = c - &cells[0];
  if(is_val(c)) return n;

  int i = closure_next_child(c);
  if(!is_closure(c->in[i])) i++;
  while(is_closure(c->in[i])) {
    n = closure_max(n, c->in[i]);
    i++;
  }
  return n;
}
*/

alloc_test() {
  int i, j;
  cell_t *a[30];
  for(j = 0; j < 50; j++) {
    for(i = 0; i < LENGTH(a); i++) {
      a[i] = func(add, 9);
    }
    for(i = 0; i < LENGTH(a); i++) {
      closure_free(a[i]);
    }
  }
}

cell_t *test() {
  cell_t *a, *b, *c, *d, *e, *f, *g, *h;

  g = func(add, 2);
  e = func(add, 2);
  c = func(add, 2);

  h = val(10);
  b = val(2);
  a = val(1);

  comp(e, c);
  comp(e, a);
  comp(e, b);
  show(closure_size(e));
  f = dup(e);
  d = val(4);
  comp(e, d);
  comp(f, a);
  comp(g, h);
  comp(g, f);

  //show(force(c));
  //show(force(e));
  show(force(f));
  //show(closure_max(0, g));
  
  return g;
}

int main() {
  cells_init();
  alloc_test();
  cell_t *x = test();
  show(force(x));
  return 0;
}
