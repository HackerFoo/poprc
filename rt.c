#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

typedef struct closure closure_t;
typedef int (*closure_func_t)(closure_t *);
struct closure {
  closure_func_t func;
  union {
    intptr_t val;
    closure_t *in[1];
  };
};

bool closure_is_ready(closure_t *c) {
  return !((intptr_t)c->func & 0x1);
}

void closure_set_ready(closure_t *c, bool r) {
  c->func = (closure_func_t)(((intptr_t)c->func & ~0x1) | !r);
}

#define LENGTH(_a) (sizeof((_a)[0]) / sizeof(_a))

// make sure &cl_stack > 255
uint8_t padding[256];
closure_t cl_stack[1024];
int top = 0;

int noop(closure_t *c) {
  return c->val;
}

bool is_closure(void *p) {
  return p >= (void *)&cl_stack && p < (void *)(&cl_stack+1);
}

closure_t *val(int x) {
  cl_stack[top].func = noop;
  cl_stack[top].val = x;
  return &cl_stack[top++];
}

// max offset is 255
bool is_offset(closure_t *c) {
  return !((intptr_t)c & ~0xff);
}

// args must be >= 1
closure_t *func(int (*f)(closure_t *), int args) {
  // assert(args >= 0); // breaks things somehow
  int cur = top;
  int size = 1 + ((sizeof(closure_t *) * (args - 1) + sizeof(closure_t) - 1) / sizeof(closure_t));
  bzero(&cl_stack[cur], sizeof(closure_t)*size);
  cl_stack[cur].func = f;
  cl_stack[cur].in[0] = (closure_t *)(intptr_t)(args - 1);
  top += size;
  closure_set_ready(&cl_stack[cur], false);
  return &cl_stack[cur];
}
/*
closure_t *pop() {
  while(top && (!cl_stack[--top].func ||
		is_closure(cl_stack[top].func)));
  return &cl_stack[top];
}
*/
int add(closure_t *c) {
  int x = force(c->in[0]);
  int y = force(c->in[1]);
  printf("add(%d, %d)\n", x, y);
  c->val = x + y;
  c->func = noop;
  return c->val;
}

int closure_size(closure_t *c) {
  if(c->func == noop) return 0;
  closure_t **p = c->in;
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

int closure_next(closure_t *c) {
  return is_offset(c->in[0]) ? (intptr_t)c->in[0] : 0;
}

// destructive composition
void comp(closure_t *c, closure_t *a) {
  assert(!closure_is_ready(c));
  int i = closure_next(c);
  if(!is_closure(c->in[i])) {
    c->in[0] = (closure_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
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

closure_t *dup(closure_t *c) {
  assert(is_closure(c));
  if(closure_is_ready(c)) return c;
  int size = closure_size(c);
  closure_t *tmp = func(c->func, size);
  memcpy(&tmp->in, &c->in, sizeof(c->in[0]) * size);
  // c->in[<i] are empty and c->in[>i] are ready, so no need to copy
  // c->in[i] only needs copied if filled
  int i = closure_next(c);
  if(is_closure(c->in[i])) tmp->in[i] = dup(c->in[i]);
  return tmp;
}

inline
int force(closure_t *c) {
  assert(closure_is_ready(c));
  c->func(c);
}

int main() {
  closure_t *a, *b, *c, *d, *e, *f, *g;

  a = val(1);
  b = val(2);
  c = func(add, 2);
  d = val(4);
  e = func(add, 2);
  comp(e, c);
  comp(e, a);
  comp(e, b);
  printf("closure_size(e) = %d\n", closure_size(e));
  f = dup(e);
  comp(e, d);
  comp(f, a);
  g = func(add, 2);
  comp(g, val(10));
  comp(g, f);

#define show(x)\
printf(#x " = %d\n", force(x))

  show(c);
  show(e);
  show(g);
  return 0;
}
