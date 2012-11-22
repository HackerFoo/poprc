#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>

typedef struct closure closure_t;
struct closure {
  int (*func)(closure_t *);
  union {
    int val;
    closure_t *in[1];
  };
};

#define LENGTH(_a) (sizeof((_a)[0]) / sizeof(_a))
closure_t cl_stack[1024];
int top = 0;

int noop(closure_t *c) {
  return c->val;
}

closure_t *val(int x) {
  cl_stack[top].func = noop;
  cl_stack[top].val = x;
  return &cl_stack[top++];
}

closure_t *func(int (*f)(closure_t *), int args) {
  int cur = top;
  int size = 1 + ((sizeof(closure_t *) * (args - 1) + sizeof(closure_t) - 1) / sizeof(closure_t));
  bzero(&cl_stack[cur], sizeof(closure_t)*size);
  if((void*)(&cl_stack[cur].in[args]) < (void*)(&cl_stack+1))
    cl_stack[cur].in[args] = &cl_stack[-1]; // invalid nonzero value to mark end of arguments
  cl_stack[cur].func = f;
  top += size;
  return &cl_stack[cur];
}

int add(closure_t *c) {
  int x = force(c->in[0]);
  int y = force(c->in[1]);
  printf("add(%d, %d)\n", x, y);
  c->val = x + y;
  c->func = noop;
  return c->val;
}

bool is_closure(void *p) {
  return p >= (void *)&cl_stack && p < (void *)(&cl_stack+1);
}

// destructive composition
bool comp(closure_t *c, closure_t *a) {
  closure_t **p = c->in;
  while(is_closure(*p)) {
    if(comp(*p, a)) return true;
    ++p;
  }
  if(!*p) {
    *p = a;
    return true;
  }
  return false; 
}
/*
closure_t *dup(closure_t *c) {
  closure_t tmp;
  
}
*/
bool arg(closure_t *c, closure_t *a) {
  closure_t **p = c->in;
  while(is_closure(*p)) ++p;
  if(!*p) {
    *p = a;
    return true;
  }
  return false;
}

inline
int force(closure_t *c) {
  c->func(c);
}

void main() {
  closure_t *a, *b, *c, *d, *e;

  a = val(1);
  b = val(2);
  c = func(add, 2);
  d = val(4);
  e = func(add, 2);
  comp(e, c);
  comp(e, a);
  comp(e, b);
  comp(e, d);
  
  printf("c = %d\n", force(c));
  printf("e = %d\n", force(e));
  printf("c = %d\n", force(c));
}
