#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct closure closure_t;
struct closure {
  int (*func)(closure_t *);
  union {
    intptr_t val;
    closure_t *in[1];
  };
};

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

closure_t *func(int (*f)(closure_t *), int args) {
  int cur = top;
  int size = 1 + ((sizeof(closure_t *) * (args - 1) + sizeof(closure_t) - 1) / sizeof(closure_t));
  bzero(&cl_stack[cur], sizeof(closure_t)*size);
  /*
  if((void*)(&cl_stack[cur].in[args]) < (void*)(&cl_stack+1))
    cl_stack[cur].in[args] = &cl_stack[-1]; // invalid nonzero value to mark end of arguments
  */
  cl_stack[cur].func = f;
  cl_stack[cur].in[0] = (closure_t *)(intptr_t)args;
  top += size;
  return &cl_stack[cur];
}

closure_t *pop() {
  while(top && (!cl_stack[--top].func ||
		is_closure(cl_stack[top].func)));
  return &cl_stack[top];
}

int add(closure_t *c) {
  int x = force(c->in[0]);
  int y = force(c->in[1]);
  printf("add(%d, %d)\n", x, y);
  c->val = x + y;
  c->func = noop;
  return c->val;
}

bool arg(closure_t *c, closure_t *a) {
  if(c->func == noop) return false;

  // c->in[0] is an offset to the next arg
  // until it is replaced with a closure
  if(is_closure(c->in[0]) ||
     !(intptr_t)c->in[0]) return false;
  intptr_t o = --*((intptr_t *)&c->in[0]);
  c->in[o] = a;
  return true;
}

int closure_size(closure_t *c) {
  if(c->func == noop) return 0;
  closure_t **p = c->in;
  int n = 0;
  if(is_offset(*p)) {
    intptr_t o = (intptr_t)(*p);
    p += o;
    n += o;
  }
  while(is_closure(*p)) {
    p++;
    n++;
  }
  return n;
}

// destructive composition
bool comp(closure_t *c, closure_t *a) {
  if(c->func == noop) return false;
  int i = closure_size(c) - 1;
  while(i >= 0 && is_closure(c->in[i])) {
    if(comp(c->in[i], a)) return true;
    --i;
  }
  if(i >= 0) {
    c->in[0] = (closure_t *)(intptr_t)i;
    c->in[i] = a;
    return true;
  }
  return false; 
}

closure_t *dup(closure_t *c) {
  if(!is_closure(c)) return c;
  closure_t *tmp;
  bool copy = false;
  if(c->func == noop) {
    tmp = c;
  } else {
    int i, size = closure_size(c);
    tmp = func(c->func, size);
    for(i = 0; i < size; i++) {
      tmp->in[i] = dup(c->in[i]);
      if(tmp->in[i] != c->in[i] ||
	 !is_closure(tmp->in[i]))
	copy = true;
    }
    if(!copy) {
      pop();
      tmp = c;
    }
    else printf("copy\n");
  }
  return tmp;
}

inline
int force(closure_t *c) {
  c->func(c);
}

int main() {
  closure_t *a, *b, *c, *d, *e, *f;

  a = val(1);
  b = val(2);
  c = func(add, 2);
  d = val(4);
  e = func(add, 2);
  comp(e, c);
  comp(e, a);
  comp(e, b);
  f = dup(e);
  comp(e, d);
  comp(f, a);
  
  printf("c = %d\n", force(c));
  printf("e = %d\n", force(e));
  printf("f = %d\n", force(f));
  printf("closure_size() = %d\n", closure_size(f));
  return 0;
}
