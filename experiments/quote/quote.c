#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "macros.h"

// trying out an idea for lightweight quotes

typedef union slot slot_t;

typedef slot_t *(*popr_fn)(slot_t *, intptr_t *);

union slot {
  intptr_t val;
  slot_t *arg;
  popr_fn popr;
};

slot_t slots[1<<8] = {0};
slot_t *slot_ptr = slots;

slot_t *new_slots(size_t n) {
  slot_t *x = slot_ptr;
  slot_ptr += n;
  return x;
}

slot_t *pushl(slot_t *x, slot_t *s) {
  assert(!s->arg);
  s->arg = x;
  return s - 1;
}

slot_t *popr(slot_t *s, intptr_t *v) {
  if(s->popr) {
    return s->popr(s, v);
  } else { // id
    s++;
    *v = s->val;
    s->popr = 0;
    return s;
  }
}

#define DEF_FN_21(name)                       \
  slot_t *fn_##name(slot_t *s, intptr_t *v) { \
    intptr_t x, y;                            \
    popr(s[1].arg, &x);                       \
    popr(s[2].arg, &y);                       \
    *v = name(x, y);                          \
    s->popr = 0;                              \
    return s;                                 \
  }


#define DEF_FN_11(name)                       \
  slot_t *fn_##name(slot_t *s, intptr_t *v) { \
    intptr_t x;                               \
    popr(s[1].arg, &x);                       \
    *v = name(x);                             \
    s->popr = 0;                              \
    return s;                                 \
}

#define DEF_OP(name, op)                        \
  intptr_t name(intptr_t x, intptr_t y) {       \
    return (x) op (y);                          \
  }

DEF_OP(add, +);
DEF_OP(sub, -);
DEF_OP(mul, *);

intptr_t negate(intptr_t x) {
  return -x;
}

DEF_FN_11(negate);
DEF_FN_21(add);
DEF_FN_21(sub);
DEF_FN_21(mul);

slot_t *make_closure(char c) {
  slot_t *ret = NULL;
  if(c >= '0' && c <= '9') {
    ret = new_slots(2);
    ret[0].popr = 0;
    ret[1].val = c - '0';
  } else {
    switch(c) {
    case '+':
      ret = new_slots(3);
      ret[0].popr = fn_add;
      ret += 2;
      break;
    case '-':
      ret = new_slots(3);
      ret[0].popr = fn_sub;
      ret += 2;
      break;
    case '*':
      ret = new_slots(3);
      ret[0].popr = fn_mul;
      ret += 2;
      break;
    case 'n':
      ret = new_slots(2);
      ret[0].popr = fn_negate;
      ret += 1;
      break;
    }
  }
  return ret;
}

int main(int argc, char **argv) {
  slot_t
    *x1 = make_closure('1'),
    *x2 = make_closure('2'),
    *add = make_closure('+'),
    *r = pushl(x1, pushl(x2, add));
  intptr_t val;
  r = popr(r, &val);

  printf("val = %d\n", (int)val);
  
  return 0;
}
