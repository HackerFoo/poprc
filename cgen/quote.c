#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "macros.h"

// trying out an idea for lightweight quotes

typedef union slot slot_t;

typedef slot_t *(*slot_fn)(slot_t *);

union slot {
  intptr_t val;
  slot_t *arg;
  slot_fn fn;
};

slot_t slots[1<<8] = {0};
slot_t *slot_ptr = slots;

slot_t *new_slots(size_t n) {
  slot_t *x = slot_ptr;
  slot_ptr += n;
  return x;
}

slot_t *reduce(slot_t *x) {
  return (x->fn)(x);
}

#define DEF_FN_2(name)                   \
  slot_t *fn_##name(slot_t *s) {         \
    slot_t                               \
      *r = new_slots(1),                 \
      *x = reduce(s[1].arg),             \
      *y = reduce(s[2].arg);             \
    r[0].val = name(x->val, y->val);     \
    return r;                            \
}

#define DEF_FN_1(name)            \
  slot_t *fn_##name(slot_t *s) {  \
    slot_t                        \
      *r = new_slots(1),          \
      *x = reduce(s[1].arg);      \
    r[0].val = name(x->val);      \
    return r;                     \
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

slot_t *fn_id(slot_t *s) {
  return s + 1;
}

DEF_FN_1(negate);
DEF_FN_2(add);
DEF_FN_2(sub);
DEF_FN_2(mul);

slot_t *make_closure(char c) {
  slot_t *ret = NULL;
  if(c >= '0' && c <= '9') {
    ret = new_slots(2);
    ret[0].fn = fn_id;
    ret[1].val = c - '0';
  } else {
    switch(c) {
    case '+':
      ret = new_slots(3);
      ret[0].fn = fn_add;
      ret += 2;
      break;
    case '-':
      ret = new_slots(3);
      ret[0].fn = fn_sub;
      ret += 2;
      break;
    case '*':
      ret = new_slots(3);
      ret[0].fn = fn_mul;
      ret += 2;
      break;
    case 'n':
      ret = new_slots(2);
      ret[0].fn = fn_negate;
      ret += 1;
      break;
    }
  }
  return ret;
}

slot_t *pushl(slot_t *x, slot_t *s) {
  s->arg = x;
  return s - 1;
}

int main(int argc, char **argv) {
  slot_t
    *x1 = make_closure('1'),
    *x2 = make_closure('2'),
    *add = make_closure('+'),
    *gr = pushl(x1, pushl(x2, add)),
    *r = reduce(gr);

  printf("r->val = %d\n", (int)r->val);
  
  return 0;
}
