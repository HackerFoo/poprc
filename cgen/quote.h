#ifndef __QUOTE__
#define __QUOTE__

#include <stdint.h>
#include <stddef.h>

typedef intptr_t any_t;

typedef union slot slot_t;

typedef slot_t *(*popr_fn)(slot_t *, any_t *);

union slot {
  any_t val;
  popr_fn popr;
};

slot_t *__primitive_quote(popr_fn f, size_t n);
slot_t *__primitive_popr(slot_t *l, any_t *x);
slot_t *__primitive_pushl(any_t x, slot_t *l);

#endif
