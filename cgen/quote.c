#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "macros.h"
#include "quote.h"

slot_t slots[1<<12] = {0};
slot_t *slot_ptr = slots;

static
slot_t *new_slots(size_t n) {
  slot_t *x = slot_ptr;
  slot_ptr += n;
  return x;
}

slot_t *__primitive_quote(popr_fn f, slot_t *s, size_t n) {
  if(!s) {
    s = new_slots(n + 1);
  } 
  s->popr = f;
  memset(s + 1, 0, sizeof(slot_t) * n);
  return s + n;
}

slot_t *__primitive_pushl(any_t x, slot_t *s) {
  assert(!s->val);
  s->val = x;
  return s - 1;
}

slot_t *__primitive_popr(slot_t *s, any_t *x) {
  if(s->popr) {
    return s->popr(s, x);
  } else { // id
    s++;
    *x = s->val;
    s->popr = 0;
    return s;
  }
}

