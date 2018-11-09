#include "rt_types.h"
#include <stdlib.h>
#include <stdio.h>

#include "primitives.h"
#include "tok.h"
#include "startle/support.h"

void print_array(array arr) {
  int *x = arr.elem - arr.size + 1;
  if(arr.size == 0) {
    printf(" []");
  } else if(arr.size == 1) {
    printf(" %d", x[0]);
  } else {
    printf(" [%d", x[0]);
    RANGEUP(i, 1, arr.size) {
      printf(" %d", x[i]);
    }
    printf("]");
  }
}

array parse(const char **sp, const char *e) {
  array arr = {0, NULL};
  int depth = 0;
  const char *s = *sp;
  seg_t t;
  char_class_t cc = CC_NONE;

  while(t = tok(s, e, &cc), t.s) {
    s = seg_end(t);
    switch(cc) {
    case CC_NUMERIC:
      arr.elem = mem_alloc(1); // using mem as a stack
      arr.elem[0] = strtol(t.s, NULL, 0);
      arr.size++;
      break;
    case CC_BRACKET:
      if(t.s[0] == '[') {
        depth++;
        break;
      } else if(t.s[0] == ']') {
        depth--;
        break;
      }
    default:
      continue;
    }
    if(depth <= 0) break;
  }

  *sp = seg_end(t);
  return arr;
}
