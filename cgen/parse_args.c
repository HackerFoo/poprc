#include "rt_types.h"
#include <stdlib.h>
#include <stdio.h>

#include "primitives.h"
#include "tok.h"
#include "startle/support.h"

static int mem[256];
static int *mem_ptr = mem;

int *mem_alloc(unsigned int n) {
  assert_throw(mem_ptr - mem + n <= LENGTH(mem), "out of mem");
  int *m = mem_ptr;
  mem_ptr += n;
  return m;
}

int *alloc_arr(array *arr, unsigned int n) {
  arr->size = n;
  arr->elem = mem_alloc(n);
  return arr->elem;
}

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

// dummy formatters
FORMAT(cell, 'C') {}
FORMAT(entry, 'E') {}
FORMAT(entry_short, 'e') {}
FORMAT(function, 'O') {}
