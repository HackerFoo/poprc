#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "rt_types.h"
#include "startle/macros.h"
#include "startle/support.h"
#include "startle/error.h"
#include "startle/log.h"
#include "cgen/primitives.h"

static int mem[256];
static int *mem_ptr = mem;
static char strings[1024];
static char *strings_ptr = strings;
static char string_buffer[64];

void init_primitives() {
  mem_ptr = mem;
  strings_ptr = strings;
}

#if INTERFACE

typedef int symbol_t;

typedef struct array {
  int size;
  int *elem;
} array;

#define __primitive_add(x, y) x + y
#define __primitive_sub(x, y) x - y
#define __primitive_mul(x, y) x * y
#define __primitive_eq(x, y) x == y;
#define __primitive_eq_s(x, y) x == y;
#define __primitive_neq(x, y) x != y;
#define __primitive_neq_s(x, y) x != y;
#define __primitive_gt(x, y) x > y
#define __primitive_gte(x, y) x >= y
#define __primitive_lt(x, y) x < y
#define __primitive_lte(x, y) x <= y
#define __primitive_mod(x, y) x % y
#define __primitive_not(x) !x
#define __primitive_bitand(x, y) x & y
#define __primitive_bitor(x, y) x | y
#define __primitive_bitxor(x, y) x ^ y
#define __primitive_shiftl(x, y) x << y
#define __primitive_shiftr(x, y) x >> y
#define __primitive_complement(x) ~x
#define __primitive_otherwise(bottom, x) (x)

#endif

bool __primitive_div(int x, int y, int *res) {
  if(y == 0) {
    return true;
  } else {
    if(res) *res = x / y;
    return false;
  }
}

const array nil = {0, NULL};

array __primitive_ap01(array arr, int *out0) {
  assert_error(arr.size >= 1, "array underflow");
  if(out0) *out0 = arr.elem[0];
  return (array) { .elem = arr.elem - 1,
                   .size = arr.size - 1 };
}

array __primitive_ap02(array arr, int *out1, int *out0) {
  assert_error(arr.size >= 2, "array underflow");
  if(out0) *out0 = arr.elem[0];
  if(out1) *out1 = arr.elem[-1];
  return (array) { .elem = arr.elem - 2,
                   .size = arr.size - 2 };
}

array __primitive_ap10(int in0, array arr) {
  int *elem = arr.elem ? arr.elem : mem_alloc(1);
  elem[-arr.size] = in0;
  return (array) { .elem = elem,
                   .size = arr.size + 1 };
}

array __primitive_compose20(array arrL, int in0, const array arrR) {
  array arr = {
    .elem = arrL.elem + arrR.size + 1,
    .size = arrL.size + arrR.size + 1
  };
  arr.elem[-arrR.size] = in0;
  memcpy(&arr.elem[-arrR.size + 1], &arrR.elem[-arrR.size + 1], arrR.size * sizeof(*arr.elem));
  return arr;
}

array __primitive_compose30(array arrL, int in0, int in1, const array arrR) {
  array arr = {
    .elem = arrL.elem + arrR.size + 2,
    .size = arrL.size + arrR.size + 2
  };
  arr.elem[-arrR.size-1] = in0;
  arr.elem[-arrR.size] = in1;
  memcpy(&arr.elem[-arrR.size + 1], &arrR.elem[-arrR.size + 1], arrR.size * sizeof(*arr.elem));
  return arr;
}

array __primitive_pushr1(array arr, int in0) {
  int *elem = arr.elem ? arr.elem + 1 : mem_alloc(1);
  elem[0] = in0;
  return (array) { .elem = elem,
                   .size = arr.size + 1 };
}

array __primitive_pushr2(array arr, int in0, int in1) {
  int *elem = arr.elem ? arr.elem + 2 : mem_alloc(2);
  elem[-1] = in0;
  elem[0] = in1;
  return (array) { .elem = elem,
                   .size = arr.size + 2 };
}

int __primitive_is_nil(array arr) {
  return arr.size == 0;
}

int *mem_alloc(unsigned int n) {
  assert_throw(mem_ptr - mem + n <= (int)LENGTH(mem), "out of mem");
  int *m = mem_ptr;
  mem_ptr += n;
  return m;
}

int *alloc_arr(array *arr, unsigned int n) {
  arr->size = n;
  arr->elem = mem_alloc(n);
  return arr->elem;
}

char *string_alloc(unsigned int n) {
  assert_throw(strings_ptr - strings + n <= (int)LENGTH(strings), "out of mem");
  char *s = strings_ptr;
  strings_ptr += n;
  return s;
}

seg_t alloc_seg(char *s, unsigned int n) {
  char *ns = string_alloc(n);
  memcpy(ns, s, n);
  return (seg_t) {
    .s = ns,
    .n = n
  };
}

seg_t __primitive_to_string(int x) {
  unsigned int len = max(0, snprintf(string_buffer,sizeof(string_buffer), "%d", x));
  return alloc_seg(string_buffer, min(sizeof(string_buffer), len));
}

TEST(prim_to_string) {
  init_primitives();
  seg_t s = __primitive_to_string(42);
  return segcmp("42", s) == 0 ? 0 : -1;
}

symbol_t __primitive_print(symbol_t io, seg_t str) {
  printf("%.*s\n", (int)str.n, str.s);
  fflush(stdout);
  return io;
}

symbol_t __primitive_input(symbol_t io, seg_t *str) {
  char *s = fgets(string_buffer, sizeof(string_buffer), stdin);
  int n = 0;
  if(s) {
    char *e = s;
    while(e < *(&string_buffer + 1) &&
          !ONEOF(*e, '\0', '\n')) e++;
    n = e - s;
  }
  if(str) *str = alloc_seg(s, n);
  return io;
}

seg_t __primitive_strtrim(seg_t str) {
  return seg_trim(str);
}

bool __from_string(seg_t str, int *x) {
  char *end = NULL;
  long lx = strtol(str.s, &end, 0);
  if(!end) {
    return true;
  } else {
    *x = lx;
    return false;
  }
}

symbol_t __primitive_eq_str(seg_t a, seg_t b) {
  return a.n == b.n && strncmp(a.s, b.s, a.n) == 0 ?
    SYM_True : SYM_False;
}

seg_t __primitive_strcat(seg_t a, seg_t b) {
  char *s = string_alloc(a.n + b.n);
  memcpy(s, a.s, a.n);
  memcpy(s + a.n, b.s, b.n);
  return (seg_t) {
    .s = s,
    .n = a.n + b.n
  };
}
