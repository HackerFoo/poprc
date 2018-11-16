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

#if INTERFACE
typedef int symbol_t;
typedef int any_t;

typedef struct array {
  unsigned int capacity,
               offset,
               size;
  int *elem;
} array;
#endif

void init_primitives() {
  mem_ptr = mem;
  strings_ptr = strings;
}

array arr_alloc(unsigned int n) {
  assert_throw(mem_ptr - mem + n <= (int)LENGTH(mem), "out of mem");
  array arr = {
    .capacity = n,
    .elem = mem_ptr
  };
  mem_ptr += n;
  return arr;
}

char *string_alloc(unsigned int n) {
  assert_throw(strings_ptr - strings + n <= (int)LENGTH(strings), "out of mem");
  char *s = strings_ptr;
  strings_ptr += n;
  return s;
}

seg_t seg_alloc(char *s, unsigned int n) {
  char *ns = string_alloc(n);
  memcpy(ns, s, n);
  return (seg_t) {
    .s = ns,
    .n = n
  };
}

unsigned int array_remaining(array *arr) {
  return arr->capacity - arr->size;
}

int *arr_elem(array *arr, unsigned int i) {
  return i < arr->size ?
    &arr->elem[(arr->capacity + arr->offset - i) % arr->capacity] :
    NULL;
}

bool arr_shift(array *arr, int l, int r) {
  bool res;
  int s = l - r;
  if((res = INRANGE(arr->size + s, 0, arr->capacity))) {
    arr->size += s;
    arr->offset = (arr->capacity + arr->offset - r) % arr->capacity;
  }
  return res;
}

void print_array(array *arr) {
  printf("[");
  if(arr->size > 0) {
    printf("%d", *arr_elem(arr, arr->size - 1));
    COUNTDOWN(i, arr->size - 1) {
      printf(", %d", *arr_elem(arr, i));
    }
  }
  printf("]\n");
}

array arr_new() {
  return arr_alloc(32);
}

TEST(arr_shift) {
  init_primitives();
  array arr = arr_alloc(16);
  print_array(&arr);
  arr_shift(&arr, 3, 0);
  COUNTUP(i, 3) *arr_elem(&arr, i) = i;
  print_array(&arr);
  arr_shift(&arr, 0, 1);
  print_array(&arr);
  arr_shift(&arr, 1, 0);
  *arr_elem(&arr, arr.size - 1) = 3;
  print_array(&arr);
  return 0;
}

#if INTERFACE

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

bool __primitive_mod(int x, int y, int *res) {
  if(y == 0) {
    return true;
  } else {
    if(res) *res = x % y;
    return false;
  }
}

const array nil = {0, 0, 0, NULL};

bool __primitive_ap01(array arr, array *ret, int *out0) {
  if(arr.size < 1) return true;
  if(out0) *out0 = *arr_elem(&arr, 0);
  arr_shift(&arr, 0, 1);
  if(ret) *ret = arr;
  return false;
}

bool __primitive_ap02(array arr, array *ret, int *out1, int *out0) {
  if(arr.size < 2) return true;
  if(out0) *out0 = *arr_elem(&arr, 0);
  if(out1) *out1 = *arr_elem(&arr, 1);
  arr_shift(&arr, 0, 2);
  if(ret) *ret = arr;
  return false;
}

array __primitive_ap10(int in0, array arr) {
  arr_shift(&arr, 1, 0);
  *arr_elem(&arr, arr.size - 1) = in0;
  return arr;
}

array __primitive_compose20(array arrL, int in0, array arrR) {
  unsigned int n = arrL.size + 1;
  arr_shift(&arrR, n, 0);
  COUNTUP(i, arrL.size) {
    *arr_elem(&arrR, i + n) = *arr_elem(&arrL, i);
  }
  *arr_elem(&arrR, n - 1) = in0;
  return arrR;
}

array __primitive_compose30(array arrL, int in0, int in1, array arrR) {
  unsigned int n = arrL.size + 2;
  arr_shift(&arrR, n, 0);
  COUNTUP(i, arrL.size) {
    *arr_elem(&arrR, i + n) = *arr_elem(&arrL, i);
  }
  *arr_elem(&arrR, n - 1) = in0;
  *arr_elem(&arrR, n - 2) = in1;
  return arrR;
}

array __primitive_pushr1(array arr, int in0) {
  arr_shift(&arr, 0, -1);
  *arr_elem(&arr, 0) = in0;
  return arr;
}

array __primitive_pushr2(array arr, int in1, int in0) {
  arr_shift(&arr, 0, -2);
  *arr_elem(&arr, 0) = in0;
  *arr_elem(&arr, 1) = in1;
  return arr;
}

array __primitive_quote0(int in0) {
  array arr = arr_new();
  arr_shift(&arr, 1, 0);
  *arr_elem(&arr, 0) = in0;
  return arr;
}

symbol_t __primitive_is_nil(array arr) {
  return arr.size == 0;
}

seg_t __primitive_to_string(int x) {
  unsigned int len = max(0, snprintf(string_buffer,sizeof(string_buffer), "%d", x));
  return seg_alloc(string_buffer, min(sizeof(string_buffer), len));
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
  if(str) *str = seg_alloc(s, n);
  return io;
}

seg_t __primitive_strtrim(seg_t str) {
  return seg_trim(str);
}

bool __primitive_from_string(seg_t str, int *x) {
  char *end = NULL;
  long lx = strtol(str.s, &end, 0);
  if(!end || end <= str.s) {
    return true;
  } else {
    *x = lx;
    return false;
  }
}

symbol_t __primitive_eq_str(seg_t a, seg_t b) {
  return a.n == b.n && strncmp(a.s, b.s, a.n) == 0;
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
