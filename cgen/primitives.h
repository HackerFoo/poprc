#ifndef __cgen_primitives__
#define __cgen_primitives__

#include <string.h>
#include "gen/error.h"

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

typedef struct array {
  int size;
  int *elem;
} array;

const static array nil = {0, NULL};

int *mem_alloc(unsigned int n);
int *alloc_arr(array *arr, unsigned int n);

static inline
array __primitive_ap01(array arr, int *out0) {
  assert_error(arr.size >= 1, "array underflow");
  if(out0) *out0 = arr.elem[0];
  return (array) { .elem = arr.elem - 1,
                   .size = arr.size - 1 };
}

static inline
array __primitive_ap02(array arr, int *out1, int *out0) {
  assert_error(arr.size >= 2, "array underflow");
  if(out0) *out0 = arr.elem[0];
  if(out1) *out1 = arr.elem[-1];
  return (array) { .elem = arr.elem - 2,
                   .size = arr.size - 2 };
}

static inline
array __primitive_ap10(int in0, array arr) {
  int *elem = arr.elem ? arr.elem : mem_alloc(1);
  elem[-arr.size] = in0;
  return (array) { .elem = elem,
                   .size = arr.size + 1 };
}

static inline
array __primitive_compose20(array arrL, int in0, const array arrR) {
  array arr = {
    .elem = arrL.elem + arrR.size + 1,
    .size = arrL.size + arrR.size + 1
  };
  arr.elem[-arrR.size] = in0;
  memcpy(&arr.elem[-arrR.size + 1], &arrR.elem[-arrR.size + 1], arrR.size * sizeof(*arr.elem));
  return arr;
}

static inline
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

static inline
int __primitive_is_nil(array arr) {
  return arr.size == 0;
}

array parse(const char **sp, const char *e);
void print_array(array arr);

#endif
