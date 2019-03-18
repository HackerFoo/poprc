#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "rt_types.h"
#include "startle/macros.h"
#include "startle/support.h"
#include "startle/error.h"
#include "startle/log.h"
#include "io.h"
#include "cgen/primitives.h"

static int mem[256];
static int *mem_ptr = mem;
static char strings[1 << 22]; // ***
static char *strings_ptr = strings;
static char string_buffer[64];

#if INTERFACE
typedef int symbol_t;
typedef int any_t;
typedef void *opaque_t;

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
  assert_error(mem_ptr - mem + n <= (int)LENGTH(mem), "out of mem");
  array arr = {
    .capacity = n,
    .elem = mem_ptr
  };
  mem_ptr += n;
  return arr;
}

char *string_alloc(unsigned int n) {
  assert_error(strings_ptr - strings + n <= (int)LENGTH(strings),
               "out of mem (%d requested, %d available)",
               n, *(&strings+1) - strings_ptr);
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

#define __primitive_add_iii(x, y) x + y
#define __primitive_sub_iii(x, y) x - y
#define __primitive_mul_iii(x, y) x * y
#define __primitive_eq_yii(x, y) x == y;
#define __primitive_eq_s_yyy(x, y) x == y;
#define __primitive_neq_yii(x, y) x != y;
#define __primitive_neq_s_yyy(x, y) x != y;
#define __primitive_gt_yii(x, y) x > y
#define __primitive_gte_yii(x, y) x >= y
#define __primitive_lt_yii(x, y) x < y
#define __primitive_lte_yii(x, y) x <= y
#define __primitive_not_yy(x) !x
#define __primitive_bitand_iii(x, y) x & y
#define __primitive_bitor_iii(x, y) x | y
#define __primitive_bitxor_iii(x, y) x ^ y
#define __primitive_shiftl_iii(x, y) x << y
#define __primitive_shiftr_iii(x, y) x >> y
#define __primitive_complement_ii(x) ~x

// aliases
#define __primitive_ap01_lLi __primitive_ap01
#define __primitive_ap01_lli __primitive_ap01
#define __primitive_ap02_lLii __primitive_ap02
#define __primitive_ap02_llii __primitive_ap02
#define __primitive_pushr1_lli __primitive_pushr1
#define __primitive_pushr1_lLi __primitive_pushr1
#define __primitive_pushr2_llii __primitive_pushr2
#define __primitive_pushr2_lLii __primitive_pushr2

#define __primitive_ap01_lLa __primitive_ap01
#define __primitive_ap01_lla __primitive_ap01
#define __primitive_ap02_lLaa __primitive_ap02
#define __primitive_ap02_llaa __primitive_ap02
#define __primitive_ap02_lla0(i0, r, o0, o1)  __primitive_ap02_lli0(i0, r, o0)
#define __primitive_pushr1_lla __primitive_pushr1
#define __primitive_pushr1_lLa __primitive_pushr1
#define __primitive_pushr2_llaa __primitive_pushr2
#define __primitive_pushr2_lLaa __primitive_pushr2

#define __primitive_from_string_iS __primitive_from_string_is
#define __primitive_eq_str_ySS __primitive_eq_str_yss
#define __primitive_eq_str_ySs __primitive_eq_str_yss
#define __primitive_read_yyooS __primitive_read_yyoos
#define __primitive_strsplit_sSsS __primitive_strsplit_ssss
#define __primitive_open_yySo __primitive_open_yyso
#define __primitive_strcat_sSs __primitive_strcat_sss

#endif

bool __primitive_div_iii(int x, int y, int *res) {
  if(y == 0) {
    return true;
  } else {
    if(res) *res = x / y;
    return false;
  }
}

bool __primitive_mod_iii(int x, int y, int *res) {
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
  if(ret) {
    arr_shift(&arr, 0, 1);
    *ret = arr;
  }
  return false;
}

bool __primitive_ap02(array arr, array *ret, int *out1, int *out0) {
  if(arr.size < 2) return true;
  if(out0) *out0 = *arr_elem(&arr, 0);
  if(out1) *out1 = *arr_elem(&arr, 1);
  if(ret) {
    arr_shift(&arr, 0, 2);
    *ret = arr;
  }
  return false;
}

bool __primitive_ap02_lli0(array arr, array *ret, int *out1) {
  if(arr.size < 2) return true;
  *out1 = *arr_elem(&arr, 1);
  arr_shift(&arr, 0, 2);
  *ret = arr;
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

array __primitive_quote0_li(int in0) {
  array arr = arr_new();
  arr_shift(&arr, 1, 0);
  *arr_elem(&arr, 0) = in0;
  return arr;
}

seg_t __primitive_to_string_si(int x) {
  unsigned int len = max(0, snprintf(string_buffer,sizeof(string_buffer), "%d", x));
  return seg_alloc(string_buffer, min(sizeof(string_buffer), len));
}

TEST(prim_to_string) {
  init_primitives();
  seg_t s = __primitive_to_string_si(42);
  return segcmp("42", s) == 0 ? 0 : -1;
}

symbol_t __primitive_open_yyso(symbol_t io, seg_t name, void **fo) {
  file_t *f = io_open(name);
  assert_error(f, "open error");
  *fo = f;
  return io;
}

symbol_t __primitive_close_yyo(symbol_t io, void *fi) {
  io_close((file_t *)fi);
  return io;
}

symbol_t __primitive_write_yyoso(symbol_t io, void *fi, seg_t str, void **fo) {
  io_write((file_t *)fi, str);
  *fo = fi;
  return io;
}

symbol_t __primitive_unread_yyoso(symbol_t io, void *fi, seg_t str, void **fo) {
  io_unread((file_t *)fi, str);
  *fo = fi;
  return io;
}

symbol_t __primitive_read_yyoos(symbol_t io, void *fi, void **fo, seg_t *str) {
  *str = io_read((file_t *)fi);
  *fo = fi;
  return io;
}

seg_t __primitive_strtrim_ss(seg_t str) {
  return seg_trim(str);
}

bool __primitive_from_string_is(seg_t str, int *x) {
  char *end = NULL;
  long lx = strtol(str.s, &end, 0);
  if(!end || end <= str.s) {
    return true;
  } else {
    *x = lx;
    return false;
  }
}

symbol_t __primitive_eq_str_yss(seg_t a, seg_t b) {
  return a.n == b.n && strncmp(a.s, b.s, a.n) == 0;
}

seg_t __primitive_strcat_sss(seg_t a, seg_t b) {
  char *s = string_alloc(a.n + b.n);
  memcpy(s, a.s, a.n);
  memcpy(s + a.n, b.s, b.n);
  return (seg_t) {
    .s = s,
    .n = a.n + b.n
  };
}

bool __primitive_strsplit_ssss(seg_t in, seg_t delim, seg_t *pre, seg_t *post) {
  const char *s = seg_find(in, delim);
  if(!s) return true;
  *pre = (seg_t)  { .s = in.s,        .n = s - in.s };
  *post = (seg_t) { .s = s + delim.n, .n = in.n - pre->n - delim.n };
  return false;
}
