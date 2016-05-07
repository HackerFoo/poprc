/* Copyright 2012-2015 Dustin DeWeese
   This file is part of PoprC.

    PoprC is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PoprC is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PoprC.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __RT_TYPES__
#define __RT_TYPES__

#include <stdbool.h>
#include <stdint.h>
#include <time.h>

typedef uint16_t type_or_csize_t;
typedef type_or_csize_t type_t;
typedef type_or_csize_t csize_t;

#define T_ANY       0x0000
#define T_INT       0x0001
#define T_IO        0x0002
#define T_LIST      0x0003
#define T_SYMBOL    0x0004
#define T_TRACED    0x0800
#define T_FAIL      0x1000
#define T_INDIRECT  0x2000
#define T_ROW       0x4000
#define T_VAR       0x8000
#define T_EXCLUSIVE 0x00FF

typedef struct cell cell_t;
typedef struct expr expr_t;
typedef struct value value_t;
typedef struct mem mem_t;

typedef uintptr_t alt_set_t;
typedef uint32_t refcount_t;
typedef intptr_t val_t;

#ifdef __clang__
#pragma clang diagnostic ignored "-Warray-bounds"
#pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#pragma clang diagnostic ignored "-Wnested-anon-types"
#pragma clang diagnostic ignored "-Wgnu-folding-constant"
#endif
typedef bool (reduce_t)(cell_t **cell, type_t type);

/* unevaluated expression */
struct __attribute__((packed)) expr {
  csize_t out;
  cell_t *arg[3];
} __attribute__((aligned(4)));

/* reduced value */
struct __attribute__((packed)) value {
  type_t type;
  alt_set_t alt_set;
  union {
    val_t integer[2]; /* integer */
    cell_t *ptr[2];   /* list */
  };
} __attribute__((aligned(4)));

/* unallocated memory */
struct __attribute__((packed)) mem {
  cell_t *prev, *next;
} __attribute__((aligned(4)));

struct __attribute__((packed)) cell {
  /* func indicates the type:
   * NULL         -> mem
   * func_reduced -> value
   * cell         -> extension
   * otherwise    -> expr
   */
  reduce_t *func;
  cell_t *alt;
  cell_t *tmp;
  refcount_t n;
  csize_t size;
  union {
    expr_t expr;
    value_t value;
    mem_t mem;
  };
} __attribute__((aligned(4)));

typedef struct word_entry_t {
  char name[64];
  reduce_t *func;
  csize_t in, out;
  cell_t *data;
} word_entry_t;

typedef struct builder_entry_t {
  char name[64];
  void *func;
  csize_t in, out;
} builder_entry_t;

typedef struct parse_tok_t {
  cell_t *c;
  csize_t out;
} parse_tok_t;

typedef enum char_class_t {
  CC_NONE,
  CC_NUMERIC,
  CC_ALPHA,
  CC_SYMBOL,
  CC_BRACKET,
  CC_VAR,
  CC_COMMENT
} char_class_t;

#define sizeof_field(s, f) sizeof(((s *)0)->f)

typedef struct measure_t {
  unsigned int reduce_cnt, alloc_cnt, max_alloc_cnt;
  unsigned int current_alloc_cnt;
  clock_t start, stop;
  uint8_t alt_cnt;
} measure_t;

#define zero(a) memset((a), 0, sizeof(a))

#define show(x) printf(#x " = %d\n", (int)(x))
#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))
#define COUNTDOWN(i, n) if(n) for(size_t i = (n); i--; )
#define COUNTUP(i, n) for(size_t i = 0, __n = (n); i < __n; i++)
#define FOREACH(i, a) COUNTUP(i, LENGTH(a))
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)
#define UNIQUE CONCAT(__unique_, __LINE__)
#define LOOP(n) COUNTDOWN(UNIQUE, n)
#define sizeof_field(s, f) sizeof(((s *)0)->f)

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#ifdef EMSCRIPTEN
#define MARK_BIT (1<<31)
#else
#define MARK_BIT 2
#endif

#define is_marked(p) (((uintptr_t)(p) & (MARK_BIT)) != 0)
#define mark_ptr(p) ((void *)((uintptr_t)(p) | (MARK_BIT)))
#define clear_ptr(p) ((void *)((uintptr_t)(p) & ~(MARK_BIT)))

#define ALT 1
#define ARGS_IN 2
#define ARGS_OUT 4
#define ARGS (ARGS_IN | ARGS_OUT)
#define PTRS 8

typedef struct two_cells_t {
  cell_t *a, *b;
} two_cells_t;

typedef struct three_cells_t {
  cell_t *a, *b, *c;
} three_cells_t;

typedef struct four_cells_t {
  cell_t *a, *b, *c, *d;
} four_cells_t;

typedef enum trace_type_t {
  tt_reduction,
  tt_touched,
  tt_force,
  tt_select,
  tt_copy,
  tt_compose_placeholders,
  tt_placeholder_dep
} trace_type_t;

#ifdef EMSCRIPTEN
#define strnlen(s, n) strlen(s)
#endif

#define UNUSED __attribute__((unused))

// declare a test function
// must preceed with 'static' and follow with semicolon
// to work with makeheaders

#ifdef __MACH__
#define TEST(func)                                       \
  __attribute__((used,section("__TEXT,__tests")))        \
  struct __test_entry __test_entry_##func =  {           \
    &(func),                                             \
    #func                                                \
  }
#else
#define TEST(func)                                       \
  __attribute__((used,section(".tests")))                \
  struct __test_entry __test_entry_##func =  {           \
    &(func),                                             \
    #func                                                \
  }
#endif

struct __test_entry {
  int (*func)(char *name);
  char *name;
};

#define BITSET(name, size) uint8_t name[((size)+7)/8] = {0}
#define BITSET_INDEX(name, array) BITSET(name, LENGTH(array))

// Maximum number of alts
#define AS_SIZE (sizeof(alt_set_t) * 4)
#define ALT_SET_IDS AS_SIZE

// string segment
typedef struct seg_t {
  const char *s;
  size_t n;
} seg_t;

#define SYM_FALSE 0
#define SYM_TRUE  1
#define SYM_IO    2
#define SYM_DICT  3

#endif
