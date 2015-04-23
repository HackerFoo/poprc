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

typedef uint16_t type_t;

#define T_ANY       0x0000
#define T_INT       0x0001
#define T_IO        0x0002
#define T_LIST      0x0003
#define T_TRACED    0x0800
#define T_FAIL      0x1000
#define T_INDIRECT  0x2000
#define T_ROW       0x4000
#define T_VAR       0x8000
#define T_EXCLUSIVE 0x00FF

typedef type_t type_rep_t;

typedef struct cell cell_t;

typedef uintptr_t alt_set_t;

#ifdef __clang__
#pragma clang diagnostic ignored "-Warray-bounds"
#pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#pragma clang diagnostic ignored "-Wnested-anon-types"
#endif
typedef bool (reduce_t)(cell_t **cell, type_rep_t type);
struct __attribute__((packed)) cell {
  reduce_t *func;
  cell_t *alt;
  cell_t *tmp;
  uint32_t n;
  uint16_t size;
  union {
    uint16_t type;
    uint16_t out;
  };
  union {
    /* unevaluated */
    cell_t *arg[3];
    /* reduced */
    struct __attribute__((packed)) {
      alt_set_t alt_set;
      union {
        intptr_t val[2]; /* value */
        cell_t *ptr[2];  /* list */
      };
    };
    /* unallocated */
    struct __attribute__((packed)) {
      cell_t *prev, *next;
    };
  };
} __attribute__((aligned(4)));

typedef struct word_entry_t {
  char name[64];
  reduce_t *func;
  unsigned int in, out;
} word_entry_t;

typedef struct builder_entry_t {
  char name[64];
  void *func;
  unsigned int in, out;
} builder_entry_t;

typedef struct parse_tok_t {
  cell_t *c;
  unsigned int out;
} parse_tok_t;

typedef enum char_class_t {
  CC_NONE,
  CC_NUMERIC,
  CC_ALPHA,
  CC_SYMBOL,
  CC_BRACKET,
  CC_VAR
} char_class_t;

#define sizeof_field(s, f) sizeof(((s *)0)->f)

typedef struct measure_t {
  unsigned int reduce_cnt, alloc_cnt, max_alloc_cnt;
  unsigned int current_alloc_cnt;
  clock_t start, stop;
  uint8_t alt_cnt;
} measure_t;

#define zero(a) bzero((a), sizeof(a))

#define show(x) printf(#x " = %d\n", (int)(x))
#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))
#define FOREACH(a, i) for(i = 0; i < LENGTH(a); i++)
#define sizeof_field(s, f) sizeof(((s *)0)->f)

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#define is_marked(p, x) (((intptr_t)(p) & (x)) != 0)
#define mark_ptr(p, x) ((void *)((intptr_t)(p) | (x)))
#define clear_ptr(p, x) ((void *)((intptr_t)(p) & ~(x)))

#define ALT 1
#define ARGS 2
#define ARGS_IN 4
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
  tt_compose_placeholders
} trace_type_t;

#ifdef EMSCRIPTEN
#define strnlen(s, n) strlen(s)
#endif

#define UNUSED __attribute__((unused))

// declare a test function
// must preceed with 'static' and follow with semicolon
// to work with makeheaders
// NOT PORTABLE
#define TEST(func)                                       \
  __attribute__((used,section("__TEXT,__tests")))        \
  struct __test_entry __test_entry_##func =  {           \
    &(func),                                             \
    #func                                                \
  }

struct __test_entry {
  int (*func)(char *name);
  char *name;
};

#endif
