/* Copyright 2012-2013 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __RT_TYPES__
#define __RT_TYPES__

#include <stdbool.h>
#include <stdint.h>
#include <time.h>

typedef enum {
  T_FAIL = 1,
  T_INDIRECT,
  T_INT
} type_t;

typedef struct cell cell_t;

typedef uintptr_t alt_set_t;

#ifdef __clang__
#pragma clang diagnostic ignored "-Warray-bounds"
#endif
typedef bool (reduce_t)(cell_t **cell);
struct __attribute__((packed)) cell {
  struct __attribute__((packed)) {
    reduce_t *func;
    cell_t *alt;
    cell_t *tmp;
  };
  uintptr_t n;
  union {
    /* unevaluated */
    cell_t *arg[3];
    /* reduced */
    struct __attribute__((packed)) {
      alt_set_t alt_set;
      union {
	/* value */
	struct __attribute__((packed)) {
	  /* type must overlap low bytes of ptr[0] */
	  uint32_t type, val_size;
	  intptr_t val[1];
	};
	/* list */
	struct __attribute__((packed)) {
	  cell_t *ptr[2];
	};
	struct __attribute__((packed)) {
	  cell_t *prev, *next;
	};
      };
    };
  };
} __attribute__((aligned(4)));

typedef struct word_entry_t {
  char name[64];
  reduce_t *func;
  unsigned int in, out;
} word_entry_t;

typedef struct parse_tok_t {
  cell_t *c;
  unsigned int out;
} parse_tok_t;

typedef enum char_class_t {
  CC_NONE,
  CC_NUMERIC,
  CC_ALPHA,
  CC_SYMBOL,
  CC_BRACKET
} char_class_t;

#define sizeof_field(s, f) sizeof(((s *)0)->f)

typedef struct measure_t {
  unsigned int reduce_cnt, alloc_cnt, max_alloc_cnt;
  signed int current_alloc_cnt;
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
#define PTRS 4

#endif
