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
typedef struct stack_frame stack_frame_t;

typedef struct stack_frame {
  stack_frame_t *up;
  cell_t *cell;
} stack_frame_t;

typedef bool (reduce_t)(cell_t *cell);
struct __attribute__((packed)) cell {
  union {
    struct __attribute__((packed)) {
      reduce_t *func;
      cell_t *alt;
    };
    struct __attribute__((packed)) {
      cell_t *prev;
      cell_t *next;
    };
  };
  uint32_t n;
  union {
    /* unevaluated */
    cell_t *arg[3];
    /* reduced */
    struct __attribute__((packed)) {
      uintptr_t alt_set;
      union {
	/* value */
	struct __attribute__((packed)) {
	  uint32_t type, val_size;
	  intptr_t val[1];
	};
	/* list */
	struct __attribute__((packed)) {
	  cell_t *ptr[2];
	};
      };
    };
  };
};

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

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#define alloca_copy(c, n)		\
  (ref_ptrs(c),	n = closure_cells(c),	\
    memcpy(alloca(sizeof(cell_t) * n),	\
	   c, sizeof(cell_t) * n))

#endif
