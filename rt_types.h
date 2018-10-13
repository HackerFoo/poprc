/* Copyright 2012-2018 Dustin DeWeese
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
#include <stddef.h>
#include <time.h>

#include "startle/macros.h"
#include "startle/types.h"
#include "macros.h"

typedef unsigned int uint;

typedef uint16_t csize_t;

typedef enum __attribute__((packed)) type_t {
  T_ANY = 0, /**< type variable */
  T_INT,
  T_LIST, /**< a list/quote */
  T_SYMBOL,
  T_MAP,
  T_STRING,
  T_RETURN, /**< a list of return values, trace ONLY */
  T_FLOAT,
  T_MODULE, /**< module definition ONLY */
  T_BOTTOM /**< the uninhabited type */
} type_t;

typedef struct cell cell_t;
typedef struct expr expr_t;
typedef struct value value_t;
typedef struct tok_list tok_list_t;
typedef struct entry entry_t;
typedef struct mem mem_t;
typedef struct context context_t;

typedef uintptr_t alt_set_t;
typedef int16_t refcount_t;
typedef intptr_t val_t;

#ifdef __clang__
#pragma clang diagnostic ignored "-Warray-bounds"
#pragma clang diagnostic ignored "-Wzero-length-array"
#pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#pragma clang diagnostic ignored "-Wnested-anon-types"
#pragma clang diagnostic ignored "-Wgnu-folding-constant"
#pragma clang diagnostic ignored "-Wgnu-empty-initializer"
#pragma clang diagnostic ignored "-Wextended-offsetof"
#endif

struct context {
  context_t *up;
  cell_t *src;
  val_t expected_value;
  alt_set_t alt_set;
  int priority;
  csize_t in, out;
  type_t t;
  uint8_t pos;
  bool retry;
  bool delay_assert;
  bool delay_var;
  bool expected;
};

typedef enum response {
  SUCCESS = 0,
  DELAY,
  RETRY,
  FAIL
} response;

#define EXPR_NEEDS_ARG 0x02
#define EXPR_RECURSIVE 0x04
#define EXPR_TRACE     0x08
#define EXPR_NO_UNIFY  0x10

/* unevaluated expression */
struct __attribute__((packed)) expr {
  uint8_t out, flags;
  union {
    cell_t *arg[2];
    val_t idx[2];
    struct {
      cell_t *arg0;
      alt_set_t alt_set;
    };
  };
};

// value flags
#define VALUE_DEP        0x04
#define VALUE_CHANGES    0x08
#define VALUE_ROW        0x10
#define VALUE_TRACED     0x20
#define VALUE_FAIL       0x40
#define VALUE_VAR        0x80

// trace flags
#define TRACE_INCOMPLETE 0x08
#define TRACE_TRACED     0x20

/* reduced value */
struct __attribute__((packed)) value {
  type_t type;
  uint8_t flags;
  alt_set_t alt_set;
  union {
    struct {
      cell_t *var; /* variable */
      union {
        val_t integer;  /* integer */
        double flt;     /* float */
        seg_t str;      /* string */
        cell_t *ptr[2]; /* list */
      };
    };
    pair_t map[1]; /* map */
  };
};

/* token list */
struct __attribute__((packed)) tok_list {
  csize_t length;
  const char *location, *line;
  cell_t *next;
};

/* unallocated memory */
struct __attribute__((packed)) mem {
  csize_t __padding;
  cell_t *prev, *next;
};

#define ENTRY_PRIMITIVE 0x01
#define ENTRY_TRACE     0x02
#define ENTRY_RECURSIVE 0x04
#define ENTRY_QUOTE     0x08
#define ENTRY_ROW       0x10
#define ENTRY_MOV_VARS  0x20
#define ENTRY_COMPLETE  0x80

typedef struct wrap_data {
  cell_t *initial;
  cell_t *expand;
  uintptr_t dep_mask;
} wrap_data;

/* word entry */
struct __attribute__((packed)) entry {
  uint8_t rec, flags, alts, sub_id;
  csize_t in, out, len;
  cell_t *parent;
  wrap_data *wrap;
};

typedef enum char_class_t {
  CC_NONE,
  CC_NUMERIC,
  CC_FLOAT,
  CC_ALPHA,
  CC_SYMBOL,
  CC_BRACKET,
  CC_VAR,
  CC_COMMENT,
  CC_DOT
} char_class_t;


// define the op enum
#define OP__ITEM(name) \
  OP_##name,

typedef enum __attribute__((packed)) op {
  OP__ITEM(null)
  #include "op_list.h"
  OP_COUNT
} op;

#undef OP__ITEM

struct __attribute__((packed, aligned(4))) cell {
  /* op indicates the type:
   * OP_null      -> mem
   * OP_value     -> value
   * otherwise    -> expr
   */
  union {
    uintptr_t raw[8];
    struct {
      union {
        cell_t *alt;
        const char *word_name; // entry
      };
      union {
        cell_t *tmp;
        const char *module_name; // entry
        char_class_t char_class; // tok_list
        struct { // trace
          type_t type;
          uint8_t flags;
        } trace;
      };
      op op;
      uint8_t pos;
      refcount_t n;
      csize_t size;
      union {
        expr_t expr;
        value_t value;
        tok_list_t tok_list;
        entry_t entry;
        mem_t mem;
      };
    };
  };
};

#define LIST_OFFSET ((offsetof(cell_t, value.ptr) - offsetof(cell_t, expr.arg)) / sizeof(cell_t *))

#ifndef EMSCRIPTEN
static_assert(sizeof(cell_t) == sizeof_field(cell_t, raw), "cell_t wrong size");
#endif

//static_assert(offsetof(cell_t, expr.arg[1]) == offsetof(cell_t, value.ptr[0]), "second arg not aliased with first ptr"); // what relies on this?
static_assert(offsetof(cell_t, expr.flags) == offsetof(cell_t, value.flags), "expr.flags should alias value.flags");

typedef struct stats_t {
  unsigned int reduce_cnt, fail_cnt, alloc_cnt, max_alloc_cnt;
  unsigned int current_alloc_cnt;
  clock_t start, stop;
  uint8_t alt_cnt;
} stats_t;

#ifdef EMSCRIPTEN
#define strnlen(s, n) strlen(s)
#endif

// Maximum number of alts
#define AS_SIZE (sizeof(alt_set_t) * 4)
#define AS_MASK ((alt_set_t)0x5555555555555555)
#define ALT_SET_IDS AS_SIZE

#define SYM_False 0
#define SYM_True  1
#define SYM_IO    2
#define SYM_Dict  3

#define PERSISTENT ((refcount_t)-15)

#define PRIMITIVE_MODULE_PREFIX __primitive
#define PRIMITIVE_MODULE_NAME STRINGIFY(PRIMITIVE_MODULE_PREFIX)

typedef struct list_iterator {
  cell_t **array;
  csize_t index, size;
  bool row;
} list_iterator_t;

void breakpoint();

#define COMMAND(name, desc) void command_##name(UNUSED cell_t *rest)

#endif
