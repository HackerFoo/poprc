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
  T_OPAQUE,
  T_MODULE, /**< module definition ONLY */
  T_BOTTOM, /**< the uninhabited type */
  T_FAIL // TODO use T_BOTTOM
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
#endif

typedef struct qsize {
  csize_t in, out;
} qsize_t;

struct context {
  context_t *up;
  cell_t *src;
  val_t expected_value;
  alt_set_t alt_set;
  int priority;
  qsize_t s;
  type_t t;
  uint8_t pos;
  bool retry;
  bool expected;
  bool inv;
};

typedef enum response {
  SUCCESS = 0,
  DELAY,
  RETRY,
  FAIL
} response;

#define TR_FINAL 0x01

typedef struct tr tr;
struct __attribute__((packed)) tr {
  union {
    cell_t *ptr;
    int entry;
    struct {
      int16_t index;
      uint8_t flags;
    };
  };
};

#define FLAG_expr (expr, EXPR)
#define EXPR_NEEDS_ARG 0x02
#define EXPR_TRACE     0x04
#define EXPR_DELAYED   0x08
#define EXPR_PARTIAL   0x10
#define EXPR_RECURSIVE 0x20 // exec
#define EXPR_NO_UNIFY  0x40 // exec
#define EXPR_ROW       0x80 // placeholder

/* unevaluated expression */
struct __attribute__((packed)) expr {
  uint8_t out, flags;
  union {
    cell_t *arg[2];
    val_t idx[2];
    struct {
      cell_t *arg0; // padding
      alt_set_t alt_set;
    };
  };
};

// value flags
#define FLAG_value (value, VALUE)
#define VALUE_TRACED     0x02
#define VALUE_DEP        0x08
#define VALUE_LOCAL      0x10
#define VALUE_LINEAR     0x20
#define VALUE_ROW        0x40
#define VALUE_VAR        0x80

// trace flags
#define FLAG_trace (trace, TRACE)
#define TRACE_INCOMPLETE 0x01
#define TRACE_TRACED     0x02
#define TRACE_USED       0x04
#define TRACE_IMMEDIATE  0x08
#define TRACE_CHANGES    0x10
#define TRACE_DECL       0x20
#define TRACE_NO_SKIP    0x40

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
        char str[0];    /* string */
        cell_t *ptr[2]; /* list */
        void *opaque;   /* opaque */
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

#define FLAG_entry (entry, ENTRY)
#define ENTRY_PRIMITIVE 0x01
#define ENTRY_TRACE     0x02
#define ENTRY_QUOTE     0x08
#define ENTRY_PARTIAL   0x10
#define ENTRY_MOV_VARS  0x20
#define ENTRY_BLOCK     0x40
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
  union {
    wrap_data *wrap;
    cell_t *compact;
  };
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
  CC_DOT,
  CC_STRING
} char_class_t;


// define the op enum
#define OP__ITEM(file, line, name)              \
  OP_##name,

typedef enum __attribute__((packed)) op {
  OP__ITEM(FILEBASE, __LINE__, null)
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
        val_t tmp_val;
        const char *module_name; // entry
        char_class_t char_class; // tok_list
        struct { // trace
          type_t type;
          uint8_t flags;
          union {
            csize_t prev_cells;
            csize_t extension;
          };
        } trace;
      };
      op op;
      union {
        uint8_t pos;
        uint8_t priority; // for use in func_list() & delay_branch()
      };
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

#define VALUE_OFFSET(f) ((offsetof(cell_t, value.f) - offsetof(cell_t, expr.arg)) / sizeof(cell_t *))

#ifndef EMSCRIPTEN
static_assert(sizeof(cell_t) == sizeof_field(cell_t, raw), "cell_t wrong size");
#endif

static_assert(sizeof(tr) == sizeof_field(cell_t, expr.arg[0]), "tr wrong size");

#define ASSERT_ALIAS(s, f0, f1) \
  static_assert(offsetof(s, f0) == offsetof(s, f1), #f0 " should alias " #f1 " in " #s)

#define ASSERT_VALUE_OFFSET(f) ASSERT_ALIAS(cell_t, expr.arg[VALUE_OFFSET(f)], value.f)

ASSERT_VALUE_OFFSET(integer);
ASSERT_VALUE_OFFSET(flt);
ASSERT_VALUE_OFFSET(str);
ASSERT_VALUE_OFFSET(ptr);
ASSERT_VALUE_OFFSET(map);

ASSERT_ALIAS(cell_t, expr.flags, value.flags);
ASSERT_ALIAS(cell_t, expr.arg[1], expr.alt_set);

typedef struct stats_t {
  int reduce_cnt, fail_cnt, alloc_cnt, max_alloc_cnt, trace_cnt;
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

#define SYM_False     0
#define SYM_True      1
#define SYM_IO        2
#define SYM_Dict      3
#define SYM_Something 4

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
