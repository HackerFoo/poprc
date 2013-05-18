#ifndef __RT_TYPES__
#define __RT_TYPES__

typedef enum {
  T_INT = 0,
  T_PTR,
  T_FAIL
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
    reduce_t *func;
    cell_t *prev;
  };
  cell_t *alt;
  uint32_t n;
  union {
    /* unevaluated */
    cell_t *arg[4];
    /* reduced */
    struct __attribute__((packed)) {
      uintptr_t alt_set;
      uintptr_t type;
      cell_t *next;
      union {
	intptr_t val;
	cell_t *ptr;
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
  CC_SYMBOL
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

#endif
