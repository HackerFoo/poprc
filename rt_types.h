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

typedef bool (reduce_t)(cell_t *cell, stack_frame_t *up);
#define FUNC(x) bool func_##x(cell_t *c, stack_frame_t *up)
struct __attribute__((packed)) cell {
  union {
    reduce_t *func;
    cell_t *prev;
  };
  cell_t *alt;
  uint32_t n;
  union {
    /* unevaluated */
    cell_t *arg[3];
    /* reduced */
    struct {
      union {
	intptr_t val;
	cell_t *ptr;
      };
      intptr_t type;
      cell_t *next;
    };
  };
};

#define MK_APPEND(fname, field)			\
  cell_t *fname(cell_t *a, cell_t *b);

#endif
