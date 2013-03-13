#ifndef __RT_TYPES__
#define __RT_TYPES__

typedef struct cell cell_t;
typedef bool (reduce_t)(cell_t *cell);
#define FUNC(x) bool func_##x(cell_t *c)
struct __attribute__((packed)) cell {
  union {
    reduce_t *func;
    cell_t *prev;
  };
  cell_t *alt;
  union {
    /* unevaluated */
    cell_t *arg[3];
    /* ref */
    struct {
      intptr_t n;
      union {
	intptr_t val;
	cell_t *ptr;
      };
      cell_t *next;
    };
  };
};

#define MK_APPEND(fname, field)			\
  cell_t *fname(cell_t *a, cell_t *b);

#endif
