#ifndef __ALLOCA_CELLS__
#define __ALLOCA_CELLS__

#include <alloca.h>
#include <string.h>
#include "rt.h"

inline __attribute__((always_inline))
cell_t *alloca_cells(int n) { 
  size_t s = sizeof(cell_t) * n + sizeof(void *);
  cell_t *c = alloca(s);
  memset(c, 0, s);
  return c;
}

inline __attribute__((always_inline))
cell_t *alloca_copy(cell_t *c) {
  int n = closure_cells(c);
  cell_t *new = alloca_cells(n);
  memcpy(new, c, sizeof(cell_t) * n);
  return ref_reduced(new);
}

inline __attribute__((always_inline))
cell_t *alloca_copy_if(cell_t *c, bool s) {
  if(s) return alloca_copy(c);
  else return alloca_cells(1);
}

#endif
