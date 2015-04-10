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

#ifndef __ALLOCA_CELLS__
#define __ALLOCA_CELLS__

#include <alloca.h>
#include <string.h>
#include "gen/rt.h"

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
  return traverse_ref(new, PTRS);
}

inline __attribute__((always_inline))
cell_t *alloca_copy_if(cell_t *c, bool s) {
  if(s) return alloca_copy(c);
  else return alloca_cells(1);
}

#endif
