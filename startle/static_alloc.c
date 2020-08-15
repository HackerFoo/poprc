/* Copyright 2012-2020 Dustin DeWeese
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

#include <stdlib.h>
#include <string.h>
#include "rt_types.h"

#include "startle/test.h"
#include "startle/support.h"
#include "startle/static_alloc.h"

#include "cells.h"
#include "rt.h"

#if INTERFACE
#define static_sizeof(name) (sizeof(*name) * name##_size)
#endif

#define STATIC_ALLOC__ITEM(file, line, name, type, default_size) STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, __alignof__(type))

#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  type *name = NULL;                                                    \
  size_t name##_size = 0;
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM

static char *__mem = NULL;
static size_t __mem_size = 0;

// alignment (a) must be a power of two
size_t align_offset(size_t n, int a) {
  int r = n & (a - 1);
  return r ? a - r : 0;
}

TEST(align_offset) {
  if(align_offset(3, 4) != 1) return -1;
  if(align_offset(4, 4) != 0) return -2;
  return 0;
}

static
void alloc_all() {
  __mem_size = 0;
  unsigned int __max_align = 1;

  // set sizes for non-dependent allocations
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment)       \
  name##_size = (default_size);
#define STATIC_ALLOC_DEPENDENT__ITEM(...)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM

  // set sizes for dependent allocations
#define STATIC_ALLOC_ALIGNED__ITEM(...)
#define STATIC_ALLOC_DEPENDENT__ITEM(file, line, name, type, default_size) \
  name##_size = (default_size);
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM

  // calculate total size
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  __mem_size += align_offset(__mem_size, alignment);                    \
  __mem_size += name##_size * sizeof(type);                             \
  __max_align = max(__max_align, alignment);
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM

  // allocate block and clear
  __mem = malloc(__max_align + __mem_size);
  __mem += align_offset((uintptr_t)__mem, __max_align);
  memset(__mem, 0, __mem_size);

  size_t __offset = 0;

  // assign aligned allocation
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  __offset += align_offset(__offset, alignment);                        \
  name = (type *)(__mem + __offset);                                    \
  __offset += sizeof(type) * name##_size;
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
}

static
void free_all() {
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment)   \
  name##_size = 0;
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
  free(__mem);
  __mem_size = 0;
}

void static_alloc_init() {
  if(__mem_size) free_all();
  alloc_all();
}

size_t get_mem_size() {
  return __mem_size;
}

void list_static_sizes() {
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  printf(#type " " #name "[%ld] __attribute__ ((aligned (%d)));" \
         " // " #file ".c:" #line ", %ld bytes\n", \
         name##_size, (int)alignment, name##_size * sizeof(type));
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
}
