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

// declare pointers to static allocations
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, ...)         \
  type *name = NULL;                                                    \
  size_t name##_size = 0;                                               \
  size_t name##_size_init = 0;
#define STATIC_ALLOC_DEPENDENT__ITEM(file, line, name, type, ...)       \
  type *name = NULL;                                                    \
  size_t name##_size = 0;
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM

static char *__alloc = NULL;
static char *__mem = NULL;
static size_t __mem_size = 0;

// determine maximum name size
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  char name[sizeof(#name)];
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
union static_alloc_names {
#include "static_alloc_list.h"
};
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
#define STATIC_ALLOC_NAME_SIZE sizeof(union static_alloc_names)

// create allocation table
typedef struct {
  char name[STATIC_ALLOC_NAME_SIZE];
  char *type_name;
  size_t offset;
  size_t width;
} allocation_t;
static allocation_t allocation_table[] = {
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, _name, _type, ...) \
  { .name = #_name, .type_name = #_type },
#define STATIC_ALLOC_DEPENDENT__ITEM(...) STATIC_ALLOC__ITEM(__VA_ARGS__)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
};

// alignment (a) must be a power of two
size_t align_offset(size_t n, int a) {
  int m = a - 1;
  return (a - (n & m)) & m;
}

TEST(align_offset) {
  if(align_offset(0, 4) != 0) return -1;
  if(align_offset(1, 4) != 3) return -2;
  if(align_offset(2, 4) != 2) return -3;
  if(align_offset(3, 4) != 1) return -4;
  if(align_offset(4, 4) != 0) return -5;
  return 0;
}

static
void load_default_sizes() {
  // set sizes for non-dependent allocations
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment)       \
  name##_size_init = (default_size);
#define STATIC_ALLOC_DEPENDENT__ITEM(...)
#include "static_alloc_list.h"
#undef STATIC_ALLOC_ALIGNED__ITEM
#undef STATIC_ALLOC_DEPENDENT__ITEM
}

static
void alloc_all() {
  __mem_size = 0;
  unsigned int __max_align = 1;
  unsigned int __i = 0;

  // set sizes for non-dependent allocations
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment)       \
  name##_size = name##_size_init;
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
  __alloc = malloc(__max_align + __mem_size);
  __mem = __alloc + align_offset((uintptr_t)__mem, __max_align);
  memset(__mem, 0, __mem_size);

  size_t __offset = 0;

  // assign aligned allocation
#define STATIC_ALLOC_ALIGNED__ITEM(file, line, name, type, default_size, alignment) \
  __offset += align_offset(__offset, alignment);                        \
  name = (type *)(__mem + __offset);                                    \
  allocation_table[__i].offset = __offset;                              \
  allocation_table[__i].width = sizeof(type);                           \
  __i++;                                                                \
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
  free(__alloc);
  __mem_size = 0;
}

void static_alloc_reinit() {
  if(__mem_size) free_all();
  alloc_all();
}

void static_alloc_init() {
  load_default_sizes();
  static_alloc_reinit();
}

size_t get_mem_size() {
  return __mem_size;
}

// list information about static allocations
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

// identify a pointer into __mem
void print_static_alloc(const void *_p) {
  const char *p = (const char *)_p;
  if(!__mem_size ||
     p < __mem ||
     p > __mem + __mem_size) {
    printf("unknown\n");
  } else {
    size_t offset = p - __mem;
    allocation_t *res = NULL;
    FOREACH(i, allocation_table) {
      allocation_t *a = &allocation_table[i];
      if(a->offset > offset) break;
      res = a;
    }
    printf("%s[%ld]\n", res->name, (offset - res->offset) / res->width);
  }
}

STATIC_ALLOC(static_address_buffer, char, 4096);
char *list_static_addresses() {
  char *end = static_address_buffer + static_address_buffer_size - 1;
  char *ptr = static_address_buffer;
  FOREACH(i, allocation_table) {
    allocation_t *e = &allocation_table[i];
    int n = snprintf(ptr, end - ptr, "%s *$%s = (%s *)%p\n", e->type_name, e->name, e->type_name, (void *)&__mem[e->offset]);
    if(n > 0) ptr += n;
  }
  *ptr = '\0';
  return static_address_buffer;
}
