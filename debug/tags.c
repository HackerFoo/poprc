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

#include "rt_types.h"
#include "startle/support.h"
#include "startle/map.h"

// Provides a way to assign string tags to pointers, which is useful
// to understand what things are when debugging.

STATIC_ALLOC(tags_map, pair_t, 256);

#if INTERFACE
#define TAG_PTR(ptr, str)                       \
  ({                                            \
    __typeof__(ptr) __ptr = ptr;                \
    set_ptr_tag(__ptr, str);                    \
    __ptr;                                      \
  })
#endif

void set_ptr_tag(const void *ptr, const char *str) {
  pair_t p = { (uintptr_t)ptr, (uintptr_t)str };
  map_insert(tags_map, p);
}

// TODO: a way to remove tags

const char *get_ptr_tag(const void *ptr) {
  pair_t *p = map_find(tags_map, (uintptr_t)ptr);
  if(p) {
    return (const char *)p->second;
  } else {
    return NULL;
  }
}

void clear_ptr_tags() {
  init_map(tags_map, tags_map_size);
}
