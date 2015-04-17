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

#include "rt_types.h"
#include <stdio.h>
#include <string.h>
#include "linenoise/linenoise.h"
#include "gen/rt.h"
#include "gen/test.h"
#include "gen/eval.h"
#include "gen/primitive.h"

int test_alloc(UNUSED char *name) {
  unsigned int i, j;
  cell_t *a[30];
  for(j = 0; j < 50; j++) {
    for(i = 0; i < LENGTH(a); i++) {
      a[i] = func(func_add, 9, 1);
    }
    for(i = 0; i < LENGTH(a); i++) {
      closure_free(a[i]);
    }
  }
  return check_free() ? 0 : -1;
}
static TEST(test_alloc);

bool check_free() {
  unsigned int i;
  bool leak = false;
  for(i = 0; i < LENGTH(cells); i++) {
    if(is_closure(&cells[i])) {
      printf("LEAK: %d (%ld)\n", i, (long int)cells[i].n);
      leak = true;
    }
  }
  FOREACH(alt_live, i) {
    if(alt_live[i]) {
      printf("ALT LEAK: %d (%ld)\n", i, (long int)alt_live[i]);
      leak = true;
    }
  }
  return !leak;
}
