/* Copyright 2012-2016 Dustin DeWeese
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
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/byte_compile.h"
#include "gen/map.h"

const char *ctype[] = {
  [T_ANY] = "cell_t *",
  [T_INT] = "int",
  [T_IO] = "cell_t *",
  [T_LIST] = "cell_t *",
  [T_SYMBOL] = "cell_t *",
  [T_MAP] = "map_t",
  [T_STRING] = "seg_t"
};

const char *cname[] = {
  [T_ANY] = "any",
  [T_INT] = "int",
  [T_IO] = "io",
  [T_LIST] = "lst",
  [T_SYMBOL] = "sym",
  [T_MAP] = "map",
  [T_STRING] = "str"
};

void gen_output_struct(cell_t *l, const char* fname) {
  printf("typedef struct {\n");
  csize_t n = list_size(l);
  COUNTUP(i, n) {
    // TODO add types
    printf("  %s%s%d;\n", ctype[T_ANY], cname[T_ANY], (int)i);
  }
  printf("} %s_output;\n", fname);
}

int test_gen_output_struct() {
  cell_t *l = make_list(3);
  gen_output_struct(l, "test_function");
  drop(l);
  return 0;
}
