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
#include "gen/parse.h"

const char *ctype[] = {
  [T_ANY] = "cell_t *",
  [T_INT] = "int ",
  [T_IO] = "cell_t *",
  [T_LIST] = "cell_t *",
  [T_SYMBOL] = "int ",
  [T_MAP] = "map_t ",
  [T_STRING] = "seg_t "
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

cell_t *trace_last(cell_t *trace) {
  cell_t *header = trace;
  cell_t *p = header + 1, *prev = p;
  size_t count = header->value.integer[0];
  cell_t *end = &p[count];
  while(p < end) {
    size_t s = closure_cells(p);
    prev = p;
    p += s;
  }
  return prev;
}

void gen_output_struct(cell_t *trace, const char* fname) {
  cell_t *l = trace_last(trace);
  cell_t *data = trace + 1;

  printf("typedef struct {\n");
  csize_t n = list_size(l);
  COUNTUP(i, n) {
    cell_t *a = &data[trace_decode(l->value.ptr[i])];
    type_t t = (uintptr_t)a->tmp & T_EXCLUSIVE;
    printf("  %s%s%d;\n", ctype[t], cname[t], (int)i);
  }
  printf("} %s_output;\n", fname);
}

void gen_function_signature(cell_t *trace, const char *fname) {
  cell_t *header = trace;
  cell_t *p = header + 1;
  size_t count = header->value.integer[0];
  printf("%s_output %s(", fname, fname);
  char *sep = "";
  COUNTUP(i, count) {
    cell_t *a = &p[i];
    if(!is_var(a)) break;
    type_t t = a->value.type & T_EXCLUSIVE;
    printf("%s%s%s%d", sep, ctype[t], cname[t], i);
    sep = ", ";
  }
  printf(")\n");
}

int test_gen() {
  cell_t *l = lex("[] pushl pushl dup . dup popr drop | [<= !] . popr swap drop cut", 0);
  cell_t *tr = test_compile(l, NULL);
  gen_output_struct(tr, "max");
  printf("\n");
  gen_function_signature(tr, "max");
  printf("{\n  // TODO\n}\n");
  free_toks(l);
  return 0;
}
