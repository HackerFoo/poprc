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
#include "gen/print.h"
#include "gen/cgen.h"

const char *ctype(type_t t) {
  static const char *table[] = {
    [T_ANY]    = "cell_t *",
    [T_INT]    = "int ",
    [T_IO]     = "cell_t *",
    [T_LIST]   = "cell_t *",
    [T_SYMBOL] = "int ",
    [T_MAP]    = "map_t ",
    [T_STRING] = "seg_t "
  };
  t &= T_EXCLUSIVE;
  assert(t < LENGTH(table));
  return table[t];
}

const char *cname(type_t t) {
  static const char *table[] = {
    [T_ANY]    = "any",
    [T_INT]    = "int",
    [T_IO]     = "io",
    [T_LIST]   = "lst",
    [T_SYMBOL] = "sym",
    [T_MAP]    = "map",
    [T_STRING] = "str"
  };
  t &= T_EXCLUSIVE;
  assert(t < LENGTH(table));
  return table[t];
}

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

/*
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
*/

type_t gen_type(cell_t *c) {
  type_t t = is_value(c) ? c->value.type : (uintptr_t)c->tmp;
  return t & T_EXCLUSIVE;
}

void gen_function_signature(cell_t *trace, char *mname, char *fname) {
  cell_t *header = trace;
  cell_t *p = header + 1;
  cell_t *l = trace_last(trace);
  csize_t out_n = list_size(l);
  size_t count = header->value.integer[0];
  size_t ires = trace_decode(l->value.ptr[out_n - 1]);

  printf("%s%s_%s(", ctype((uintptr_t)p[ires].tmp), mname, fname);
  char *sep = "";
  COUNTDOWN(i, count) {
    cell_t *a = &p[i];
    if(!is_var(a)) continue;
    type_t t = gen_type(a);
    printf("%s%s%s%d", sep, ctype(t), cname(t), (int)i);
    sep = ", ";
  }
  COUNTDOWN(i, out_n-1) {
    int ai = trace_decode(l->value.ptr[i]);
    cell_t *a = &p[ai];
    type_t t = gen_type(a);
    printf("%s%s*out_%s%d", sep, ctype(t), cname(t), (int)i);
  }

  printf(")\n");
}

void gen_body(cell_t *trace) {
  cell_t
    *start = trace + 1,
    *end = trace_last(trace);
  while(is_var(start)) start++;

  for(cell_t *c = start; c < end; c += closure_cells(c)) {
    gen_decl(trace, c);
  }
  for(cell_t *c = start; c < end; c += closure_cells(c)) {
    gen_instruction(trace, c);
  }
  gen_return(trace, end);
}

void gen_return(cell_t *trace, cell_t *l) {
  cell_t *p = trace + 1;
  csize_t out_n = list_size(l);
  int ires = trace_decode(l->value.ptr[out_n - 1]);
  COUNTDOWN(i, out_n-1) {
    int ai = trace_decode(l->value.ptr[i]);
    cell_t *a = &p[ai];
    type_t t = gen_type(a);
    const char *n = cname(t);
    printf("  *out_%s%d = %s%d;\n", n, (int)i, n, ai);
  }
  printf("  return %s%d;\n", cname((uintptr_t)p[ires].tmp), ires);
}

void gen_decl(cell_t *trace, cell_t *c) {
  int i = c - trace - 1;
  type_t t = gen_type(c);
  printf("  %s%s%d;\n", ctype(t), cname(t), i);
}

void gen_instruction(cell_t *trace, cell_t *c) {
  if(c->func == func_value) {
    gen_value(trace, c);
  } else if(c->func == func_select) {
    gen_select(trace, c);
  } else if(c->func == func_assert) {
    gen_assert(trace, c);
  } else {
    gen_call(trace, c);
  }
}

void gen_call(cell_t *trace, cell_t *c) {
  cell_t *d = trace + 1;
  int i = c - d;
  const char *fname = function_name(c->func);
  char *sep = "";
  printf("  %s%d = %s(", cname((uintptr_t)c->tmp), i, fname);

  traverse(c, {
      int a = trace_decode(*p);
      printf("%s%s%d", sep, cname((uintptr_t)d[a].tmp), a);
      sep = ", ";
    }, ARGS_IN);
  traverse(c, {
      int a = trace_decode(*p);
      printf("%s&%s%d", sep, cname((uintptr_t)d[a].tmp), a);
      sep = ", ";
    }, ARGS_OUT);

  printf(");\n");
}

void gen_value(cell_t *trace, cell_t *c) {
  cell_t *d = trace + 1;
  int i = c - d;
  type_t t = gen_type(c);
  printf("  %s%d = ", cname(t), i);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("%d;\n", (int)c->value.integer[0]);
    break;
  case T_STRING: {
    seg_t s = c->value.str;
    printf("{ .s = \"%*.s\", .n = %d };\n", (int)s.n, s.s, (int)s.n);
    break;
  }
  default:
    assert(false); // TODO add more types
  }
}

void gen_select(cell_t *trace, cell_t *c) {
  cell_t *d = trace + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname((uintptr_t)c->tmp);
  printf("label%d:\n", iq);
  printf("  %s%d = %s%d;\n", cn, i, cname((uintptr_t)d[iq].tmp), iq);
  printf("  goto label%d;\n", i);
  printf("label%d:\n", ip);
  printf("  %s%d = %s%d;\n", cn, i, cname((uintptr_t)d[ip].tmp), ip);
  printf("label%d:\n", i);
}

void gen_assert(cell_t *trace, cell_t *c) {
  cell_t *d = trace + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname((uintptr_t)c->tmp);
  printf("  %s%d = %s%d;\n", cn, i, cname((uintptr_t)d[ip].tmp), ip);
  printf("  if(%s%d == SYM_TRUE) goto label%d;\n", cname((uintptr_t)d[iq].tmp), iq, i);
}

void testgen(char *name, char *src) {
  cell_t *l = lex(src, 0);
  cell_t *tr = test_compile(l, NULL);
  gen_function(tr, "testgen", name);
  free_toks(l);
}

void gen_function(cell_t *tr, char *mname, char *fname) {
  gen_function_signature(tr, mname, fname);
  printf("{\n");
  gen_body(tr);
  printf("}\n");
}

int test_gen() {
  testgen("max", "[] pushl pushl dup . dup popr drop | [<= !] . popr swap drop cut");
  testgen("ifte", "[] pushl pushl swap pushr [0 == !] [drop drop] | . popr swap drop cut");
  testgen("rot", "[] pushl swap pushr pushl popr swap popr swap popr swap drop");
  return 0;
}
