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

cell_t *trace_last(cell_t *e) {
  cell_t *p = e + 1, *prev = p;
  size_t count = e->entry.len;
  cell_t *end = &p[count];
  while(p < end) {
    size_t s = closure_cells(p);
    prev = p;
    p += s;
  }
  return prev;
}

type_t gen_type(cell_t *c) {
  type_t t = is_value(c) ? c->value.type : c->expr_type;
  return t & T_EXCLUSIVE;
}

void gen_function_signature(cell_t *e) {
  cell_t *p = e + 1;
  cell_t *l = trace_last(e);
  csize_t out_n = list_size(l);
  size_t count = e->entry.len;
  size_t ires = trace_decode(l->value.ptr[out_n - 1]);

  printf("%s%s_%s(", ctype(gen_type(&p[ires])), e->module_name, e->word_name);
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

void gen_body(cell_t *e) {
  cell_t
    *start = e + 1,
    *end = trace_last(e);
  while(is_var(start)) start++;

  for(cell_t *c = start; c < end; c += closure_cells(c)) {
    gen_decl(e, c);
  }
  for(cell_t *c = start; c < end; c += closure_cells(c)) {
    gen_instruction(e, c);
  }
  gen_return(e, end);
}

void gen_return(cell_t *e, cell_t *l) {
  cell_t *p = e + 1;
  csize_t out_n = list_size(l);
  int ires = trace_decode(l->value.ptr[out_n - 1]);
  COUNTDOWN(i, out_n-1) {
    int ai = trace_decode(l->value.ptr[i]);
    cell_t *a = &p[ai];
    type_t t = gen_type(a);
    const char *n = cname(t);
    printf("  *out_%s%d = %s%d;\n", n, (int)i, n, ai);
  }
  printf("  return %s%d;\n", cname(gen_type(&p[ires])), ires);
}

void gen_decl(cell_t *e, cell_t *c) {
  int i = c - e - 1;
  type_t t = gen_type(c);
  printf("  %s%s%d;\n", ctype(t), cname(t), i);
}

void gen_instruction(cell_t *e, cell_t *c) {
  if(c->func == func_value) {
    gen_value(e, c);
  } else if(c->func == func_select) {
    gen_select(e, c);
  } else if(c->func == func_assert) {
    gen_assert(e, c);
  } else {
    gen_call(e, c);
  }
}

void get_name(cell_t *c, const char **module_name, const char **word_name) {
  if(c->func == func_exec) {
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = "__primitive";
    *word_name = function_name(c->func);
  }
}

void gen_call(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int i = c - d;
  char *sep = "";
  const char *module_name, *word_name;
  get_name(c, &module_name, &word_name);
  printf("  %s%d = %s_%s(", cname(gen_type(c)), i, module_name, word_name);

  traverse(c, {
      int a = trace_decode(*p);
      printf("%s%s%d", sep, cname(gen_type(&d[a])), a);
      sep = ", ";
    }, ARGS_IN);
  traverse(c, {
      int a = trace_decode(*p);
      printf("%s&%s%d", sep, cname(gen_type(&d[a])), a);
      sep = ", ";
    }, ARGS_OUT);

  printf(");\n");
}

void gen_value(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
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

void gen_select(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname(gen_type(c));
  printf("label%d:\n", iq);
  printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[iq])), iq);
  printf("  goto label%d;\n", i);
  printf("label%d:\n", ip);
  printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[ip])), ip);
  printf("label%d:\n", i);
}

void gen_assert(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname(gen_type(c));
  printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[ip])), ip);
  printf("  if(%s%d == SYM_TRUE) goto label%d;\n", cname(gen_type(&d[iq])), iq, i);
}

void gen_function(cell_t *e) {
  gen_function_signature(e);
  printf("{\n");
  gen_body(e);
  printf("}\n");
}
