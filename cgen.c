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
  cell_t *last = e + 1;
  size_t count = e->entry.len;
  cell_t *end = &e[count + 1];
  FOR_TRACE(p, e + 1, end) {
    last = p;
  }
  return last;
}

type_t gen_type(cell_t *c) {
  return *trace_type(c) & T_EXCLUSIVE;
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
    *end = start + e->entry.len;
  while(is_var(start)) start++;

  FOR_TRACE(c, start, end) {
    gen_decl(e, c);
  }
  printf("\nbody:\n");
  FOR_TRACE(c, start, end) {
    gen_instruction(e, c);
  }
}

void gen_return(cell_t *e, cell_t *l) {
  cell_t *p = e + 1;
  cell_t *end = p + e->entry.len;
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
  cell_t *next = closure_next(l);
  if(next < end) {
    printf("\nblock%d:\n", (int)(next - (e + 1)));
  }
}

void gen_decl(cell_t *e, cell_t *c) {
  int i = c - e - 1;
  type_t t = gen_type(c);
  if(t != T_RETURN) {
    if(c->func == func_value) {
      printf("  %s%s%d = ", ctype(t), cname(t), i);
      gen_value_rhs(c);
    } else {
      printf("  %s%s%d;\n", ctype(t), cname(t), i);
    }
  }
}

void gen_instruction(cell_t *e, cell_t *c) {
  if(gen_type(c) == T_RETURN) {
    gen_return(e, c);
  } else if(c->func == func_value) {
    // values are already declared
    // gen_value(e, c);
  } else if(c->func == func_select) {
    gen_select(e, c);
  } else if(c->func == func_assert) {
    gen_assert(e, c);
  } else {
    gen_call(e, c);
  }
}

cell_t *get_entry(cell_t *c) {
  if(c->func != func_exec) return NULL;
  return &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
}

void gen_call(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int i = c - d;
  char *sep = "";
  const char *module_name, *word_name;

  if(get_entry(c) == e && gen_type(closure_next(c)) == T_RETURN) {
    csize_t in = closure_in(c) - 1;
    for(csize_t i = 0; i < in; i++) {
      int a = trace_decode(c->expr.arg[i]);
      printf("\n  // tail call\n");
      printf("  %s%d = %s%d;\n", cname(gen_type(&d[i])), i, cname(gen_type(&d[a])), a);
    };
    printf("  goto body;\n");
  } else {
    get_name(c, &module_name, &word_name);
    printf("  %s%d = %s_%s(", cname(gen_type(c)), i, module_name, word_name);

    csize_t in = closure_in(c), start_out = in;
    csize_t n = closure_args(c);
    if(c->func == func_exec) in--;

    for(csize_t i = 0; i < in; i++) {
      int a = trace_decode(c->expr.arg[i]);
      printf("%s%s%d", sep, cname(gen_type(&d[a])), a);
      sep = ", ";
    };

    for(csize_t i = start_out; i < n; i++) {
      int a = trace_decode(c->expr.arg[i]);
      printf("%s&%s%d", sep, cname(gen_type(&d[a])), a);
      sep = ", ";
    }

    printf(");\n");
  }
}

void gen_value_rhs(cell_t *c) {
  type_t t = gen_type(c);
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

void gen_value(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int i = c - d;
  type_t t = gen_type(c);
  printf("  %s%d = ", cname(t), i);
  gen_value_rhs(c);
}

void gen_select(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname(gen_type(c));
  printf("phi%d:\n", iq);
  printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[iq])), iq);
  printf("  goto phi%d;\n", i);
  printf("phi%d:\n", ip);
  printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[ip])), ip);
  printf("phi%d:\n", i);
}

void gen_assert(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]),
    done = trace_decode(c->alt);
  const char *cn = cname(gen_type(c));
  if(done >= 0) {

    int next = done;
    while(next > 0) {
      done = next;
      next = trace_decode(d[next].alt);
    }
    printf("  if(%s%d) {", cname(gen_type(&d[iq])), iq);
    printf(" %s%d = %s%d;", cname(gen_type(&d[done])), done, cname(gen_type(&d[ip])), ip);
    printf(" goto phi%d; }\n", done);
  } else {
    cell_t *end = d + e->entry.len;
    printf("  %s%d = %s%d;\n", cn, i, cname(gen_type(&d[ip])), ip);
    FOR_TRACE(p, closure_next(c), end) {
      if(gen_type(p) == T_RETURN) {
        cell_t *next = p + closure_cells(p);
        if(next < end) {
          printf("  if(!%s%d) ", cname(gen_type(&d[iq])), iq);
          printf("goto block%d;\n", (int)(next - d));
        }
        break;
      }
    }
  }
  printf("phi%d:\n", i);
}

void gen_function(cell_t *e) {
  gen_function_signature(e);
  printf("{\n");
  gen_body(e);
  printf("}\n");
}
