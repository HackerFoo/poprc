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
#include "gen/eval.h"
#include "gen/lex.h"
#include "gen/user_func.h"

const char *ctype(type_t t) {
  static const char *table[] = {
    [T_ANY]    = "any_t ",
    [T_INT]    = "int ",
    [T_IO]     = "void *",
    [T_LIST]   = "slot_t *",
    [T_SYMBOL] = "int ",
    [T_MAP]    = "map_t ",
    [T_STRING] = "seg_t ",
    [T_BOTTOM] = "void ",
  };
  assert(t.exclusive < LENGTH(table));
  return table[t.exclusive];
}

const char *cname(type_t t) {
  static const char *table[] = {
    [T_ANY]    = "any",
    [T_INT]    = "int",
    [T_IO]     = "io",
    [T_LIST]   = "lst",
    [T_SYMBOL] = "sym",
    [T_MAP]    = "map",
    [T_STRING] = "str",
    [T_BOTTOM] = "bot"
  };
  assert(t.exclusive < LENGTH(table));
  return table[t.exclusive];
}

void gen_function_signature(cell_t *e) {
  cell_t *p = e + 1;
  size_t count = e->entry.len;

  // find first return
  cell_t *l = NULL;
  for(size_t i = e->entry.in; i < count; i++) {
    if(trace_type(&p[i]).exclusive == T_RETURN) {
      l = &p[i];
      break;
    }
  }
  csize_t out_n = list_size(l);
  type_t rtypes[out_n];
  resolve_types(e, l, rtypes);

  printf("%s%s_%s(", ctype(rtypes[out_n - 1]), e->module_name, e->word_name);
  char *sep = "";
  COUNTDOWN(i, e->entry.in) {
    cell_t *a = &p[i];
    type_t t = trace_type(a);
    printf("%s%s%s%d", sep, ctype(t), cname(t), (int)i);
    sep = ", ";
  }

  COUNTDOWN(i, out_n-1) {
    printf("%s%s*out_%s%d", sep, ctype(rtypes[i]), cname(rtypes[i]), (int)i);
  }

  printf(")");
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
    if(~c->expr_type.flags & T_TRACED) {
      gen_instruction(e, c);
    }
    c->expr_type.flags &= ~T_TRACED;
  }
}

void gen_return(cell_t *e, cell_t *l) {
  cell_t *p = e + 1;
  cell_t *end = p + e->entry.len;
  csize_t out_n = list_size(l);
  int ires = trace_decode(l->value.ptr[out_n - 1]);

  // skip if T_BOTTOM
  COUNTDOWN(i, out_n) {
    int ai = trace_decode(l->value.ptr[i]);
    type_t t = trace_type(&p[ai]);
    if(t.exclusive == T_BOTTOM) goto end;
  }

  COUNTDOWN(i, out_n-1) {
    int ai = trace_decode(l->value.ptr[i]);
    cell_t *a = &p[ai];
    type_t t = trace_type(a);
    const char *n = cname(t);
    printf("  *out_%s%d = %s%d;\n", n, (int)i, n, ai);
  }
  printf("  return %s%d;\n", cname(trace_type(&p[ires])), ires);

end:
  {
    cell_t *next = closure_next(l);
    if(closure_next(l) < end) {
      printf("\nblock%d:\n", (int)(next - (e + 1)));
    }
  }
}

void gen_decl(cell_t *e, cell_t *c) {
  int i = c - e - 1;
  type_t t = trace_type(c);
  if(t.exclusive != T_RETURN &&
     t.exclusive != T_BOTTOM &&
     c->func != func_assert) {
    if(c->func == func_value) {
      printf("  %s%s%d = ", ctype(t), cname(t), i);
      gen_value_rhs(c);
    } else {
      printf("  %s%s%d;\n", ctype(t), cname(t), i);
    }
  }
}

void gen_instruction(cell_t *e, cell_t *c) {
  if(trace_type(c).exclusive == T_RETURN) {
    gen_return(e, c);
  } else if(c->func == func_value) {
    // values are already declared
    // gen_value(e, c);
  } else if(c->func == func_assert) {
    gen_assert(e, c);
  } else if(c->func == func_quote) {
    gen_quote(e, c);
  } else if(c->func == func_dep) {
    // don't generate anything
  } else {
    gen_call(e, c);
  }
}

cell_t *get_entry(cell_t *c) {
  if(!is_user_func(c)) return NULL;
  return &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
}

void gen_call(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int i = c - d;
  char *sep = "";
  const char *module_name, *word_name;

  if(get_entry(c) == e && trace_type(closure_next(c)).exclusive == T_RETURN) {
    csize_t in = closure_in(c);
    printf("\n  // tail call\n");
    for(csize_t i = 0; i < in; i++) {
      int a = trace_decode(c->expr.arg[i]);
      printf("  %s%d = %s%d;\n", cname(trace_type(&d[in - 1 - i])), in - 1 - i, cname(trace_type(&d[a])), a);
    };
    printf("  goto body;\n");
  } else if(trace_type(c).exclusive != T_BOTTOM) {
    trace_get_name(c, &module_name, &word_name);
    printf("  %s%d = %s_%s(", cname(trace_type(c)), i, module_name, word_name);

    csize_t in = closure_in(c);
    csize_t n = closure_args(c);
    csize_t start_out = n - closure_out(c);

    for(csize_t i = 0; i < in; i++) {
      int a = trace_decode(c->expr.arg[i]);
      if(a == NIL_INDEX) {
        printf("%sNULL", sep);
      } else {
        printf("%s%s%d", sep, cname(trace_type(&d[a])), a);
      }
      sep = ", ";
    };

    for(csize_t i = start_out; i < n; i++) {
      int a = trace_decode(c->expr.arg[i]);
      printf("%s&%s%d", sep, cname(trace_type(&d[a])), a);
      sep = ", ";
    }

    printf(");\n");
  }
}

void gen_value_rhs(cell_t *c) {
  type_t t = trace_type(c);
  switch(t.exclusive) {
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
  type_t t = trace_type(c);
  printf("  %s%d = ", cname(t), i);
  gen_value_rhs(c);
}

void gen_skipped(cell_t *e, int start_after, int until) {
  cell_t
    *code = e + 1,
    *start_after_c = &code[start_after],
    *start = start_after_c + calculate_cells(start_after_c->size),
    *end = code + e->entry.len;
  if(start_after > until) {
    FOR_TRACE(c, start, end) {
      type_t t = trace_type(c);
      if(t.exclusive == T_RETURN) break;
      TRAVERSE(c, in) {
        if(*p && trace_decode(*p) == until) return;
      }
      gen_instruction(e, c);
      c->expr_type.flags |= T_TRACED;
    }
  }
}

void gen_assert(cell_t *e, cell_t *c) {
  cell_t *d = e + 1;
  int
    i = c - d,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname(trace_type(c));
  printf("\n  // assert\n");
  cell_t *end = d + e->entry.len;
  bool bottom = trace_type(c).exclusive == T_BOTTOM;
  if(!bottom) {
    printf("  #define %s%d %s%d\n", cn, i, cname(trace_type(&d[ip])), ip); // a little HACKy
  }
  FOR_TRACE(p, closure_next(c), end) {
    if(trace_type(p).exclusive == T_RETURN) {
      cell_t *next = p + closure_cells(p);
      if(next < end) {
        if(bottom &&
           next->func == func_assert &&
           next->expr.arg[1] == c->expr.arg[1]) {
          next->expr_type.flags |= T_TRACED;
          continue;
        }
        gen_skipped(e, trace_decode(c->expr.arg[0]), c - d);
        printf("  if(!%s%d)", cname(trace_type(&d[iq])), iq);
        printf(" goto block%d;\n", (int)(next - d));
        goto done;
      }
    }
  }
  printf("  assert(%s%d);\n", cname(trace_type(&d[iq])), iq);
done:
  return;
}

void gen_quote(cell_t *e, cell_t *c) {
  const char *module_name, *word_name;
  cell_t *d = e + 1;
  const int ic = c - d;
  csize_t n = closure_in(c);
  cell_t *qe = &trace_cells[trace_decode(c->expr.arg[n - 1])];
  trace_get_name(c, &module_name, &word_name);
  printf("  %s%d = __primitive_quote(%s_%s, %d, %d);\n",
         cname(trace_type(c)), ic,
         module_name, word_name,
         qe->entry.in, qe->entry.out);
  COUNTUP(i, n - 1) {
    uintptr_t ai = trace_decode(c->expr.arg[i]);
    printf("  %1$s%2$d = __primitive_pushl(%3$s%4$d, %1$s%2$d);\n",
           cname(trace_type(c)), ic,
           cname(trace_type(&d[ai])), (int)ai);
  }
}

void gen_function(cell_t *e) {
  cell_t *start = e + 1;
  cell_t *end = start + e->entry.len;

  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
    gen_function_signature(e);
    printf(";\n");
  }

  gen_function_signature(e);
  printf("\n{\n");
  gen_body(e);
  printf("}\n");

  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
    printf("\n");
    gen_function(e);
  }
}

// for now assumes int
void gen_main(cell_t *e) {
  printf("#include <stdio.h>\n"
         "#include <stdlib.h>\n"
         "#include <assert.h>\n"
         "#include \"macros.h\"\n"
         "#include \"cgen/primitives.h\"\n\n");

  gen_function_signature(e);
  printf(";\n\n");

  printf("int main(int argc, char **argv)\n{\n");
  printf("  int in[%d];\n", e->entry.in);
  printf("  int out[%d];\n", e->entry.out);
  printf("  if(argc < LENGTH(in) + 1) {\n"
         "    printf(\"not enough arguments\\n\");\n"
         "    return -1;\n"
         "  }\n\n");

  printf("  FOREACH(i, in) {\n"
         "    in[i] = atoi(argv[i + 1]);\n"
         "  }\n");

  char *sep = "";
  printf("  out[0] = %s_%s(", e->module_name, e->word_name);
  COUNTUP(i, e->entry.in) {
    printf("%sin[%d]", sep, (int)i);
    sep = ", ";
  }
  COUNTUP(i, e->entry.out - 1) {
    printf("%s&out[%d]", sep, (int)i + 1);
    sep = ", ";
  }
  printf(");\n");
  printf("  FOREACH(i, in) {\n"
         "    printf(\"%%d \", in[i]);\n"
         " }\n");
  printf("  printf(\"%s_%s =>\");\n", e->module_name, e->word_name);
  printf("  FOREACH(i, out) {\n"
         "    printf(\" %%d\", out[i]);\n"
         "  }\n");
  printf("  printf(\"\\n\");\n\n"
         "  return 0;\n"
         "}\n");
}

void command_cgen(cell_t *rest) {
  if(rest) {
    command_def(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) gen_function(e);
  }
}

void command_main(cell_t *rest) {
  if(rest) {
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) gen_main(e);
  }
}
