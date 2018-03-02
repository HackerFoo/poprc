/* Copyright 2012-2017 Dustin DeWeese
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
#include <inttypes.h>

#include "startle/error.h"
#include "startle/log.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "byte_compile.h"
#include "parse.h"
#include "print.h"
#include "cgen.h"
#include "eval.h"
#include "lex.h"
#include "user_func.h"
#include "trace.h"

static uintptr_t assert_set[31];

// table of corresponding C types
const char *ctype(type_t t) {
  static const char *table[] = {
    [T_ANY]      = "any_t ",
    [T_INT]      = "int ",
    [T_IO]       = "void *",
    [T_LIST]     = "array ",
    [T_SYMBOL]   = "int ",
    [T_MAP]      = "map_t ",
    [T_STRING]   = "seg_t ",
    [T_FUNCTION] = "array ",
    [T_BOTTOM]   = "void ",
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

// table of identifier prefixes for each type
const char *cname(type_t t) {
  static const char *table[] = {
    [T_ANY]      = "any",
    [T_INT]      = "int",
    [T_IO]       = "io",
    [T_LIST]     = "lst",
    [T_SYMBOL]   = "sym",
    [T_MAP]      = "map",
    [T_STRING]   = "str",
    [T_FUNCTION] = "func",
    [T_BOTTOM]   = "bot"
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

void gen_function_signature(cell_t *e) {
  cell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  type_t rtypes[out_n];
  resolve_types(e, rtypes);

  printf("%s%s_%s(", ctype(rtypes[out_n - 1]), e->module_name, e->word_name);
  char *sep = "";
  COUNTDOWN(i, e->entry.in) {
    cell_t *a = &p[i];
    type_t t = trace_type(a);
    printf("%s%s%s%d", sep, ctype(t), cname(t), (int)i + 1);
    sep = ", ";
  }

  COUNTDOWN(i, out_n-1) {
    printf("%s%s*out_%s%d", sep, ctype(rtypes[i]), cname(rtypes[i]), (int)i);
  }

  printf(")");
}

void gen_function_signatures(cell_t *e) {
  gen_function_signature(e);
  printf(";\n");

  FOR_TRACE(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      cell_t *x = get_entry(c);
      if(x != e) {
        gen_function_signatures(x);
      }
    }
  }
}

void gen_body(cell_t *e) {
  FOR_TRACE(c, e) {
    if(is_var(c)) continue;
    gen_decl(e, c);
  }
  printf("\nbody:\n");
  FOR_TRACE(c, e) {
    if(is_var(c)) continue;
    gen_instruction(e, c);
    FLAG_CLEAR(c->trace, TRACE_TRACED);
  }
}

void gen_return(cell_t *e, cell_t *l) {
  cell_t *end = e + e->entry.len;
  csize_t out_n = list_size(l);
  int ires = trace_decode(l->value.ptr[out_n - 1]);

  if(FLAG(l->trace, TRACE_TRACED)) goto end;

  // skip if T_BOTTOM
  COUNTDOWN(i, out_n) {
    int ai = trace_decode(l->value.ptr[i]);
    type_t t = trace_type(&e[ai]);
    if(t == T_BOTTOM) goto end;
  }

  COUNTDOWN(i, out_n-1) {
    int ai = trace_decode(l->value.ptr[i]);
    cell_t *a = &e[ai];
    type_t t = trace_type(a);
    const char *n = cname(t);
    printf("  if(out_%s%d) *out_%s%d = %s%d;\n",
           n, (int)i, n, (int)i, n, ai);
  }
  printf("  return %s%d;\n", cname(trace_type(&e[ires])), ires);

end:
  {
    cell_t *next = closure_next(l);
    if(closure_next(l) <= end) {
      printf("\nblock%d:\n", (int)(next - e));
    }
  }
}

void gen_decl(cell_t *e, cell_t *c) {
  int i = c - e;
  type_t t = trace_type(c);
  if(t != T_RETURN &&
     t != T_BOTTOM &&
     c->op != OP_assert) {
    if(c->op == OP_value) {
      printf("  %s%s%d = ", ctype(t), cname(t), i);
      gen_value_rhs(c);
    } else {
      printf("  %s%s%d;\n", ctype(t), cname(t), i);
    }
  }
}

void gen_instruction(cell_t *e, cell_t *c) {
  if(trace_type(c) == T_RETURN) {
    gen_return(e, c);
  } else if(c->op == OP_value) {
    // values are already declared
    // gen_value(e, c);
  } else if(c->op == OP_assert) {
    gen_assert(e, c);
  } else if(c->op == OP_dep) {
    // don't generate anything
  } else {
    gen_call(e, c);
  }
}

static
bool last_call(cell_t *e, cell_t *c) {
  c = closure_next(c);
  FOR_TRACE(p, e, c - e - 1) {
    if(trace_type(p) == T_RETURN) {
      return true;
    } else if(p->op != OP_dep &&
              p->op != OP_assert) {
      break;
    }
  }
  return false;
}

void skip_to_next_block(cell_t *e, cell_t *c) {
  c = closure_next(c);
  FOR_TRACE(p, e, c - e - 1) {
    FLAG_SET(p->trace, TRACE_TRACED);
    if(trace_type(p) == T_RETURN) break;
  }
}

void gen_call(cell_t *e, cell_t *c) {
  if(FLAG(c->trace, TRACE_TRACED)) return;
  int i = c - e;
  char *sep = "";
  const char *module_name, *word_name;

  if(get_entry(c) == e && last_call(e, c)) {
    // this is a tail call
    skip_to_next_block(e, c);
    csize_t in = closure_in(c);
    printf("\n  // tail call\n");

    // overwrite function arguments with new values
    COUNTUP(i, in) {
      int a = trace_decode(c->expr.arg[i]);
      printf("  %s%d = %s%d;\n",
             cname(trace_type(&e[in - i])),
             (int)(in - i),
             cname(trace_type(&e[a])), a);
    };

    // jump to the beginning
    printf("  goto body;\n");
  } else if(trace_type(c) != T_BOTTOM) {
    csize_t
      in = closure_in(c),
      out = closure_out(c),
      n = closure_args(c),
      start_out = n - closure_out(c);

    trace_get_name(c, &module_name, &word_name);

    printf("  %s%d = %s_%s", cname(trace_type(c)), i, module_name, word_name);
    if(c->op == OP_ap || c->op == OP_compose) {
      assert_error(in >= 1);
      printf("%d%d", in-1, out);
    }
    printf("(");

    COUNTUP(i, in) {
      int a = trace_decode(c->expr.arg[i]);
      if(a == NIL_INDEX) {
        printf("%snil", sep);
      } else {
        printf("%s%s%d", sep, cname(trace_type(&e[a])), a);
      }
      sep = ", ";
    };

    RANGEUP(i, start_out, n) {
      int a = trace_decode(c->expr.arg[i]);
      if(a <= 0) {
        printf("%sNULL", sep);
      } else {
        printf("%s&%s%d", sep, cname(trace_type(&e[a])), a);
      }
      sep = ", ";
    }

    printf(");\n");
  }
}

// print the RHS to initialize a value
void gen_value_rhs(cell_t *c) {
  type_t t = trace_type(c);
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
    assert_error(false); // TODO add more types
  }
}

void gen_value(cell_t *e, cell_t *c) {
  type_t t = trace_type(c);
  printf("  %s%d = ", cname(t), (int)(c - e));
  gen_value_rhs(c);
}

// print instructions that have been delayed (skipped)
void gen_skipped(cell_t *e, int start_after, int until) {
  if(start_after > until) {
    FOR_TRACE(c, e, start_after) {
      type_t t = trace_type(c);
      if(t == T_RETURN) break;
      TRAVERSE(c, in) {
        if(*p && trace_decode(*p) == until) return;
      }
      gen_instruction(e, c);
      FLAG_SET(c->trace, TRACE_TRACED);
    }
  }
}

void gen_assert(cell_t *e, cell_t *c) {
  if(FLAG(c->trace, TRACE_TRACED)) return;
  int
    i = c - e,
    ip = trace_decode(c->expr.arg[0]),
    iq = trace_decode(c->expr.arg[1]);
  const char *cn = cname(trace_type(c));
  cell_t *ret = NULL;
  printf("\n  // assert %d\n", iq);
  cell_t *end = e + e->entry.len + 1;
  bool bottom = trace_type(c) == T_BOTTOM;
  if(!bottom) {
    // use #define to replace references to the assertion output to the output of arg[0]
    printf("  #define %s%d %s%d\n", cn, i, cname(trace_type(&e[ip])), ip); // a little HACKy
  }

  if(!set_insert(iq, assert_set, LENGTH(assert_set))) {
    FOR_TRACE(p, e, closure_next(c) - e - 1) {
      if(!ret && trace_type(p) == T_RETURN) {
        ret = p;
      }
      if(p->op == OP_assert && trace_decode(p->expr.arg[1]) == iq) {
        ret = NULL;
      }
    }
    if(ret) {
      cell_t *next = ret + closure_cells(ret);
      if(next < end) {
        printf("  if(!%s%d)", cname(trace_type(&e[iq])), iq);
        printf(" goto block%d;\n", (int)(next - e));
      } else {
        printf("  assert(%s%d);\n", cname(trace_type(&e[iq])), iq);
      }
    }
  }
}

void gen_function(cell_t *e) {
  zero(assert_set);

  gen_function_signature(e);
  printf("\n{\n");
  gen_body(e);
  printf("}\n");

  FOR_TRACE(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      cell_t *x = get_entry(c);
      if(x != e) {
        printf("\n");
        gen_function(x);
      }
    }
  }
}

void gen_functions(cell_t *e) {
  gen_function(e);
  printf("\n");

  FOR_TRACE(c, e) {
    if(c->op == OP_exec) {
      cell_t *x = get_entry(c);
      if(x != e) gen_functions(x);
    }
  }
}

// generate the driver to allow testing the function from the command line
// for now assumes int
void gen_main(cell_t *e) {
  type_t rtypes[e->entry.out];
  resolve_types(e, rtypes);

  printf("#include <stdio.h>\n"
         "#include <stdlib.h>\n"
         "#include <assert.h>\n"
         "#include \"macros.h\"\n"
         "#include \"rt_types.h\"\n"
         "#include \"startle/support.h\"\n"
         "#include \"cgen/primitives.h\"\n\n");

  gen_function_signatures(e);
  printf("\n");

  printf("int main(int argc, char **argv)\n{\n");
  printf("  const int arity_in = %d;\n", e->entry.in);
  printf("  const int arity_out = %d;\n", e->entry.out);
  printf("  array in[arity_in];\n"
         "  array out[arity_out];\n"
         "  error_t error;\n"
         "  if(catch_error(&error)) {\n"
         "    log_print_all();\n"
         "    return -1;\n"
         "  }\n"
         "  if(argc < 2) {\n"
         "    printf(\"not enough arguments\\n\");\n"
         "    return -1;\n"
         "  }\n\n");

  printf("  const char *p = arguments(argc - 1, argv + 1);\n"
         "  const char *e = p + strlen(p);\n"
         "  COUNTUP(i, arity_in) {\n"
         "    if(!p) {\n"
         "      printf(\"parse error at argument %%d\\n\", (int)i);\n"
         "      return -1;\n"
         "    }\n"
         "    in[i] = parse(&p, e);\n"
         "  }\n\n");

  char *sep = "";
  if(rtypes[0] == T_FUNCTION) {
    printf("  out[0]");
  } else {
    printf("  *alloc_arr(&out[0], 1)");
  }
  printf(" = %s_%s(", e->module_name, e->word_name);
  cell_t *code = e + 1;
  csize_t in = e->entry.in;
  COUNTUP(i, in) {
    if(code[in - 1 - i].value.type == T_FUNCTION) {
      printf("%sin[%d]", sep, (int)i);
    } else {
      printf("%sin[%d].elem[0]", sep, (int)i);
    }
    sep = ", ";
  }
  RANGEUP(i, 1, e->entry.out) {
    if(rtypes[i] == T_FUNCTION) {
      printf("%s&out[%d]", sep, (int)i);
    } else {
      printf("%salloc_arr(&out[%d], 1)", sep, (int)i);
    }
    sep = ", ";
  }
  printf(");\n");
  printf("  printf(\"%s_%s =>\");\n", e->module_name, e->word_name);
  printf("  COUNTUP(i, arity_out) {\n"
         "    print_array(out[i]);\n"
         "  }\n");
  printf("  printf(\"\\n\");\n\n"
         "  return 0;\n"
         "}\n\n");
}

COMMAND(cc, "print C code for given function") {
  if(rest) {
    command_define(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) {
      gen_function_signatures(e);
      printf("\n");
      gen_function(e);
    }
  }
  if(command_line) quit = true;
}

COMMAND(main, "print main() for given function") {
  if(rest) {
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) gen_main(e);
  }
  if(command_line) quit = true;
}
