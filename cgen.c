/* Copyright 2012-2018 Dustin DeWeese
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
    [T_LIST]     = "array ",
    [T_SYMBOL]   = "symbol_t ",
    [T_MAP]      = "map_t ",
    [T_STRING]   = "seg_t ",
    [T_FLOAT]    = "double ",
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
    [T_LIST]     = "lst",
    [T_SYMBOL]   = "sym",
    [T_MAP]      = "map",
    [T_STRING]   = "str",
    [T_FLOAT]    = "flt",
    [T_BOTTOM]   = "bot"
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

char capitalize(char c) {
  return INRANGE(c, 'a', 'z') ? c - ('a' - 'A') : c;
}

// print a suffix to select the mode of some primitives
void print_type_suffix(cell_t *entry, cell_t *c) {
  if(ONEOF(c->op, OP_otherwise, OP_assert, OP_exec)) return;
  printf("_%c", type_char(trace_type(c)));
  TRAVERSE(c, args) {
    char ch;
    if(*p) {
      cell_t *ta = &entry[tr_index(*p)];
      type_t t = trace_type(ta);
      ch = type_char(t);
      if(!tr_flags(*p, TR_FINAL) &&
         ONEOF(t, T_LIST, T_STRING)) {
        ch = capitalize(ch);
      }
    } else {
      ch = '0';
    }
    printf("%c", ch);
  }
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
  if(e->entry.rec) printf("\nentry: {\n");
  FOR_TRACE(c, e) {
    if(is_var(c)) continue;
    gen_instruction(e, c);
    FLAG_CLEAR(*c, trace, TRACED);
  }
}

void gen_return(cell_t *e, cell_t *l) {
  cell_t *end = e + e->entry.len;
  csize_t out_n = list_size(l);
  int ires = tr_index(l->value.ptr[out_n - 1]);

  if(FLAG(*l, trace, TRACED)) goto end;

  // skip if T_BOTTOM
  COUNTDOWN(i, out_n) {
    int ai = tr_index(l->value.ptr[i]);
    type_t t = trace_type(&e[ai]);
    if(t == T_BOTTOM) goto end;
  }

  COUNTDOWN(i, out_n-1) {
    int ai = tr_index(l->value.ptr[i]);
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
      printf("}\n\nblock%d: {\n", (int)(next - e));
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
      if(NOT_FLAG(*c, value, IMMEDIATE)) {
        printf("  const %s%s%d = ", ctype(t), cname(t), i);
        gen_value_rhs(c);
      }
    } else if(c->n ||
              is_dep(c) ||
              FLAG(*c, expr, PARTIAL)) {
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
  FOR_TRACE(p, e, c - e) {
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
  FOR_TRACE(p, e, c - e) {
    FLAG_SET(*p, trace, TRACED);
    if(trace_type(p) == T_RETURN) break;
  }
}

const char *external_name(const char *str) {
  const char *sep = strchr(str, ':');
  return sep ? sep + 1 : str;
}

bool print_external_header(const char *str) {
  bool sys = false;
  if(str[0] == '@') {
    sys = true;
    str++;
  }
  const char *sep = strchr(str, ':');
  if(sep) {
    if(sys) {
      printf("#include <%.*s>\n", (int)(sep-str), str);
    } else {
      printf("#include \"%.*s\"\n", (int)(sep-str), str);
    }
  }
  return sep != NULL;
}

void gen_call(cell_t *e, cell_t *c) {
  if(FLAG(*c, trace, TRACED)) return;
  int lhs = c - e;
  char *sep = "";
  const char *module_name, *word_name;

  if(get_entry(c) == e && last_call(e, c)) {
    // this is a tail call
    skip_to_next_block(e, c);
    csize_t in = closure_in(c);
    printf("\n  // tail call\n");

    // overwrite function arguments with new values
    COUNTUP(i, in) {
      int a = tr_index(c->expr.arg[i]);
      printf("  %s%d = %s%d;\n",
             cname(trace_type(&e[in - i])),
             (int)(in - i),
             cname(trace_type(&e[a])), a);
    };

    // jump to the beginning
    printf("  goto entry;\n");
  } else if(!ONEOF(c->op, OP_seq)) {
    csize_t
      in = closure_in(c),
      out = closure_out(c),
      n = closure_args(c),
      start_out = n - closure_out(c);

    trace_get_name(c, &module_name, &word_name);
    type_t t = trace_type(c);
    bool partial = FLAG(*c, expr, PARTIAL);
    int next_block = 0;
    if(partial) {

      // find next_block
      FOR_TRACE(p, e, closure_next(c) - e) {
        if(trace_type(p) == T_RETURN) {
          int size = calculate_cells(p->size);
          if(e->entry.len + 1 - (p-e) > size) { // if this isn't the last return
            next_block = p - e + size;
          }
          break;
        }
      }

      if(next_block) {
        printf("  if(");
      } else {
        printf("  assert_error(!");
      }
    } else {
      if(trace_type(c) == T_BOTTOM) {
        printf("  ");
      } else {
        printf("  %s%s%d = ", c->n ? "" : ctype(t), cname(t), lhs);
      }
    }
    if(ONEOF(c->op, OP_external, OP_external_io)) {
      cell_t *name = &e[tr_index(c->expr.arg[closure_in(c) - 1])];
      assert_error(is_value(name) &&
                   !is_var(name) &&
                   name->value.type == T_STRING,
                   "external name must be an immediate string");
      printf("%s", external_name(name->value.str));
      in--;
    } else {
      printf("%s_%s", module_name, word_name);
    }
    if(ONEOF(c->op, OP_ap, OP_compose)) {
      assert_error(in >= 1);
      printf("%d%d", in-1, out);
    } else if(ONEOF(c->op, OP_quote, OP_pushr)) {
      assert_error(in >= 1 && out == 0);
      printf("%d", in-1);
    }
    if(!ONEOF(c->op, OP_external, OP_external_io)) print_type_suffix(e, c);
    printf("(");

    COUNTUP(i, in) {
      int a = tr_index(c->expr.arg[i]);
      printf("%s%s%d", sep, cname(trace_type(&e[a])), a);
      sep = ", ";
    };

    if(partial) {
      if(trace_type(c) == T_BOTTOM) {
        printf("%sNULL", sep);
      } else {
        printf("%s&%s%d", sep, cname(t), lhs);
      }
      sep = ", ";
    }

    RANGEUP(i, start_out, n) {
      int a = tr_index(c->expr.arg[i]);
      if(a <= 0) {
        printf("%sNULL", sep);
      } else {
        printf("%s&%s%d", sep, cname(trace_type(&e[a])), a);
      }
      sep = ", ";
    }

    if(partial) {
      if(next_block) {
        printf(")) goto block%d;\n", next_block);
      } else {
        printf("));\n");
      }
    } else {
      printf(");\n");
    }
  }
}

// print the RHS to initialize a value
void gen_value_rhs(cell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("%d;\n", (int)c->value.integer);
    break;
  case T_STRING: {
    char *s = c->value.str;
    int n = strlen(s);
    printf("{ .s = \"%s\", .n = %d };\n", s, n);
    break;
  }
  case T_LIST:
    assert_error(list_size(c) == 0);
    printf("arr_new();\n");
    break;
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
        if(*p && tr_index(*p) == until) return;
      }
      gen_instruction(e, c);
      FLAG_SET(*c, trace, TRACED);
    }
  }
}

void gen_assert(cell_t *e, cell_t *c) {
  if(FLAG(*c, trace, TRACED)) return;
  int
    i = c - e,
    ip = tr_index(c->expr.arg[0]),
    iq = tr_index(c->expr.arg[1]);
  const char *cn = cname(trace_type(c));
  cell_t *ret = NULL;
  printf("\n  // assert %d\n", iq);
  cell_t *end = e + trace_entry_size(e);
  bool bottom = trace_type(c) == T_BOTTOM;
  if(!bottom) {
    // use #define to replace references to the assertion output to the output of arg[0]
    printf("#define %s%d %s%d\n", cn, i, cname(trace_type(&e[ip])), ip); // a little HACKy
  }

  if(!set_insert(iq, assert_set, LENGTH(assert_set))) {
    FOR_TRACE(p, e, closure_next(c) - e) {
      if(!ret && trace_type(p) == T_RETURN) {
        ret = p;
      }
      if(p->op == OP_assert && tr_index(p->expr.arg[1]) == iq) {
        ret = NULL;
      }
    }
    if(ret) {
      cell_t *next = ret + closure_cells(ret);
      if(next < end) {
        printf("  if(!%s%d)", cname(trace_type(&e[iq])), iq);
        printf(" goto block%d;\n", (int)(next - e));
      } else {
        printf("  assert_error(%s%d);\n", cname(trace_type(&e[iq])), iq);
      }
    }
  }
}

void undef_asserts(cell_t *e) {
  FOR_TRACE(c, e) {
    type_t t = trace_type(c);
    if(c->op == OP_assert && t != T_BOTTOM) {
      printf("#undef %s%d\n", cname(t), (int)(c - e));
    }
  }
}

bool external_includes(cell_t *e) {
  bool has_external_includes = false;
  FOR_TRACE(c, e) {
    if(ONEOF(c->op, OP_external, OP_external_io)) {
      cell_t *name = &e[tr_index(c->expr.arg[closure_in(c) - 1])];
      if(print_external_header(name->value.str)) {
        has_external_includes = true;
      }
    }
  }
  return has_external_includes;
}

void gen_function(cell_t *e) {
  e->op = OP_value;
  zero(assert_set);

  gen_function_signature(e);
  printf("\n{\n");
  gen_body(e);
  if(e->entry.rec || e->entry.alts > 1) printf("}\n");
  undef_asserts(e);
  printf("} // end %s_%s\n", e->module_name, e->word_name);

  FOR_TRACE(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      cell_t *x = get_entry(c);
      if(x != e && e->op != OP_null) {
        printf("\n");
        gen_function(x);
      }
    }
  }
}

void clear_ops(cell_t *e) {
  FOR_TRACE(c, e) {
    if(c->op == OP_exec) {
      cell_t *x = get_entry(c);
      x->op = OP_null;
    }
  }
}

COMMAND(cc, "print C code for given function") {
  if(rest) {
    command_define(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);

    if(e) {
      if(external_includes(e)) {
        printf("\n");
      }
      gen_function_signatures(e);
      printf("\n");
      gen_function(e);
      clear_ops(e);
    }
  }
  if(command_line) quit = true;
}
