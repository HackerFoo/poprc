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

static uintptr_t assert_set[67];

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
    [T_OPAQUE]   = "void *",
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
    [T_OPAQUE]   = "ptr",
    [T_BOTTOM]   = "bot"
  };
  assert_error(t < LENGTH(table));
  return table[t];
}

// print a suffix to select the mode of some primitives
void print_type_suffix(const cell_t *entry, const cell_t *c) {
  if(ONEOF(c->op, OP_assert, OP_exec)) return;
  printf("_%c", type_char(trace_type(c)));
  TRAVERSE(c, const, args) {
    char ch;
    if(*p) {
      const cell_t *ta = &entry[cgen_index(entry, *p)];
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

void gen_function_signature(const cell_t *e) {
  const cell_t *p = e + 1;
  csize_t out_n = e->entry.out;
  type_t rtypes[out_n];
  resolve_types(e, rtypes);

  printf("%s%s_%s(", ctype(rtypes[0]), e->module_name, e->word_name);
  char *sep = "";
  COUNTDOWN(i, e->entry.in) {
    const cell_t *a = &p[i];
    type_t t = trace_type(a);
    printf("%s%s%s%d", sep, ctype(t), cname(t), (int)i + 1);
    sep = ", ";
  }

  RANGEUP(i, 1, out_n) {
    printf("%s%s*out_%s%d", sep, ctype(rtypes[i]), cname(rtypes[i]), (int)i-1);
  }

  printf(")");
}

void gen_function_signatures(const cell_t *e) {
  gen_function_signature(e);
  printf(";\n");

  FOR_TRACE_CONST(c, e) {
    if(c->op == OP_exec && c->trace.type != T_BOTTOM) {
      const cell_t *x = get_entry(c);
      if(x != e) {
        gen_function_signatures(x);
      }
    }
  }
}

void gen_next_block(const cell_t *e, const cell_t *c) {
  const cell_t *end = e + e->entry.len;
  const cell_t *next = closure_next_const(c);
  if(next <= end) {
    printf("}\n\nblock%d: {\n", (int)(next - e));
  }
}

void gen_body(const cell_t *e) {
  printf("\nentry: {\n");
  bool skip = false;
  FOR_TRACE_CONST(c, e) {
    if(!skip && !is_var(c)) {
      skip = gen_instruction(e, c);
    }
    if(is_return(c)) {
      skip = false;
      gen_next_block(e, c);
    }
  }
}

void gen_return(const cell_t *e, const cell_t *l) {
  csize_t out_n = list_size(l);
  int ires = cgen_index(e, l->value.ptr[out_n - 1]);

  // skip if T_BOTTOM
  COUNTDOWN(i, out_n) {
    int ai = cgen_index(e, l->value.ptr[i]);
    type_t t = trace_type(&e[ai]);
    if(t == T_BOTTOM) return;
  }

  COUNTDOWN(i, out_n-1) {
    int ai = cgen_index(e, l->value.ptr[i]);
    const cell_t *a = &e[ai];
    type_t t = trace_type(a);
    const char *n = cname(t);
    printf("  if(out_%s%d) *out_%s%d = %s%d;\n",
           n, (int)i, n, (int)i, n, ai);
  }
  printf("  return %s%d;\n", cname(trace_type(&e[ires])), ires);
}

void gen_decls(cell_t *e) {
  FOR_TRACE(c, e) {
    if(is_var(c)) continue;
    type_t t = trace_type(c);
    if(!ONEOF(t, T_RETURN, T_BOTTOM)) {
      if(c->op == OP_value) {
        if(NOT_FLAG(*c, trace, IMMEDIATE)) {
          FLAG_SET(*c, trace, DECL);
        }
      } else if(c->n ||
                is_dep(c) ||
                FLAG(*c, expr, PARTIAL)) {
        int x = cgen_lookup(e, c);
        if(x) {
          FLAG_SET(e[x], trace, DECL);
        }
      }
    }
  }

  // T_BOTTOM is (ab)used to indicate a new line needs to be started
  type_t last_type = T_BOTTOM;
  FOR_TRACE_CONST(c, e) {
    if(is_var(c)) continue;
    int i = c - e;
    type_t t = trace_type(c);
    if(FLAG(*c, trace, DECL)) {
      if(c->op == OP_value) {
        if(last_type != T_BOTTOM) printf(";\n");
        printf("  const %s%s%d = ", ctype(t), cname(t), i);
        gen_value_rhs(c);
        last_type = T_BOTTOM;
      } else {
        if(last_type == t) {
          printf(", %s%d", cname(t), i);
        } else {
          if(last_type != T_BOTTOM) printf(";\n");
          printf("  %s%s%d", ctype(t), cname(t), i);
          last_type = t;
        }
      }
    }
  }

  if(last_type != T_BOTTOM) printf(";\n");
}

bool gen_skip(const cell_t *c) {
  return ONEOF(c->op, OP_dep, OP_seq, OP_otherwise);
}

static
bool last_call(const cell_t *e, const cell_t *c) {
  c = closure_next_const(c);
  FOR_TRACE_CONST(p, e, c - e) {
    if(!gen_skip(p)) {
      return is_return(p);
    }
  }
  return false;
}

// returns true if the rest of the block should be skipped
bool gen_instruction(const cell_t *e, const cell_t *c) {
  if(is_return(c))         { gen_return(e, c);    } else
  if(gen_skip(c) ||
     is_value(c))          { /* nothing */        } else
  if(c->op == OP_assert)   { gen_assert(e, c);    } else
  if(get_entry(c) == e &&
     last_call(e, c))      { gen_tail_call(e, c);
                             return true;         } else
                           { gen_call(e, c);      }
  return false;
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

bool gen_is_aliased(const cell_t *c) {
  return ONEOF(c->op, OP_seq, OP_assert, OP_otherwise);
}

int cgen_lookup(const cell_t *e, const cell_t *c) {
  LOOP(e->entry.len) {
    switch(c->op) {
    case OP_seq:
    case OP_assert:
      c = &e[tr_index(c->expr.arg[0])];
      break;
    case OP_otherwise:
      c = &e[tr_index(c->expr.arg[1])];
      break;
    default:
      return c - e;
    }
  }
  assert_error(false);
  return 0;
}

int cgen_index(const cell_t *e, const cell_t *a) {
  return cgen_lookup(e, &e[tr_index(a)]);
}

void gen_tail_call(const cell_t *e, const cell_t *c) {
  csize_t in = closure_in(c);
  printf("\n  // tail call\n");

  // overwrite function arguments with new values
  COUNTUP(i, in) {
    int a = cgen_index(e, c->expr.arg[i]);
    printf("  %s%d = %s%d;\n",
           cname(trace_type(&e[in - i])),
           (int)(in - i),
           cname(trace_type(&e[a])), a);
  };

  // jump to the beginning
  printf("  goto entry;\n");
}

// find next_block
int find_next_block(const cell_t *e, const cell_t *c) {
  FOR_TRACE_CONST(p, e, closure_next_const(c) - e) {
    if(is_return(p)) {
      int size = calculate_cells(p->size);
      if(e->entry.len + 1 - (p-e) > size) { // if this isn't the last return
        return p - e + size;
      } else {
        return 0;
      }
    }
  }
  return 0;
}

void gen_call(const cell_t *e, const cell_t *c) {
  int lhs = c - e;
  char *sep = "";
  const char *module_name, *word_name;

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
    next_block = find_next_block(e, c);
    if(next_block) {
      printf("  if(");
    } else {
      printf("  assert_error(!");
    }
  } else {
    if(trace_type(c) == T_BOTTOM) {
      printf("  ");
    } else {
      printf("  %s%s%d = ", FLAG(*c, trace, DECL) ? "" : ctype(t), cname(t), lhs);
    }
  }
  if(c->op == OP_external) {
    const cell_t *name = &e[cgen_index(e, c->expr.arg[closure_in(c) - 1])];
    assert_error(is_string(name) && !is_var(name),
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
  if(c->op != OP_external) print_type_suffix(e, c);
  printf("(");

  COUNTUP(i, in) {
    int a = cgen_index(e, c->expr.arg[i]);
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
    int a = cgen_index(e, c->expr.arg[i]);
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

// print the RHS to initialize a value
void gen_value_rhs(const cell_t *c) {
  type_t t = trace_type(c);
  switch(t) {
  case T_INT:
  case T_SYMBOL:
    printf("%d;\n", (int)c->value.integer);
    break;
  case T_STRING: {
    seg_t str = value_seg(c);
    printf("{ .s = \"");
    print_escaped_string(str);
    printf("\", .n = %d };\n", (int)str.n);
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

void gen_assert(const cell_t *e, const cell_t *c) {
  int iq = cgen_index(e, c->expr.arg[1]);
  const cell_t *ret = NULL;
  const cell_t *end = e + trace_entry_size(e);

  if(!set_insert(iq, assert_set, LENGTH(assert_set))) {
    FOR_TRACE_CONST(p, e, closure_next_const(c) - e) {
      if(!ret && trace_type(p) == T_RETURN) {
        ret = p;
      }
      if(p->op == OP_assert && cgen_index(e, p->expr.arg[1]) == iq) {
        ret = NULL;
      }
    }
    if(ret) {
      const cell_t *next = ret + closure_cells(ret);
      if(next < end) {
        printf("  if(!%s%d)", cname(trace_type(&e[iq])), iq);
        printf(" goto block%d; // assert\n", (int)(next - e));
      } else {
        printf("  assert_error(%s%d);\n", cname(trace_type(&e[iq])), iq);
      }
    }
  }
}

bool external_includes(const cell_t *e) {
  bool has_external_includes = false;
  FOR_TRACE_CONST(c, e) {
    if(c->op == OP_external) {
      const cell_t *name = &e[cgen_index(e, c->expr.arg[closure_in(c) - 1])];
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
  gen_decls(e);
  gen_body(e);
  printf("}\n");
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
