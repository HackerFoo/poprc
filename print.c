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
#include <inttypes.h>

#if INTERFACE
#include <stdio.h>
#endif

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "trace.h"
#include "parse.h"
#include "print.h"
#include "module.h"
#include "user_func.h"
#include "list.h"
#include "lex.h"

static BITSET_INDEX(visited, cells);
static BITSET_INDEX(marked, cells);

static enum {
  BASE_DEC = 0,
  BASE_HEX
} display_base = BASE_DEC;

void mark_cell(cell_t *c) {
  if(is_cell(c)) {
    set_bit(marked, CELL_INDEX(c));
  }
}

char const *show_alt_set(uintptr_t as) {
  static char out[sizeof(as)*4+1];
  char *p = out;
  unsigned int n = sizeof(as)*4;
  const unsigned int shift = sizeof(as) * 8 - 2;
  uintptr_t mask = ((uintptr_t)3) << shift;

  while(!(as & mask) && n) {
    as <<= 2;
    n--;
  }
  while(n--) {
    switch(as >> shift) {
    case 0: *p++ = 'X'; break;
    case 1: *p++ = '0'; break;
    case 2: *p++ = '1'; break;
    case 3: *p++ = 'E'; break;
    }
    as <<= 2;
  }
  *p++ = '\0';
  return out;
}

// only valid on primitives
char const *entry_function_name(cell_t *e) {
  if(is_user_func(e)) return NULL;
  const char *s = e->word_name;
  while(*s++);
  return s;
}

#define OP__ITEM(name) #name,
static const char *_op_name[] = {
  OP__ITEM(null)
#include "op_list.h"
};

char const *op_name(op op) {
  assert_error(op < OP_COUNT);
  return _op_name[op];
}

void get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if(is_user_func(c)) {
    cell_t *e = c->expr.arg[closure_in(c)];
    if(is_trace_cell(e)) {
      *module_name = e->module_name;
      *word_name = e->word_name;
    } else {
      *module_name = "null";
      *word_name = "null";
    }
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = op_name(c->op);
  }
}

char const *function_token(const cell_t *c) {
  static char ap_str[] = "ap00";
  op op = c->op;
  if(op == OP_ap) {
    csize_t
      in = closure_in(c),
      out = closure_out(c),
      n = closure_args(c);

    if(n == 2) {
      if(out) {
        return "popr";
      } else {
        return "pushl";
      }
    } else {
      ap_str[2] = in <= 10 ? '0' + in - 1 : 'X';
      ap_str[3] = out <= 9 ? '0' + out : 'X';
      return ap_str;
    }
  }

  FORMAP(i, primitive_module) {
    pair_t *p = &primitive_module[i];
    cell_t *e = (cell_t *)p->second;
    if(e && e->op == op)
      return (char *)p->first;
  }
  return NULL;
}

/* Graphviz graph generation */
void make_graph(char const *path, cell_t const *c) {
  FILE *f = fopen(path, "w");
  fprintf(f, "digraph g {\n"
             "label=\"%s\";\n"
             "labelloc=bottom\n"
             "labeljust=right\n"
             "graph [\n"
             "rankdir = \"RL\"\n"
             "];\n", path);
  zero(visited);
  graph_cell(f, c);
  fprintf(f, "}\n");
  fclose(f);
}

void make_graph_all(char const *path) {
  static char autopath[16];
  static unsigned int autopath_count = 0;
  if(!path && autopath_count < 1000) {
    snprintf(autopath, sizeof(autopath), "graph%03d.dot", autopath_count++);
    path = autopath;
  }
  char label[sizeof(tag_t) + 1];
  label[sizeof(tag_t)] = 0;
  get_tag(label);
  FILE *f = fopen(path, "w");
  fprintf(f, "digraph g {\n"
             "label=\"%s %s\";\n"
             "labelloc=bottom\n"
             "labeljust=right\n"
             "graph [\n"
             "rankdir = \"RL\"\n"
             "];\n", path, label);
  zero(visited);
  FOREACH(i, cells) {
    graph_cell(f, &cells[i]);
  }
  fprintf(f, "}\n");
  fclose(f);
}

void print_cell_pointer(FILE *f, cell_t *p) {
  if(p == &fail_cell) {
    fprintf(f, "<font color=\"red\">&amp;fail_cell</font>");
  } else if(p == &nil_cell) {
    fprintf(f, "<font color=\"gray70\">&amp;nil_cell</font>");
  } else if(is_cell(p)) {
    fprintf(f, "<font color=\"gray70\">&amp;cells[%d]</font>", CELL_INDEX(p));
  } else {
    fprintf(f, "<font color=\"gray70\">%p</font>", (void *)p);
  }
}

void graph_cell(FILE *f, cell_t const *c) {
  c = clear_ptr(c);
  if(!is_closure(c) || !is_cell(c)) return;
  int node = CELL_INDEX(c);
  int border = check_bit(marked, node) ? 4 : 0;
  clear_bit(marked, node);
  if(check_bit(visited, node)) return;
  set_bit(visited, node);
  csize_t n = closure_args(c);
  csize_t s = calculate_cells(n);

  COUNTUP(i, s) {
    set_bit(visited, node+i);
  }

  if(c->n == PERSISTENT || is_map(c)) return; // HACK

  /* print node attributes */
  fprintf(f, "node%d [\nlabel =<", node);

  const char *module_name, *word_name;
  get_name(c, &module_name, &word_name);

  fprintf(f, "<table border=\"%d\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>(%d) ",
          border,
          node);
  if(is_user_func(c)) {
    fprintf(f, "%s.", module_name);
  }
  fprintf(f, "%s%s ",
          word_name,
          closure_is_ready(c) ? "" : "*");
  if(is_value(c)) {
    fprintf(f, "%s ", show_type_all_short(c));
  }

  if(is_root(c)) {
    fprintf(f, "(x%u)", (unsigned int)c->n + 1);
  } else {
    fprintf(f, "x%u", (unsigned int)c->n + 1);
  }

  fprintf(f, "</b></font></td></tr>");
  if(c->alt) {
    fprintf(f, "<tr><td port=\"alt\">alt: ");
    print_cell_pointer(f, c->alt);
    fprintf(f, "</td></tr>");
  }
  if(is_value(c)) {
    if(c->value.alt_set) {
      fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
              show_alt_set(c->value.alt_set));
    }
    if(is_list(c)) {
      csize_t n = list_size(c);
      if(n && FLAG(c->value, VALUE_ROW)) {
        n--;
        fprintf(f, "<tr><td port=\"ptr%u\" bgcolor=\"gray90\" >row: ", (unsigned int)n);
        print_cell_pointer(f, c->value.ptr[n]);
        fprintf(f, "</td></tr>");
      }
      while(n--) {
        fprintf(f, "<tr><td port=\"ptr%u\">ptr: ", (unsigned int)n);
        print_cell_pointer(f, c->value.ptr[n]);
        fprintf(f, "</td></tr>");
      }
    }
    if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    }
    if(c->value.var) {
      fprintf(f, "<tr><td bgcolor=\"orange\">trace: %d.%d",
              entry_number(var_entry(c->value.var)),
              (int)var_index(c->value.var));
      if(FLAG(c->value, VALUE_DEP)) {
        fprintf(f, "[%d]", (int)c->pos);
      }
      fprintf(f, "</td></tr>");
    }
    if(!is_var(c)) {
      if(ONEOF(c->value.type, T_INT, T_SYMBOL)) {
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %" PRIdPTR "</td></tr>", c->value.integer);
      } else if(c->value.type == T_FLOAT && !is_var(c)) {
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %.15g</td></tr>", c->value.flt);
      }
    }
  } else {
    COUNTUP(i, closure_in(c)) {
      fprintf(f, "<tr><td port=\"arg%u\">", (unsigned int)i);
      print_cell_pointer(f, c->expr.arg[i]);
      fprintf(f, "</td></tr>");
    }
    RANGEUP(i, closure_args(c) - closure_out(c), closure_args(c)) {
      fprintf(f, "<tr><td port=\"arg%u\">", (unsigned int)i);
      fprintf(f, "out: ");
      print_cell_pointer(f, c->expr.arg[i]);
      fprintf(f, "</td></tr>");
    }
    if(c->op == OP_id && c->expr.arg[1]) {
      fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
              show_alt_set((alt_set_t)c->expr.arg[1]));
    }
  }
  if(c->pos) {
    fprintf(f, "<tr><td bgcolor=\"deepskyblue\">pos: %d</td></tr>",
            entry_number(trace_expr_entry(c->pos)));
  }
  fprintf(f, "</table>>\nshape = \"none\"\n];\n");

  /* print edges */
  if(is_cell(c->alt)) {
    cell_t *alt = clear_ptr(c->alt);
    fprintf(f, "node%d:alt -> node%d:top;\n",
            node, CELL_INDEX(alt));
    graph_cell(f, c->alt);
  }
  if(is_value(c)) {
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--) {
        if(is_cell(c->value.ptr[n])) {
          fprintf(f, "node%d:ptr%u -> node%d:top;\n",
                  node, (unsigned int)n, CELL_INDEX(c->value.ptr[n]));
          graph_cell(f, c->value.ptr[n]);
        }
      }
    }
  } else {
    csize_t start_out = closure_args(c) - closure_out(c);
    COUNTUP(i, start_out) {
      cell_t *arg = clear_ptr(c->expr.arg[i]);
      if(is_cell(arg)) {
        fprintf(f, "node%d:arg%d -> node%d:top;\n",
                node, (unsigned int)i, CELL_INDEX(arg));
        graph_cell(f, arg);
      }
    }
    RANGEUP(i, start_out, n) {
      cell_t *arg = clear_ptr(c->expr.arg[i]);
      if(is_cell(arg)) {
        fprintf(f, "node%d:arg%d -> node%d:top [color=lightgray];\n",
                node, (unsigned int)i, CELL_INDEX(arg));
        graph_cell(f, arg);
      }
    }
  }
}

static
void print_int(val_t x) {
  switch(display_base) {
  case BASE_HEX:
    printf(" 0x%" PRIxPTR, x);
    break;
  case BASE_DEC:
  default:
    printf(" %" PRIdPTR, x);
    break;
  }
}

void show_int(cell_t const *c) {
  assert_error(c && type_match(T_INT, c));
  print_int(c->value.integer);
}

void show_float(cell_t const *c) {
  assert_error(c && type_match(T_FLOAT, c));
  printf(" %.15g", c->value.flt);
}

bool any_alt_overlap(cell_t const * const *p, csize_t size) {
  uintptr_t  mask = 0;
  while(size--) {
    if(is_value(*p)) {
      alt_set_t m = as_mask((*p)->value.alt_set);
      if(mask & m) return true;
      mask |= m;
    }
    p++;
  }
  return false;
}

csize_t any_conflicts(cell_t const * const *p, csize_t size) {
  uintptr_t as = 0;
  COUNTUP(i, size) {
    if(is_value(*p)) {
      as |= (*p)->value.alt_set;
      if(as_conflict(as)) {
        return size - i;
      }
    }
    p++;
  }
  return 0;
}

void show_list_elements(cell_t const *c) {
  csize_t n = list_size(c);
  if(!n) return;
  if(is_row_list(c)) {
    show_list_elements(c->value.ptr[--n]);
  }
  COUNTDOWN(i, n) {
    show_one(c->value.ptr[i]);
  }
}

void show_list(cell_t const *c) {
  assert_error(c && is_list(c));
  csize_t n = list_size(c);
  if(!n) {
    printf(" []");
  } else {
    printf(" [");
    show_list_elements(c);
    printf(" ]");
  }
}

void show_func(cell_t const *c) {
  int n = closure_args(c);
  char const *s = function_token(c);
  if(!s) return;
  if(is_placeholder(c)) printf(" ?%d =", CELL_INDEX(c));
  COUNTUP(i, n) {
    cell_t *arg = c->expr.arg[i];
    if(is_closure(arg)) {
      show_one(arg);
    }
  }
  if(c->op != OP_id) { // to reduce noise and allow diff'ing test output
    printf(" %s", s);
  }
}

void show_var(cell_t const *c) {
  assert_error(is_var(c));
  if(is_list(c)) {
    show_list(c);
  } else {
    printf(" ?%c%d", type_char(c->value.type), CELL_INDEX(c));
  }
}

void show_one(cell_t const *c) {
  if(!c) {
    printf(" []");
  } else if(!is_closure(c)) {
    printf(" ?");
  } else if(!is_value(c)) {
    show_func(c);
  } else if(is_fail(c)) {
    printf(" {}");
  } else if(is_var(c)) {
    show_var(c);
  } else if(type_match(T_INT, c)) {
    show_int(c);
  } else if(type_match(T_FLOAT, c)) {
    show_float(c);
  } else if(type_match(T_LIST, c)) {
    show_list(c);
  } else if(type_match(T_SYMBOL, c)) {
    val_t x = c->value.integer;
    const char *str = symbol_string(x);
    if(str) {
      printf(" %s", str);
    } else {
      printf(" UnknownSymbol");
    }
  } else {
    printf(" ?");
  }
}

void show_alts(cell_t const *c) {
  cell_t const *p = c;
  while(p) {
    putchar(' ');
    show_list_elements(p);
    putchar('\n');
    p = p->alt;
  }
}

char *show_type(type_t t) {
#define _case(x) case x: return #x
  switch(t) {
  _case(T_ANY);
  _case(T_INT);
  _case(T_LIST);
  _case(T_SYMBOL);
  _case(T_MAP);
  _case(T_STRING);
  _case(T_RETURN);
  _case(T_FLOAT);
  _case(T_BOTTOM);
  _case(T_MODULE);
  default: return "???";
  }
#undef case
}

// unsafe
char *show_type_all(const cell_t *c) {
  const static char *type_flag_name[] = {
    "VAR",
    "FAIL",
    "TRACED",
    "ROW",
    "CHANGES",
    "DEP"
  };
  static char buf[64];
  char *p = buf;
  p += sprintf(p, "%s", show_type(c->value.type));
  FOREACH(i, type_flag_name) {
    if(FLAG(c->value, 0x80 >> i)) {
      p += sprintf(p, "|%s", type_flag_name[i]);
    }
  }
  return buf;
}

char type_char(type_t t) {
  switch(t) {
  case T_ANY: return 'a';
  case T_INT: return 'i';
  case T_LIST: return 'l';
  case T_SYMBOL: return 's';
  case T_MAP: return 'm';
  case T_STRING: return 'S';
  case T_RETURN: return 'r';
  case T_FLOAT: return 'd';
  case T_BOTTOM: return '0';
  case T_MODULE: return 'M';
  }
  return 'x';
}

// unsafe
char *show_type_all_short(const cell_t *c) {
  const static char type_flag_char[] = {
    '?',
    '!',
    '.',
    '@',
    '*',
    '^'
  };
  static char buf[LENGTH(type_flag_char) + 1];

  char *p = buf;

  FOREACH(i, type_flag_char) {
    if(FLAG(c->value, 0x80 >> i)) {
      *p++ = type_flag_char[i];
    }
  }

  *p++ = type_char(c->value.type);
  *p = 0;
  return buf;
}

FORMAT(cell, 'C') {
  cell_t *c = (cell_t *)i;
  if(is_cell(c)) {
    printf("%d", CELL_INDEX(c));
    mark_cell(c);
  } else if(c == &nil_cell) {
    printf("nil");
  } else if(c == &fail_cell) {
    printf("fail");
  } else {
    printf("X");
  }
}

FORMAT(function, 'O') {
  printf("%s", op_name(i));
}

void breakpoint_hook() {
  print_active_entries("  - while compiling ");
  make_graph_all(NULL);
}

static char *show_base[] = {
  [BASE_DEC] = "decimal",
  [BASE_HEX] = "hexadecimal"
};

COMMAND(base, "set numeric base for display") {
  if(rest) {
    seg_t base = tok_seg(rest);
    if(segcmp("dec", base) == 0) {
      display_base = BASE_DEC;
    } else if(segcmp("hex", base) == 0) {
      display_base = BASE_HEX;
    } else {
      printf("Unknown base\n");
    }
  } else {
    printf("Base is %s\n",
           show_base[min(display_base, LENGTH(show_base)-1)]);
  }
}
