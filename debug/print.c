/* Copyright 2012-2020 Dustin DeWeese
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
#include "startle/static_alloc.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "parse/parse.h"
#include "debug/print.h"
#include "module.h"
#include "user_func.h"
#include "list.h"
#include "parse/lex.h"
#include "debug/tags.h"
#include "var.h"

STATIC_ALLOC_DEPENDENT(visited, uint8_t, (cells_size + 7) / 8);
STATIC_ALLOC_DEPENDENT(marked, uint8_t, (cells_size + 7) / 8);

static enum {
  BASE_DEC = 0,
  BASE_HEX,
  BASE_BIN
} display_base = BASE_DEC;

void mark_cell(cell_t *c) {
  if(is_cell(c)) {
    set_bit(marked, CELL_INDEX(c));
  }
}

// mark nodes in the context to provide a visual stack trace
void mark_ctx(const context_t *ctx) {
  FOLLOW(p, ctx, up) {
    cell_t **cp = p->src;
    if(cp && *cp) {
      mark_cell(*cp);
    }
  }
}

char const *show_alt_set(uintptr_t as) {
  static char out[sizeof(as)*4+1];
  char *p = out;
  unsigned int n = sizeof(as)*4;
  const unsigned int shift = sizeof_bits(as) - 2;
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

FORMAT(alt_set, 'S') {
  printf("%s", show_alt_set(i));
}

// only valid on primitives
char const *entry_function_name(cell_t *e) {
  if(is_user_func(e)) return NULL;
  const char *s = e->word_name;
  while(*s++);
  return s;
}

#define OP__ITEM(file, line, name) #name,
static const char *_op_name[] = {
  OP__ITEM(BASENAME, __LINE__, null)
#include "op_list.h"
};

char const *op_name(op op) {
  assert_error(op < OP_COUNT);
  return _op_name[op];
}

void get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if(!c) {
    *module_name = "null";
    *word_name = "null";
  } else if(is_user_func(c)) {
    tcell_t *e = (tcell_t *)c->expr.arg[closure_in(c)];
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
             "bgcolor=gray10\n"
             "color=white\n"
             "fontcolor=white\n"
             "graph [\n"
             "rankdir = \"RL\"\n"
             "];\n", path);
  static_zero(visited);
  graph_cell(f, c);
  fprintf(f, "}\n");
  fclose(f);
}

static
void print_relations(FILE *f) {
  STATIC_FOREACH(i, related_link) {
    pair_t *p = &related_link[i];
    const cell_t *x = (const cell_t *)p->first;
    const cell_t *y = (const cell_t *)p->second;
    if(is_cell(x) &&
       is_cell(y) &&
       is_closure(x) &&
       is_closure(y) &&
       x->alt != y &&
       !(check_bit(visited, CELL_INDEX(x)) &&
         check_bit(visited, CELL_INDEX(y)))) {
      fprintf(f, "node%d:top -> node%d:top [style=dashed, color=gray50, arrowhead=none];\n",
              CELL_INDEX(x), CELL_INDEX(y));
      set_bit(visited, CELL_INDEX(x));
      set_bit(visited, CELL_INDEX(y));
    }
  }
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
             "bgcolor=gray10\n"
             "color=white\n"
             "fontcolor=white\n"
             "graph [\n"
             "rankdir = \"RL\"\n"
             "];\n", path, label);
  static_zero(visited);
  print_relations(f);
  static_zero(visited);
  static_zero(marked);
  if(current_ctx) mark_ctx(current_ctx);
  STATIC_FOREACH(i, cells) {
    graph_cell(f, &cells[i]);
  }
  fprintf(f, "}\n");
  fclose(f);
}

void print_cell_pointer(FILE *f, const char *pre, cell_t *p) {
  if(pre) fprintf(f, "<font color=\"white\">%s: </font>", pre);
  if(p == &fail_cell) {
    fprintf(f, "<font color=\"red\">&amp;fail_cell</font>");
  } else if(is_cell(p)) {
    fprintf(f, "<font color=\"gray60\">&amp;cells[%d]</font>", CELL_INDEX(p));
  } else {
    fprintf(f, "<font color=\"gray60\">%p</font>", (void *)p);
  }
}

void graph_cell(FILE *f, cell_t const *c) {
  if(!is_closure(c) || !is_cell(c)) return;
  int node = CELL_INDEX(c);
  bool is_marked = check_bit(marked, node);
  int border = is_marked ? 4 : 0;
  if(check_bit(visited, node)) return;
  set_bit(visited, node);
  csize_t n = closure_args(c);
  csize_t s = calculate_cells(n);

  COUNTUP(i, s) {
    set_bit(visited, node+i);
  }

  if(is_persistent(c) || is_map(c)) return; // HACK

  /* print node attributes */
  fprintf(f, "node%d [\ncolor=white\nlabel =<", node);

  const char *module_name, *word_name;
  get_name(c, &module_name, &word_name);

  fprintf(f, "<table border=\"%d\" bgcolor=\"gray20\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"gray40\"><font color=\"white\"><b>(%d) ",
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
    fprintf(f, "<tr><td port=\"alt\">");
    print_cell_pointer(f, "alt", c->alt);
    fprintf(f, "</td></tr>");
  }
  if(is_value(c)) {
    if(c->value.alt_set) {
      fprintf(f, "<tr><td><font color=\"white\">alt_set: X%s</font></td></tr>",
              show_alt_set(c->value.alt_set));
    }
    if(is_list(c)) {
      csize_t n = list_size(c);
      if(n && FLAG(*c, value, ROW)) {
        n--;
        fprintf(f, "<tr><td port=\"ptr%u\">", (unsigned int)n);
        print_cell_pointer(f, "row", c->value.ptr[n]);
        fprintf(f, "</td></tr>");
      }
      while(n--) {
        fprintf(f, "<tr><td port=\"ptr%u\">", (unsigned int)n);
        print_cell_pointer(f, "ptr", c->value.ptr[n]);
        fprintf(f, "</td></tr>");
      }
    }
    if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    }
    if(c->value.var) {
      tcell_t *entry = var_entry(c->value.var);
      fprintf(f, "<tr><td bgcolor=\"orange\">trace: %d.%d",
              entry_number(entry),
              (int)var_index_nofail(entry, c->value.var));
      if(FLAG(*c, value, DEP)) {
        fprintf(f, "[%d]", (int)c->arg_index);
      }
      fprintf(f, "</td></tr>");
    }
    if(!is_var(c)) {
      switch(c->value.type) {
      case T_INT:
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %" PRIdPTR "</td></tr>", c->value.integer);
        break;
      case T_SYMBOL:
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %s</td></tr>", symbol_string(c->value.symbol));
        break;
      case T_FLOAT:
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %.15g</td></tr>", c->value.flt);
        break;
      case T_STRING: {
        seg_t str = value_seg(c);
        char buf[64];
        int len = escape_string(buf, sizeof(buf), str, true);
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %.*s</td></tr>", (int)len, buf);
      } break;
      case T_LIST:
        break;
      default:
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: ?</td></tr>");
        break;
      }
    }
  } else {
    COUNTUP(i, closure_in(c)) {
      fprintf(f, "<tr><td port=\"arg%u\">", (unsigned int)i);
      print_cell_pointer(f, NULL, c->expr.arg[i]);
      fprintf(f, "</td></tr>");
    }
    RANGEUP(i, closure_args(c) - closure_out(c), closure_args(c)) {
      fprintf(f, "<tr><td port=\"arg%u\">", (unsigned int)i);
      print_cell_pointer(f, "out", c->expr.arg[i]);
      fprintf(f, "</td></tr>");
    }
    if(c->op == OP_id && c->expr.arg[1]) {
      fprintf(f, "<tr><td><font color=\"white\">alt_set: X%s</font></td></tr>",
              show_alt_set((alt_set_t)c->expr.arg[1]));
    }
  }
  if(c->pos && !(is_var(c) && FLAG(*c, value, DEP))) {
    tcell_t *e = pos_entry(c->pos);
    if(e) {
      if(is_value(c)) {
        fprintf(f, "<tr><td bgcolor=\"deepskyblue\">lift: %d</td></tr>", entry_number(e->entry.parent));
      } else {
        fprintf(f, "<tr><td bgcolor=\"deepskyblue\">contain: %d</td></tr>", entry_number(e));
      }
    }
  }
  const char *tag = get_ptr_tag(c);
  if(tag) {
    fprintf(f, "<tr><td><font color=\"white\">tag: %s</font></td></tr>", tag);
  }
  if(c->src.s) {
    fprintf(f, "<tr><td><font color=\"white\">");
    fprintf_escaped_string(f, c->src, true);
    fprintf(f, "</font></td></tr>");
  }
  fprintf(f, "</table>>\nshape = \"none\"\n];\n");

  /* print edges */
  if(is_cell(c->alt)) {
    cell_t *alt = c->alt;
    fprintf(f, "node%d:alt -> node%d:top [color=white];\n",
            node, CELL_INDEX(alt));
    graph_cell(f, c->alt);
  }
  if(is_value(c)) {
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--) {
        cell_t *ptr = c->value.ptr[n];
        if(is_cell(ptr)) {
          fprintf(f, "node%d:ptr%u -> node%d:top [color=white%s];\n",
                  node, (unsigned int)n, CELL_INDEX(ptr),
                  STR_IF(is_marked && check_bit(marked, CELL_INDEX(ptr)) , ", penwidth=3"));
          graph_cell(f, ptr);
        }
      }
    }
  } else {
    csize_t start_out = closure_args(c) - closure_out(c);
    COUNTUP(i, start_out) {
      cell_t *arg = c->expr.arg[i];
      if(is_cell(arg)) {
        fprintf(f, "node%d:arg%d -> node%d:top [color=white%s];\n",
                node, (unsigned int)i, CELL_INDEX(arg),
                STR_IF(is_marked && check_bit(marked, CELL_INDEX(arg)) , ", penwidth=3"));
        graph_cell(f, arg);
      }
    }
    RANGEUP(i, start_out, n) {
      cell_t *arg = c->expr.arg[i];
      if(is_cell(arg)) {
        fprintf(f, "node%d:arg%d -> node%d:top [color=gray50];\n",
                node, (unsigned int)i, CELL_INDEX(arg));
        graph_cell(f, arg);
      }
    }
  }
}

#define VAL_BITS sizeof_bits(val_t)
static
void print_binary(val_t x) {
  char bits[VAL_BITS];
  int n = 0;
  int s = 1;
  COUNTDOWN(i, VAL_BITS) {
    bits[i] = x & 1 ? '1' : '0';
    x >>= 1;
    n++;
    if(!x && s == n) break;
    if(n > s) s *= 2;
  }
  printf("%.*s", n, bits + (VAL_BITS - n));
}

static
void print_int(val_t x) {
  switch(display_base) {
  case BASE_HEX:
    printf("0x%" PRIxPTR, x);
    break;
  case BASE_BIN:
    print_binary(x);
    break;
  case BASE_DEC:
  default:
    printf("%" PRIdPTR, x);
    break;
  }
}

void show_int(cell_t const *c) {
  assert_error(has_type(c, T_INT));
  print_int(c->value.integer);
}

void show_float(cell_t const *c) {
  assert_error(has_type(c, T_FLOAT));
  printf("%.15g", c->value.flt);
}

void show_string(cell_t const *c) {
  assert_error(has_type(c, T_STRING));
  printf("\"");
  print_escaped_string(value_seg(c), false);
  printf("\"");
}

bool any_alt_overlap(cell_t const * const *p, csize_t size) {
  uintptr_t mask = 0;
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

bool show_list_elements(cell_t const *c) {
  csize_t n = list_size(c);
  if(!n) return false;
  if(!list_is_printable(c)) {
    printf("...");
    return true;
  }
  if(is_row_list(c)) {
    if(FLAG(*c, value, ABBREV)) printf("...");
    bool out = show_list_elements(c->value.ptr[--n]);
    if(!n) return out;
    if(out) putchar(' ');
  }
  show_one(c->value.ptr[n-1]);
  COUNTDOWN(i, n-1) {
    putchar(' ');
    show_one(c->value.ptr[i]);
  }
  return true;
}

void show_list(cell_t const *c) {
  assert_error(c && is_list(c));
  printf("[");
  show_list_elements(c);
  printf("]");
}

bool list_is_printable(cell_t const *l) {
  if(!is_list(l)) return false;
  cell_t **p;
  FORLIST(p, (cell_t *)l) {
    if(!is_printable(*p)) return false;
  }
  return true;
}

bool is_printable(cell_t const *c) {
  if(!c || is_value(c)) return true;
  if(is_dep(c) || closure_out(c)) return false;
  TRAVERSE(c, const_in) {
    if(*p && !is_printable(*p)) return false;
  }
  return true;
}

void show_func(cell_t const *c) {
  if(!is_dep(c) && !closure_out(c)) {
    bool first = true;
    TRAVERSE(c, const_in) {
      if(*p) {
        if(!first) putchar(' ');
        first = false;
        show_one(*p);
      }
    }
    if(!first) putchar(' ');
    if(c->op == OP_exec) {
      const char *module_name = NULL, *word_name = NULL;
      get_name(c, &module_name, &word_name);
      printf("%s.%s", module_name, word_name);
    } else {
      printf("%s", function_token(c));
    }
  } else {
    printf("...");
  }
}

void show_var(cell_t const *c) {
  assert_error(is_var(c));
  if(is_list(c)) {
    show_list(c);
  } else {
    printf("?%c%d", type_char(c->value.type), CELL_INDEX(c));
  }
}

void show_one(cell_t const *c) {
  while(c && c->op == OP_id) c = c->expr.arg[0];
  if(!c) {
    printf("[]");
  } else if(!is_closure(c)) {
    printf("?");
  } else if(!is_value(c)) {
    show_func(c);
  } else if(is_fail(c)) {
    printf("{}");
  } else if(is_var(c)) {
    show_var(c);
  } else if(has_type(c, T_INT)) {
    show_int(c);
  } else if(has_type(c, T_FLOAT)) {
    show_float(c);
  } else if(has_type(c, T_STRING)) {
    show_string(c);
  } else if(has_type(c, T_LIST)) {
    show_list(c);
  } else if(has_type(c, T_SYMBOL) ||
            has_type(c, T_OPAQUE)) {
    val_t x = c->value.symbol;
    const char *str = symbol_string(x);
    if(str) {
      printf("%s", str);
    } else {
      printf("UnknownSymbol");
    }
    if(has_type(c, T_OPAQUE)) printf("#");
  } else {
    printf("?");
  }
}

void show_alts(const char *prefix, cell_t const *c) {
  cell_t const *p = c;
  while(p) {
    printf("%s", prefix);
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
  _case(T_OPAQUE);
  _case(T_BOTTOM);
  _case(T_MODULE);
  _case(T_FAIL);
  default: return "???";
  }
#undef _case
}

// unsafe
char *show_type_all(const cell_t *c) {
  const static char *type_flag_name[] = {
    "VAR",
    "ROW",
    "LINEAR",
    "BOUNDED",
    "DEP",
    "INLINE",
    "TRACED"
  };
  static char buf[64];
  char *p = buf;
  p += sprintf(p, "%s", show_type(c->value.type));
  FOREACH(i, type_flag_name) {
    if(FLAG_(c->value.flags, 0x80 >> i)) {
      p += sprintf(p, "|%s", type_flag_name[i]);
    }
  }
  return buf;
}

// a lowercase letter to represent each type
char type_char(type_t t) {
  switch(t) {
  case T_ANY:    return 'a';
  case T_INT:    return 'i';
  case T_LIST:   return 'l';
  case T_SYMBOL: return 'y';
  case T_MAP:    return 'm';
  case T_STRING: return 's';
  case T_RETURN: return 'r';
  case T_FLOAT:  return 'd';
  case T_OPAQUE: return 'o';
  case T_BOTTOM: return 'v';
  case T_FAIL:   return 'f';
  case T_MODULE: return 'e';
  }
  return 'x';
}

// unsafe
char *show_type_all_short(const cell_t *c) {
  const static char type_flag_char[] = {
    '?',
    '@',
    '!'
  };
  static char buf[LENGTH(type_flag_char) + 1];

  char *p = buf;

  FOREACH(i, type_flag_char) {
    if(FLAG_(c->value.flags, 0x80 >> i)) {
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
  } else if(c == &fail_cell) {
    printf("fail");
  } else {
    printf("X");
  }
}

FORMAT(function, 'O') {
  printf("%s", op_name(i));
}

FORMAT(type, 't') {
  printf("%s", show_type(i));
}

#define FILE_ID(id, str) [CONCAT(FILE_ID_, id)] = str,
const char *file_names[FILE_ID_COUNT] = {
  [0] = "*none*",
  #include "file_ids.h"
};
#undef FILE_ID

const char *show_file_id(file_id_t id) {
  return file_names[id];
}

FORMAT(location, 'L') {
  location_t loc;
  loc.raw = i;
  printf("%s:%u", show_file_id(loc.file), loc.line);
}

static char *show_base[] = {
  [BASE_DEC] = "decimal",
  [BASE_HEX] = "hexadecimal",
  [BASE_BIN] = "binary"
};

COMMAND(base, "set numeric base for display") {
  if(rest) {
    seg_t base = tok_seg(rest);
    if(segcmp("dec", base) == 0) {
      display_base = BASE_DEC;
    } else if(segcmp("hex", base) == 0) {
      display_base = BASE_HEX;
    } else if(segcmp("bin", base) == 0) {
      display_base = BASE_BIN;
    } else {
      printf("Unknown base\n");
    }
  } else {
    printf("Base is %s\n",
           show_base[min(display_base, LENGTH(show_base)-1)]);
  }
}
