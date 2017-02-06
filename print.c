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
#include "gen/module.h"
#include "gen/user_func.h"

static BITSET_INDEX(visited, cells);
static BITSET_INDEX(marked, cells);

void mark_cell(cell_t *c) {
  if(is_cell(c)) {
    set_bit(marked, c - cells);
  }
}

char const *show_alt_set(uintptr_t as) {
  static char out[sizeof(as)*4+1];
  char *p = out;
  unsigned int n = sizeof(as)*4;
  const unsigned int shift = sizeof(as) * 8 - 2;
  uintptr_t mask = 3l << shift;

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
  if(e->func == func_exec || e->func == func_quote) return NULL;
  const char *s = e->word_name;
  while(*s++);
  return s;
}

char const *function_name(reduce_t *f) {
  f = (reduce_t *)clear_ptr(f);
#define CASE(x) if(f == func_##x) return #x
  CASE(value);
  CASE(fail);
  CASE(dep);
  CASE(dep_entered);
  CASE(ap);
#undef CASE
  const char *s = NULL;
  FORMAP(i, primitive_module) {
    cell_t *e = (cell_t *)primitive_module[i].second;
    if(e->func == f) {
      s = entry_function_name(e);
      break;
    }
  }
  assert(s);
  return s;
}

void get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if((reduce_t *)clear_ptr(c->func) == func_exec ||
     (reduce_t *)clear_ptr(c->func) == func_quote) {
    cell_t *e = c->expr.arg[closure_in(c) - 1];
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = function_name(c->func);
  }
}

char const *function_token(reduce_t *f) {
  f = (reduce_t *)clear_ptr(f);
  FORMAP(i, primitive_module) {
    pair_t *p = &primitive_module[i];
    cell_t *e = (cell_t *)p->second;
    if(e && e->func == f)
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
  FILE *f = fopen(path, "w");
  fprintf(f, "digraph g {\n"
             "label=\"%s\";\n"
             "labelloc=bottom\n"
             "labeljust=right\n"
             "graph [\n"
             "rankdir = \"RL\"\n"
             "];\n", path);
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
    fprintf(f, "<font color=\"lightgray\">&amp;nil_cell</font>");
  } else {
    fprintf(f, "<font color=\"lightgray\">%p</font>", (void *)p);
  }
}

void graph_cell(FILE *f, cell_t const *c) {
  c = clear_ptr(c);
  if(!is_closure(c) || !is_cell(c)) return;
  size_t node = c - cells;
  int border = check_bit(marked, node) ? 4 : 0;
  clear_bit(marked, node);
  if(check_bit(visited, node)) return;
  set_bit(visited, node);
  csize_t n = closure_args(c);
  csize_t s = calculate_cells(n);

  for(csize_t i = 0; i < s; ++i) set_bit(visited, node+i);

  if(c->n == PERSISTENT || is_map(c)) return; // HACK

  /* print node attributes */
  fprintf(f, "node%" PRIuPTR " [\nlabel =<", node);

  const char *module_name, *word_name;
  get_name(c, &module_name, &word_name);

  fprintf(f, "<table border=\"%d\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>(%" PRIuPTR ") %s.%s%s %x ",
          border,
          node,
          module_name,
          word_name,
          closure_is_ready(c) ? "" : "*",
          (int)c->size);
  if(!is_value(c)) {
    fprintf(f, "%x ", (unsigned int)c->expr.out);
  } else {
    fprintf(f, "%s ", show_type_all_short(c->value.type));
  }
  fprintf(f, "(%u%s)</b></font></td></tr>", (unsigned int)c->n, is_root(c) ? "*" : "");
  fprintf(f, "<tr><td port=\"alt\">alt: ");
  print_cell_pointer(f, c->alt);
  fprintf(f, "</td></tr>");
  if(is_value(c)) {
    fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
            show_alt_set(c->value.alt_set));
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--) {
        fprintf(f, "<tr><td port=\"ptr%u\">ptr: ", (unsigned int)n);
        print_cell_pointer(f, c->value.ptr[n]);
        fprintf(f, "</td></tr>");
      }
    } else if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else if(is_var(c)) {
        fprintf(f, "<tr><td bgcolor=\"orange\">trace: %" PRIdPTR "</td></tr>", c->value.ptr[0] - trace_cur);
    } else {
      int n = val_size(c);
      while(n--)
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %" PRIdPTR "</td></tr>", c->value.integer[n]);
    }
  } else {
    for(csize_t i = 0; i < n; i++) {
      fprintf(f, "<tr><td port=\"arg%u\">", (unsigned int)i);
      print_cell_pointer(f, c->expr.arg[i]);
      fprintf(f, "</td></tr>");
    }
    if(c->func == func_id) {
      fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
              show_alt_set((alt_set_t)c->expr.arg[1]));
    }
  }
  fprintf(f, "</table>>\nshape = \"none\"\n];\n");

  /* print edges */
  if(is_cell(c->alt)) {
    cell_t *alt = clear_ptr(c->alt);
    fprintf(f, "node%" PRIuPTR ":alt -> node%" PRIuPTR ":top;\n",
            node, alt - cells);
    graph_cell(f, c->alt);
  }
  if(is_value(c)) {
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--) {
        if(is_cell(c->value.ptr[n])) {
          fprintf(f, "node%" PRIuPTR ":ptr%u -> node%" PRIuPTR ":top;\n",
                  node, (unsigned int)n, c->value.ptr[n] - cells);
          graph_cell(f, c->value.ptr[n]);
        }
      }
    }
  } else {
    for(csize_t i = 0; i < n; i++) {
      cell_t *arg = clear_ptr(c->expr.arg[i]);
      if(is_cell(arg)) {
        fprintf(f, "node%" PRIuPTR ":arg%d -> node%" PRIuPTR ":top%s;\n",
                c - cells, (unsigned int)i, arg - cells, is_weak(c, arg) ? " [color=lightgray]" : "");
        graph_cell(f, arg);
      }
    }
  }
}

void show_int(cell_t const *c) {
  assert(c && type_match(T_INT, c));
  int n = val_size(c);
  switch(n) {
  case 0: printf(" ()"); break;
  case 1: printf(" %" PRIdPTR, c->value.integer[0]); break;
  default:
    printf(" (");
    while(n--) printf(" %" PRIdPTR, c->value.integer[n]);
    printf(" )");
    break;
  }
}

void show_float(cell_t const *c) {
  assert(c && type_match(T_FLOAT, c));
  printf(" %g", c->value.flt[0]);
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

// must be at least 2
#define SHOW_LIST_LIMIT 16

void show_list(cell_t const *c) {
  assert(c && is_list(c));
  csize_t n = list_size(c), i;
  csize_t conflict = 0;
  if(n) {
    if(any_alt_overlap((cell_t const *const *)c->value.ptr, n)) {
      cell_t *p = 0, *free_this = 0;
      cell_t const *m1 = 0, *m2 = 0;

      /* find first match */
      if(!(conflict = any_conflicts((cell_t const *const *)c->value.ptr, n))) {
        m1 = c;
      } else {
        p = copy(c);
        while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, conflict, n)) {
          if(!(conflict = any_conflicts((cell_t const *const *)p->value.ptr, n))) {
            m1 = p;
            free_this = p;
            break;
          }
        }
      }
      if(!m1) {
        /* no matches */
        printf(" []");
        if(p) closure_free(p);
      } else {
        /* find second match */
        p = copy(m1);
        while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, conflict, n)) {
          if(!(conflict = any_conflicts((cell_t const *const *)p->value.ptr, n))) {
            m2 = p;
            break;
          }
        }
        if(m2) printf(" {");
        /* at least one match */
        printf(" [");
        i = n; while(i--) show_one(m1->value.ptr[i]);
        printf(" ]");
        closure_free(free_this);
        if(m2) {
          /* second match */
          printf(" | [");
          i = n; while(i--) show_one(m2->value.ptr[i]);
          printf(" ]");
          /* remaining matches */
          int limit = SHOW_LIST_LIMIT - 2;
          while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, conflict, n)) {
            if(!(conflict = any_conflicts((cell_t const *const *)p->value.ptr, n))) {
              if(!limit--) {
                printf(" | ...");
                break;
              } else {
                printf(" | [");
                i = n; while(i--) show_one(p->value.ptr[i]);
                printf(" ]");
              }
            }
          }
          printf(" }");
        }
        closure_free(p);
      }
    } else {
      printf(" [");
      i = n; while(i--) show_alt(c->value.ptr[i]);
      printf(" ]");
    }
  } else printf(" []");
}

int test_count() {
  cell_t test[] = {
    [0] = { .alt = &test[1] },
    [1] = { .alt = 0 },
    [2] = { .alt = 0 },
    [3] = { .alt = &test[4] },
    [4] = { .alt = &test[5] },
    [5] = { .alt = 0 }
  };
  cell_t *cnt[3];
  cell_t const *reset[3] = {&test[0], &test[2], &test[3]};
  memcpy(cnt, reset, sizeof(reset));
  int n = 0;
  FOREACH(i, reset) {
    printf("___ conflict = %d ___\n", (int)i);
    do {
      n++;
      printf("%d %d %d\n", (int)(cnt[0]-test), (int)(cnt[1]-test), (int)(cnt[2]-test));
    } while(count((const cell_t **)cnt, reset, i, 3));
  }
  return n == 16 ? 0 : -1;
}

void show_func(cell_t const *c) {
  int n = closure_args(c), i;
  char const *s = function_token(c->func);
  if(!s) return;
  if(is_placeholder(c)) printf(" ?%" PRIuPTR " =", c - cells);
  for(i = 0; i < n; ++i) {
    cell_t *arg = c->expr.arg[i];
    if(is_closure(arg)) {
      show_one(arg);
    }
  }
  if(c->func != func_id) { // to reduce noise and allow diff'ing test output
    printf(" %s", s);
  }
}

void show_var(cell_t const *c) {
  assert(is_var(c));
  if(is_list(c)) {
    show_list(c);
  } else {
    printf(" ?%c%" PRIuPTR, type_char(c->value.type), c - cells);
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
    val_t x = c->value.integer[0];
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

void show_alt(cell_t const *c) {
  cell_t const *p = c, *t;

  if(p) {
    if(!p->alt) {
      /* one */
      show_one(p);
    } else {
      /* many */
      printf(" {");
      t = p->alt;
      show_one(p);
      p = t;
      do {
        printf(" |");
        t = p->alt;
        show_one(p);
        p = t;
      } while(p);
      printf(" }");
    }
  } else {
    /* none */
    printf(" {}");
  }
}

char *show_type(type_t t) {
#define _case(x) case x: return #x
  switch(t.exclusive) {
  _case(T_ANY);
  _case(T_INT);
  _case(T_IO);
  _case(T_LIST);
  _case(T_SYMBOL);
  _case(T_MAP);
  _case(T_STRING);
  _case(T_RETURN);
  _case(T_FUNCTION);
  _case(T_BOTTOM);
  default: return "???";
  }
#undef case
}

// unsafe
char *show_type_all(type_t t) {
  const static char *type_flag_name[] = {
    "T_VAR",
    "T_FAIL",
    "T_TRACED",
  };
  static char buf[64];
  char *p = buf;
  p += sprintf(p, "%s", show_type(t));
  FOREACH(i, type_flag_name) {
    if(t.flags & (0x80 >> i)) {
      p += sprintf(p, "|%s", type_flag_name[i]);
    }
  }
  return buf;
}

char type_char(type_t t) {
  switch(t.exclusive) {
  case T_ANY: return 'a';
  case T_INT: return 'i';
  case T_IO: return 'w';
  case T_LIST: return 'l';
  case T_SYMBOL: return 's';
  case T_MAP: return 'm';
  case T_STRING: return 'S';
  case T_RETURN: return 'r';
  case T_FUNCTION: return 'f';
  case T_BOTTOM: return '0';
  }
  return 'x';
}

// unsafe
char *show_type_all_short(type_t t) {
  const static char type_flag_char[] = {
    '?',
    '!',
    '.',
  };
  static char buf[LENGTH(type_flag_char) + 1];

  char *p = buf;

  FOREACH(i, type_flag_char) {
    if(t.flags & (0x80 >> i)) {
      *p++ = type_flag_char[i];
    }
  }

  *p++ = type_char(t);
  *p = 0;
  return buf;
}

bool count(cell_t const **cnt, cell_t const *const *reset, csize_t conflict, csize_t size) {
  csize_t i= conflict;
  while(i && !cnt[i]->alt) i--; // find the nost significant with an alt
  for(; i < size; i++) {
    if(!cnt[i]->alt) {
      cnt[i] = reset[i];
    } else {
      cnt[i] = cnt[i]->alt;
      return true;
    }
  }
  return false;
}
