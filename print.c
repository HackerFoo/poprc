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
  uintptr_t set_mask = 1l << (sizeof(as) * 8 - 1);
  uintptr_t val_mask = 1l << (sizeof(as) * 4 - 1);
  while(!(as & set_mask) && n) {
    as <<= 1;
    n--;
  }
  while(n--) {
    *p++ = as & set_mask ? (as & val_mask ? '1' : '0') : 'X';
    as <<= 1;
  }
  *p++ = '\0';
  return out;
}

char const *function_name(reduce_t *f) {
  f = (reduce_t *)clear_ptr(f);
  //  int i;
# define CASE(n) if(f == func_##n) return #n
  CASE(add);
  CASE(sub);
  CASE(mul);
  CASE(value);
  CASE(compose);
  CASE(pushl);
  CASE(pushr);
  CASE(quote);
  CASE(dep);
  CASE(popr);
  CASE(alt);
  CASE(assert);
  CASE(id);
  //  CASE(collect);
  CASE(gt);
  CASE(gte);
  CASE(lt);
  CASE(lte);
  CASE(eq);
  CASE(neq);
  CASE(dup);
  CASE(swap);
  CASE(drop);
  CASE(cut);
  CASE(alt2);
  //CASE(fib);
  CASE(select);
  CASE(placeholder);
  CASE(self);
  CASE(exec);
  CASE(ap);
  CASE(print);
  return "?";
# undef CASE
}

char const *function_token(reduce_t *f) {
  f = (reduce_t *)clear_ptr(f);
  for(unsigned int i = 0; i < word_table_length; ++i) {
    if(word_table[i].func == f)
      return word_table[i].name;
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

  /* print node attributes */
  fprintf(f, "node%" PRIuPTR " [\nlabel =<", node);
  fprintf(f, "<table border=\"%d\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>(%" PRIuPTR ") %s%s %x ",
          border,
          node,
          function_name(c->func),
          closure_is_ready(c) ? "" : "*",
          (int)c->size);
  if(!is_value(c)) {
    fprintf(f, "%x (%u)</b></font></td></tr>", (unsigned int)c->expr.out, (unsigned int)c->n);
  } else {
    fprintf(f, "%s (%u)</b></font></td></tr>", show_type_all_short(c->value.type), (unsigned int)c->n);
  }
  fprintf(f, "<tr><td port=\"alt\">alt: <font color=\"lightgray\">%p</font></td></tr>",
          (void *)c->alt);
  if(is_value(c)) {
    fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
            show_alt_set(c->value.alt_set));
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--)
        fprintf(f, "<tr><td port=\"ptr%u\">ptr: <font color=\"lightgray\">%p</font></td></tr>",
                (unsigned int)n, (void *)c->value.ptr[n]);
    } else if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else {
      int n = val_size(c);
      while(n--)
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %" PRIdPTR "</td></tr>", c->value.integer[n]);
    }
  } else {
    for(csize_t i = 0; i < n; i++) {
      fprintf(f, "<tr><td port=\"arg%u\"><font color=\"lightgray\">%p</font></td></tr>", (unsigned int)i, (void *)c->expr.arg[i]);
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

bool any_alt_overlap(cell_t const *const *p, csize_t size) {
  uintptr_t t, as = 0;
  for(csize_t i = 0; i < size; ++i) {
    if((t = p[i]->value.alt_set) & as) return true;
    as |= t;
  }
  return false;
}

bool any_conflicts(cell_t const *const *p, csize_t size) {
  uintptr_t t, as = 0;
  for(csize_t i = 0; i < size; ++i) {
    if(is_value(p[i])) {
      if(as_conflict(as, t = p[i]->value.alt_set)) return true;
      as |= t;
    }
  }
  return false;
}

void show_list(cell_t const *c) {
  assert(c && is_list(c));
  int n = list_size(c), i;
  if(n) {
    if(any_alt_overlap((cell_t const *const *)c->value.ptr, n)) {
      cell_t *p = 0, *free_this = 0;
      cell_t const *m1 = 0, *m2 = 0;

      /* find first match */
      if(!any_conflicts((cell_t const *const *)c->value.ptr, n)) {
        m1 = c;
      } else {
        p = copy(c);
        while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, n) >= 0) {
          if(!any_conflicts((cell_t const *const *)p->value.ptr, n)) {
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
        while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, n) >= 0) {
          if(!any_conflicts((cell_t const *const *)p->value.ptr, n)) {
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
          while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, n) >= 0) {
            if(!any_conflicts((cell_t const *const *)p->value.ptr, n)) {
              printf(" | [");
              i = n; while(i--) show_one(p->value.ptr[i]);
              printf(" ]");
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

int test_count(UNUSED char *name) {
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
  do {
    n++;
    printf("%d %d %d\n", (int)(cnt[0]-test), (int)(cnt[1]-test), (int)(cnt[2]-test));
  } while(count((const cell_t **)cnt, reset, 3) >= 0);
  return n == 6 ? 0 : -1;
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
  printf(" %s", s);
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
  } else if(type_match(T_LIST, c)) {
    show_list(c);
  } else if(type_match(T_SYMBOL, c)) {
    val_t x = c->value.integer[0];
    char *str = symbol_string(x);
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
  switch(t & T_EXCLUSIVE) {
  _case(T_ANY);
  _case(T_INT);
  _case(T_IO);
  _case(T_LIST);
  _case(T_SYMBOL);
  default: return "???";
  }
#undef case
}

// unsafe
char *show_type_all(type_t t) {
  const static char *type_flag_name[] = {
    "T_VAR",
    "T_ROW",
    "T_INDIRECT",
    "T_FAIL",
    "T_TRACED"
  };
  static char buf[64];
  char *p = buf;
  p += sprintf(p, "%s", show_type(t));
  FOREACH(i, type_flag_name) {
    if(t & (0x8000 >> i)) {
      p += sprintf(p, "|%s", type_flag_name[i]);
    }
  }
  return buf;
}

char type_char(type_t t) {
  switch(t & T_EXCLUSIVE) {
  case T_ANY: return 'a';
  case T_INT: return 'i';
  case T_IO: return 'w';
  case T_LIST: return 'l';
  case T_SYMBOL: return 's';
  }
  return 'x';
}

// unsafe
char *show_type_all_short(type_t t) {
  const static char type_flag_char[] = {
    '?',
    '@',
    '*',
    '!',
    '.'
  };
  static char buf[] = "XXXXXX";

  char *p = buf;

  FOREACH(i, type_flag_char) {
    if(t & (0x8000 >> i)) {
      *p++ = type_flag_char[i];
    }
  }

  *p++ = type_char(t);
  *p = 0;
  return buf;
}

int count(cell_t const **cnt, cell_t const *const *reset, int size) {
  int i = size;
  while(i--) {
    if(!cnt[i]->alt) {
      cnt[i] = reset[i];
    } else {
      cnt[i] = cnt[i]->alt;
      break;
    }
  }
  return i;
}

