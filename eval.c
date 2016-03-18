/* Copyright 2012-2015 Dustin DeWeese
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
#include <time.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <getopt.h>
#include <inttypes.h>

#if defined(USE_READLINE)
#include <readline/readline.h>
#include <readline/history.h>
#elif defined(USE_LINENOISE)
#include "linenoise/linenoise.h"
#else
#define RAW_LINE
#endif

#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/byte_compile.h"

#ifdef USE_LLVM
#include "llvm.h"
#endif

// for when index declaration is missing for some reason e.g. clang on Android
char *index(const char *s, int c);

word_entry_t user_word_table[64] = {{"", NULL, 0, 0, NULL}};
unsigned int const user_word_table_length = LENGTH(user_word_table);
word_entry_t *new_user_word_entry = user_word_table;

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
  CASE(reduced);
  CASE(compose);
  CASE(pushl);
  CASE(pushr);
  CASE(quote);
  CASE(dep);
  CASE(popl);
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
  if(!is_reduced(c)) {
    fprintf(f, "%x (%u)</b></font></td></tr>", (unsigned int)c->out, (unsigned int)c->n);
  } else {
    fprintf(f, "%s (%u)</b></font></td></tr>", show_type_all_short(c->type), (unsigned int)c->n);
  }
  fprintf(f, "<tr><td port=\"alt\">alt: <font color=\"lightgray\">%p</font></td></tr>",
          (void *)c->alt);
  if(is_reduced(c)) {
    fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
            show_alt_set(c->alt_set));
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--)
        fprintf(f, "<tr><td port=\"ptr%u\">ptr: <font color=\"lightgray\">%p</font></td></tr>",
                (unsigned int)n, (void *)c->ptr[n]);
    } else if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else {
      int n = val_size(c);
      while(n--)
        fprintf(f, "<tr><td bgcolor=\"yellow\">val: %" PRIdPTR "</td></tr>", c->val[n]);
    }
  } else {
    for(csize_t i = 0; i < n; i++) {
      fprintf(f, "<tr><td port=\"arg%u\"><font color=\"lightgray\">%p</font></td></tr>", (unsigned int)i, (void *)c->arg[i]);
    }
    if(c->func == func_id) {
      fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
              show_alt_set((alt_set_t)c->arg[1]));
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
  if(is_reduced(c)) {
    if(is_list(c)) {
      csize_t n = list_size(c);
      while(n--) {
        if(is_cell(c->ptr[n])) {
          fprintf(f, "node%" PRIuPTR ":ptr%u -> node%" PRIuPTR ":top;\n",
                  node, (unsigned int)n, c->ptr[n] - cells);
          graph_cell(f, c->ptr[n]);
        }
      }
    }
  } else {
    for(csize_t i = 0; i < n; i++) {
      cell_t *arg = clear_ptr(c->arg[i]);
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
  case 1: printf(" %" PRIdPTR, c->val[0]); break;
  default:
    printf(" (");
    while(n--) printf(" %" PRIdPTR, c->val[n]);
    printf(" )");
    break;
  }
}

bool reduce_list(cell_t *c) {
  bool b = true;
  csize_t n = list_size(c);
  cell_t **p = c->ptr;
  while(n--) {
    *p = reduce_alt(*p);
    b &= *p != 0;
    if(*p == 0) *p = &fail_cell;
    ++p;
  }
  return b;
}

bool any_alt_overlap(cell_t const *const *p, csize_t size) {
  uintptr_t t, as = 0;
  for(csize_t i = 0; i < size; ++i) {
    if((t = p[i]->alt_set) & as) return true;
    as |= t;
  }
  return false;
}

bool any_conflicts(cell_t const *const *p, csize_t size) {
  uintptr_t t, as = 0;
  for(csize_t i = 0; i < size; ++i) {
    if(is_reduced(p[i])) {
      if(as_conflict(as, t = p[i]->alt_set)) return true;
      as |= t;
    }
  }
  return false;
}

void show_list(cell_t const *c) {
  assert(c && is_list(c));
  int n = list_size(c), i;
  if(n) {
    if(any_alt_overlap((cell_t const *const *)c->ptr, n)) {
      cell_t *p = 0, *free_this = 0;
      cell_t const *m1 = 0, *m2 = 0;

      /* find first match */
      if(!any_conflicts((cell_t const *const *)c->ptr, n)) {
        m1 = c;
      } else {
        p = copy(c);
        while(count((cell_t const **)p->ptr, (cell_t const *const *)c->ptr, n) >= 0) {
          if(!any_conflicts((cell_t const *const *)p->ptr, n)) {
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
        while(count((cell_t const **)p->ptr, (cell_t const *const *)c->ptr, n) >= 0) {
          if(!any_conflicts((cell_t const *const *)p->ptr, n)) {
            m2 = p;
            break;
          }
        }
        if(m2) printf(" {");
        /* at least one match */
        printf(" [");
        for(i = 0; i < n; i++) show_one(m1->ptr[i]);
        printf(" ]");
        closure_free(free_this);
        if(m2) {
          /* second match */
          printf(" | [");
          for(i = 0; i < n; i++) show_one(m2->ptr[i]);
          printf(" ]");
          /* remaining matches */
          while(count((cell_t const **)p->ptr, (cell_t const *const *)c->ptr, n) >= 0) {
            if(!any_conflicts((cell_t const *const *)p->ptr, n)) {
              printf(" | [");
              show_one(p->ptr[i]);
              printf(" ]");
            }
          }
          printf(" }");
        }
        closure_free(p);
      }
    } else {
      printf(" [");
      for(i = 0; i < n; i++) show_alt(c->ptr[i]);
      printf(" ]");
    }
  } else printf(" []");
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
static TEST(test_count);

void show_func(cell_t const *c) {
  int n = closure_args(c), i;
  char const *s = function_token(c->func);
  if(!s) return;
  if(is_placeholder(c)) printf(" ?%" PRIuPTR " =", c - cells);
  for(i = 0; i < n; ++i) {
    cell_t *arg = c->arg[i];
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
    printf(" ?%c%" PRIuPTR, type_char(c->type), c - cells);
  }
}

void show_one(cell_t const *c) {
  if(!c) {
    printf(" []");
  } else if(!is_closure(c)) {
    printf(" ?");
  } else if(!is_reduced(c)) {
    show_func(c);
  } else if(is_fail(c)) {
    printf(" {}");
  } else if(is_var(c)) {
    show_var(c);
  } else if(type_match(T_INT, c)) {
    show_int(c);
  } else if(type_match(T_LIST, c)) {
    show_list(c);
  } else {
    printf(" ?");
  }
}

bool reduce_one(cell_t **cp) {
  if(!closure_is_ready(*cp)) return true;
  bool b = reduce(cp, T_ANY);
  //if(is_list(*cp)) reduce_list(*cp); // *** hack to force things that might be needed later
  return b;
}

cell_t *reduce_alt(cell_t *c) {
  cell_t *r, *p = c;
  cell_t **q = &r;
  while(p && reduce_one(&p)) {
    *q = p;
    q = &p->alt;
    p = p->alt;
  }
  *q = 0;
  return r;
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

void measure_start() {
  memset(&measure, 0, sizeof(measure));
  measure.start = clock();
}

void measure_stop() {
  memcpy(&saved_measure, &measure, sizeof(measure));
  saved_measure.stop = clock();
  saved_measure.alt_cnt = alt_cnt;
}

void measure_display() {
  double time = (saved_measure.stop - saved_measure.start) /
    (double)CLOCKS_PER_SEC;
  printf("time        : %.3e sec\n"
         "allocated   : %d cells\n"
         "working set : %d cells\n"
         "reductions  : %d\n"
         "rate        : %.3e reductions/sec\n"
         "alts used   : %d\n",
         time,
         saved_measure.alloc_cnt,
         saved_measure.max_alloc_cnt,
         saved_measure.reduce_cnt,
         saved_measure.reduce_cnt / time,
         saved_measure.alt_cnt);

}

void usage() {
  printf("usage: eval [-t <test name>]\n");
}

#ifndef EMSCRIPTEN
int main(UNUSED int argc, UNUSED char *argv[]) {
  int ch;

  while ((ch = getopt(argc, argv, "t:l:r:")) != -1) {
    switch (ch) {
    case 't':
      cells_init();
      return test_run(optarg, test_log);
      break;
    case 'l':
    case 'r':
      load_source(optarg);
      if(ch == 'r') return 0;
      break;
    case '?':
    default:
      usage();
      return 0;
      break;
    }
  }
  argc -= optind;
  argv += optind;

  run_eval();
  measure_display();
  return 0;
}
#endif

#ifdef USE_LINENOISE
static void completion(char const *buf, linenoiseCompletions *lc) {
  /* FIXME
  unsigned int n = strlen(buf);
  char comp[n+sizeof_field(word_entry_t, name)];
  char *insert = comp + n;
  strncpy(comp, buf, sizeof(comp));
  char *tok = rtok(comp, insert);
  unsigned int tok_len = strnlen(tok, sizeof_field(word_entry_t, name));
  if(!tok) return;
  word_entry_t *e = lookup_word(tok);
  if(e) {
    // add completions
    do {
      if(strnlen(e->name, sizeof_field(word_entry_t, name)) >
         tok_len) {

        strncpy(tok, e->name, sizeof(e->name));
        linenoiseAddCompletion(lc, comp);
      }
      e++;
    } while(strncmp(e->name, tok, tok_len) == 0);
  }
  */
}
#endif

#ifdef USE_READLINE
static char **completion(char *buf, UNUSED int start, UNUSED int end)
{
    unsigned int current_match = 1;
    char **matches = NULL;
/* FIXME
    unsigned int n = strlen(buf);
    char *comp = malloc(n + sizeof_field(word_entry_t, name) + 1);
    memcpy(comp, buf, n);
    char *insert = comp + n;
    *insert = 0;
    char *tok = rtok(comp, insert);
    if(tok) {
        word_entry_t *e = lookup_word(tok);
        if(e) {
            matches = malloc(sizeof(char *) * 16);
            memset(matches, 0, sizeof(char *) * 16);

            unsigned int tok_len = strnlen(tok, sizeof_field(word_entry_t, name));
            char *copy = malloc(sizeof_field(word_entry_t, name));
            memcpy(copy, tok, tok_len+1);
            matches[0] = copy;

            // add completions
            do {
                unsigned int entry_len = strnlen(e->name, sizeof_field(word_entry_t, name));
                if(entry_len > tok_len) {
                    memcpy(tok, e->name, entry_len);
                    comp[entry_len] = 0;

                    char *copy = malloc(sizeof_field(word_entry_t, name));
                    memcpy(copy, comp, entry_len+1);
                    matches[current_match++] = copy;
                }
                e++;
            } while(strncmp(e->name, tok, tok_len) == 0 && current_match < 16);

            // if there is just one match, make it the substitute
            if(current_match == 2) {
                strncpy(matches[0], matches[1], sizeof_field(word_entry_t, name));
            }
        }
    }

    free(comp);
*/
    return matches;
}

static void initialize_readline()
{
  rl_readline_name = "Poprc";
#if defined(__clang__)
  rl_attempted_completion_function = (CPPFunction *)completion;
#else
  rl_attempted_completion_function = (rl_completion_func_t *)completion;
#endif
}

#endif

#define HISTORY_FILE ".poprc_history"
#define GRAPH_FILE "cells.dot"
#define REDUCED_GRAPH_FILE "reduced.dot"
bool write_graph = false;

void run_eval() {
  char *line_raw, *line;
#ifdef USE_LINENOISE
  linenoiseSetCompletionCallback(completion);
  linenoiseHistoryLoad(HISTORY_FILE);
  while((line_raw = linenoise(": ")))
#elif USE_READLINE
  initialize_readline();
  while((line_raw = readline(": ")))
#else
  char buf[1024];
  while(printf(": "),
        (line_raw = fgets(buf, sizeof(buf), stdin)))
#endif
  {
#ifdef RAW_LINE
    char *p = line_raw;
    while(*p && *p != '\n') ++p;
    *p = 0;
#endif
    if(line_raw[0] == '\0') {
#ifndef RAW_LINE
      free(line_raw);
#endif
      continue;
    }
    line = line_raw;

#if defined(USE_LINENOISE)
    linenoiseHistoryAdd(line);
    linenoiseHistorySave(HISTORY_FILE);
#elif defined(USE_READLINE)
    add_history(line);
#endif

    bool run = eval_command(line);

#ifndef RAW_LINE
    free(line_raw);
#endif
    if(!run) break;
  }
}

bool eval_command(char *line) {
    while(*line == ' ') ++line;
    if(strcmp(line, ":m") == 0) {
      measure_display();
    } else if(strcmp(line, ":g") == 0) {
      write_graph = !write_graph;
      printf("graph %s\n", write_graph ? "ON" : "OFF");
    } else if(strncmp(line, ":l ", 3) == 0) {
      if(line[3])
        load_source(&line[3]);
    } else if(strcmp(line, ":q") == 0) {
      return false;
#ifndef EMSCRIPTEN
    } else if(strncmp(line, ":t ", 3) == 0) {
      cells_init();
      line += 3;
      while(*line == ' ') ++line;
      char *name = line;
      test_run(name, test_log);
#endif
    } else if(strncmp(line, ":C ", 3) == 0) {
#ifdef USE_LLVM
      cells_init();
      line += 3;
      while(*line == ' ') ++line;
      char *name = line;
      while(*line != ' ') ++line;
      *line++ = 0;
      compile_expr(name, line, strlen(line));
#else
      printf("Compilation is not supported.\n");
#endif
    } else if(strncmp(line, ":c ", 3) == 0) {
      cells_init();
      line += 3;
      while(*line == ' ') ++line;
      char *name = line;
      while(*line != ' ') ++line;
      *line++ = 0;
      compact_expr(name, line, strlen(line));
    } else if(strncmp(line, ":a ", 3) == 0) {
      csize_t in, out;
      cells_init();
      line += 3;
      if(get_arity(line, strlen(line), &in, &out)) {
        printf("%d -> %d\n", in, out);
      }
    } else {
      cells_init();
      measure_start();
      eval(line, strlen(line));
      measure_stop();
      check_free();
    }
    return true;
}

bool is_num(char const *str) {
  return char_class(str[0]) == CC_NUMERIC ||
    (str[0] == '-' && char_class(str[1]) == CC_NUMERIC);
}

word_entry_t *lookup_word(seg_t w) {
  word_entry_t *res =
    lookup(word_table,
           WIDTH(word_table),
           word_table_length,
           w);
  if(!res) res =
    lookup_linear(user_word_table,
                  WIDTH(user_word_table),
                  user_word_table_length,
                  w);
  return res;
}
/*
cell_t *word(char const *w) {
  csize_t in, out;
  return word_parse(w, &in, &out);
}
*/
cell_t *word_parse(seg_t w,
                   csize_t *in,
                   csize_t *out,
                   cell_t **data) {
  cell_t *c;
  *data = NULL;
  if(is_num(w.s)) {
    c = val(atoi(w.s));
    *in = 0;
    *out = 1;
  } else if(w.s[0] == '?') {
    c = var(T_ANY);
    *in = 0;
    *out = 1;
  } else if(w.n == 4 &&
            w.s[0] == 'a' && w.s[1] == 'p' &&
            char_class(w.s[2]) == CC_NUMERIC &&
            char_class(w.s[3]) == CC_NUMERIC) {
    *in = w.s[2] - '0' + 1;
    *out = w.s[3] - '0' + 1;
    c = func(func_ap, *in, *out);
  } else {
    word_entry_t *e = lookup_word(w);
    /* disallow partial matches */
    if(!e ||
       w.n != strnlen(e->name, sizeof_field(word_entry_t, name))) {
      // trace the name ***
      c = func(func_placeholder, 0, 1);
      *in = 0;
      *out = 1;
    } else {
      if(e->data) {
        c = func(e->func, e->in + 1, e->out);
        *data = e->data;
      } else {
        c = func(e->func, e->in, e->out);
      }
      *in = e->in;
      *out = e->out;
    }
  }
  return c;
}

char_class_t char_class(char c) {
  if(c <= ' ' || c > '~')
    return CC_NONE;
  if(c >= '0' && c <= '9')
    return CC_NUMERIC;
  if((c >= 'a' && c <= 'z') ||
     (c >= 'A' && c <= 'Z'))
    return CC_ALPHA;
  if(c == '?') return CC_VAR;
  if(c == '_') return CC_COMMENT;
  if(index("[](){}", c))
    return CC_BRACKET;
  return CC_SYMBOL;
}

// starts at comment
char *skip_comment(char *s) {
  int level = 0;
  char_class_t before = CC_NONE;
  char *ptr = s;

  for(;;) {
    // move cursor past comment character, and record character class after it
    ptr++;
    char_class_t after;
    unsigned int length = 1;
    while((after = char_class(*ptr)) == CC_COMMENT) {
      length++;
      ptr++;
    }

    if(before == CC_NONE) {
      if(after != CC_NONE) {
        level++;
      } else if(level == 0) {
        if(length > 1) {
          // line comment
          while(*ptr && *ptr != '\n') ptr++;
          return ptr;
        } else {
          // just a lone comment char
          return s;
        }
      }
    } else if(after == CC_NONE) {
      level--;
      if(!*ptr || level <= 0) return ptr;
    }

    // move cursor to next comment character, tracking character class before it
    ptr++;
    char_class_t cur = after;
    do {
      before = cur;
      ptr++;
      if(!*ptr) return ptr;
      cur = char_class(*ptr);
    } while(cur != CC_COMMENT);
  }
}

void mark_comments(char c, char *str) {
  char *ptr = str;
  while(*ptr) {
    char_class_t cc = char_class(*ptr);
    switch(cc) {
    case CC_NONE:
      ptr++;
      break;
    case CC_COMMENT: {
      char *start = ptr;
      ptr = skip_comment(ptr);
      if(ptr == start) {
        ptr++;
      } else {
        memset(start, c, ptr - start);
      }
      break;
    }
    default:
      while(char_class(*++ptr) != CC_NONE);
    }
  }
}

int test_comments(UNUSED char *name) {
  char str[] =
    "[1] :one def\n"
    "[2] :t_w_o def\n"
    "{ _an inline _n_e_s_t_ed_ comment_\n"
    "  [one t_w_o +] :three def\n"
    "  three *\n"
    "} :m def __ a line comment\n"
    "__ stack is: 6\n"
    "m:three\n"
    "__ stack is: 6 3\n";
  mark_comments('#', str);
  printf("%s", str);
  return 0;
}
static TEST(test_comments);

char *seg_end(seg_t seg) {
  return seg.s ? seg.s + seg.n : NULL;
}

seg_t tok(char *s) {
  seg_t seg = {NULL, 0};
  char_class_t cc = char_class(*s);

  /* skip spaces & comments */
  while(cc == CC_NONE || cc == CC_COMMENT) {
    if(!*s) {
      return seg;
    } else if(cc == CC_COMMENT) {
      char *n = skip_comment(s);
      if(s == n) break;
      s = n;
    } else {
      s++;
    }
    cc = char_class(*s);
  }

  /* at start of token */
  seg.s = s;

  /* allow adjacent brackets to be separately tokenized */
  if(cc == CC_BRACKET ||
     cc == CC_COMMENT) {
    seg.n = 1;
    return seg;
  }

  while(*++s) {
    char_class_t ncc = char_class(*s);
    if(cc == ncc) continue;

    // exceptions
    switch(ncc) {
    case CC_NUMERIC: // negative numbers
      if(s[-1] == '-') {
        cc = ncc;
        continue;
      } else break;
    case CC_COMMENT: // comment char inside token
      if(cc == CC_ALPHA ||
         cc == CC_SYMBOL) {
        continue;
      } else break;
    default:
      break;
    }
    break;
  }
  seg.n = s - seg.s;
  return seg;
}

cell_t *parse_vector(char **s) {
  seg_t t;
  cell_t *c = vector(0);
  while((t = tok(*s), t.s) &&
        *t.s != ')') {
    assert(is_num(t.s));
    c = pushr_val(atoi(t.s), c);
    *s = seg_end(t);
  }
  *s = seg_end(t);
  return c;
}

bool parse_word(char **s, cell_t **r) {
  csize_t in = 0, out = 1;
  cell_t *data = NULL;
  seg_t t = tok(*s);
  *s = seg_end(t);
  if(!t.s || *t.s == ']') return false;
  cell_t *c =
    *t.s == '[' ? _build(s) :
    *t.s == '(' ? parse_vector(s) :
    word_parse(t, &in, &out, &data);
  if(c) {
    *r = compose_expand(c, out, *r);
    if(data) arg(&c, data); // arg will not return a new pointer
  }
  return true;
}

void argf_noop(UNUSED cell_t *c, UNUSED val_t i) {}
val_t fill_args(cell_t *r, void (*argf)(cell_t *, val_t)) {
  if(!argf) argf = argf_noop;
  csize_t n = list_size(r);
  if(n < 1) return 0;
  cell_t **l = &r->ptr[n-1];
  val_t i = 0;
  close_placeholders(*l);
  while(!closure_is_ready(*l)) {
    cell_t *v = var(T_ANY);
    v->val[0] = i;
    arg_noexpand(l, v);
    argf(v, i);
    ++i;
  }
  return i;
}

cell_t *build(char *s) {
  return _build(&s);
}

cell_t *_build(char **s) {
  cell_t *r = empty_list();
  while((parse_word(s, &r)));
  return r;
}

void reduce_root(cell_t *c) {
  if(write_graph) make_graph_all(GRAPH_FILE);
  reduce_list(c);
  if(write_graph) make_graph_all(REDUCED_GRAPH_FILE);
}

void eval(char *str, unsigned int n) {
  cell_t *c = build(str);
  reduce_root(c);
  if(!c) return;
  csize_t s = list_size(c);
  if(s > 0 && !closure_is_ready(c->ptr[s-1])) {
    printf("incomplete expression\n");
  } else {
    c = remove_row(c);
    show_list(c);
    printf("\n");
  }
  drop(c);
}

bool get_arity(char *str, unsigned int n, csize_t *in, csize_t *out) {
  set_trace(NULL);
  cell_t *c = build(str);
  if(!c) return false;
  *in = fill_args(c, NULL);
  if(!closure_is_ready(c)) {
    printf("incomplete expression\n");
    return false;
  } else {
    c = remove_row(c);
    *out = max(1, list_size(c));
    drop(c);
    return true;
  }
}

#ifdef USE_LLVM
void compile_expr(char const *name, char *str, unsigned int n) {
  word_entry_t *e =
    lookup_linear(user_word_table,
                  WIDTH(user_word_table),
                  user_word_table_length,
                  name);
  if(!e) e = new_user_word_entry++;
  strcpy(e->name, name);
  e->func = func_placeholder;
  e->in = 0;
  e->out = 1;
  char *s = malloc(n+1);
  memcpy(s, str, n+1);
  get_arity(s, n, &e->in, &e->out);
  free(s);
  e->func = func_self;
  cell_t *c = build(str, n);
  if(!c) {
    --new_user_word_entry;
    return;
  }
  e->func = compile(c, e->in, e->out);
}
#endif

cell_t *remove_row(cell_t *c) {
  assert(is_list(c));
  csize_t n = list_size(c);
  if(n == 0 || !(c->ptr[n-1]->type & T_ROW)) return c;
  return remove_left(c);
}

cell_t *remove_left(cell_t *c) {
  assert(is_list(c));
  assert(list_size(c) > 0);
  csize_t size = calculate_cells(c->size - 1);
  cell_t *new = closure_alloc_cells(size);
  memcpy(new, c, sizeof(cell_t) * size);
  --new->size;
  traverse_ref(new, PTRS | ALT);
  drop(c);
  return new;
}

void load_source(char *path) {
  char buf[1024];
  char *line = 0;
  FILE *f = fopen(path, "r");
  if(!f) {
    printf("could not open: %s\n", path);
    return;
  }
  printf("loading: %s\n", path);
  while((line = fgets(buf, sizeof(buf), f))) {
    if(line[0] == '@')
      printf("%s", line);
    else if(line[0] == ':') {
      printf("%s", line);
      cells_init();
      eval(line+1, strlen(line));
      check_free();
    } else if(line[0] == '=') {
      printf("%s", line);
      ++line;
      while(*line == ' ') ++line;
      char *name = line;
      while(*line != ' ') ++line;
      *line++ = 0;
      char *x = line;
      while(*x && *x != '\n') ++x;
      *x = 0;
      cells_init();
      compact_expr(name, line, strlen(line));
    }
  }
  fclose(f);
  printf("loaded: %s\n", path);
}

char *show_type(type_t t) {
#define _case(x) case x: return #x
  switch(t & T_EXCLUSIVE) {
  _case(T_ANY);
  _case(T_INT);
  _case(T_IO);
  _case(T_LIST);
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
