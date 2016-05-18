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
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <getopt.h>

#if defined(USE_READLINE)
#include <readline/readline.h>
#include <readline/history.h>
#elif defined(USE_LINENOISE)
#include "linenoise/linenoise.h"
#else
#define RAW_LINE
#endif

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/special.h"
#include "gen/eval.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/byte_compile.h"
#include "gen/parse.h"
#include "gen/print.h"

#ifdef USE_LLVM
#include "llvm.h"
#endif

bool reduce_list(cell_t *c) {
  bool b = true;
  csize_t n = list_size(c);
  cell_t **p = c->value.ptr;
  while(n--) {
    *p = reduce_alt(*p);
    b &= *p != 0;
    if(*p == 0) *p = &fail_cell;
    ++p;
  }
  return b;
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
      return run_test(optarg, test_log);
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

#if defined(USE_LINENOISE) || defined(USE_READLINE)
static seg_t last_tok(const char *str) {
  seg_t last, n;
  while(n = tok(str), n.s) {
    last = n;
    str = seg_end(n);
  }
  return last;
}
#endif

#ifdef USE_LINENOISE
static void completion(char const *buf, linenoiseCompletions *lc) {
  unsigned int n = strlen(buf);
  char comp[n+sizeof_field(word_entry_t, name)];
  strncpy(comp, buf, sizeof(comp));
  seg_t t = last_tok(comp);
  if(!t.s) return;
  word_entry_t *e = lookup_word(t);
  if(e) {
    // add completions
    do {
      if(strnlen(e->name, sizeof_field(word_entry_t, name)) >
         t.n) {

        strncpy((char *)t.s, e->name, sizeof(e->name));
        linenoiseAddCompletion(lc, comp);
      }
      e++;
    } while(strncmp(e->name, t.s, t.n) == 0);
  }
}
#endif

#ifdef USE_READLINE
static char **completion(char *buf, UNUSED int start, UNUSED int end)
{
    unsigned int current_match = 1;
    char **matches = NULL;
    unsigned int n = strlen(buf);
    char *comp = malloc(n + sizeof_field(word_entry_t, name) + 1);
    memcpy(comp, buf, n);
    seg_t t = last_tok(comp);
    if(t.s) {
        word_entry_t *e = lookup_word(t);
        if(e) {
            matches = malloc(sizeof(char *) * 16);
            memset(matches, 0, sizeof(char *) * 16);

            char *copy = malloc(sizeof_field(word_entry_t, name));
            memcpy(copy, t.s, t.n+1);
            matches[0] = copy;

            // add completions
            do {
                unsigned int entry_len = strnlen(e->name, sizeof_field(word_entry_t, name));
                if(entry_len > t.n) {
                  memcpy((char *)t.s, e->name, entry_len);
                    comp[entry_len] = 0;

                    char *copy = malloc(sizeof_field(word_entry_t, name));
                    memcpy(copy, comp, entry_len+1);
                    matches[current_match++] = copy;
                }
                e++;
            } while(strncmp(e->name, t.s, t.n) == 0 && current_match < 16);

            // if there is just one match, make it the substitute
            if(current_match == 2) {
                strncpy(matches[0], matches[1], sizeof_field(word_entry_t, name));
            }
        }
    }

    free(comp);
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
      run_test(name, test_log);
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
      compact_expr(name, line);
    } else if(strncmp(line, ":a ", 3) == 0) {
      csize_t in, out;
      cells_init();
      line += 3;
      if(get_arity(line, &in, &out)) {
        printf("%d -> %d\n", in, out);
      }
    } else {
      cells_init();
      measure_start();
      eval(line);
      measure_stop();
      check_free();
    }
    return true;
}

void reduce_root(cell_t *c) {
  if(write_graph) make_graph_all(GRAPH_FILE);
  reduce_list(c);
  if(write_graph) make_graph_all(REDUCED_GRAPH_FILE);
}

void eval(const char *str) {
  cell_t *c = build(str);
  reduce_root(c);
  if(!c) return;
  csize_t s = list_size(c);
  if(s > 0 && !closure_is_ready(c->value.ptr[s-1])) {
    printf("incomplete expression\n");
  } else {
    c = remove_row(c);
    show_list(c);
    printf("\n");
  }
  drop(c);
}

bool get_arity(char *str, csize_t *in, csize_t *out) {
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
  if(n == 0 || !(c->value.ptr[n-1]->value.type & T_ROW)) return c;
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
      eval(line+1);
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
      compact_expr(name, line);
    }
  }
  fclose(f);
  printf("loaded: %s\n", path);
}
