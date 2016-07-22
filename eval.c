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
#include <stdlib.h>
#include <unistd.h>

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

#ifdef EMSCRIPTEN
#include <emscripten.h>
#endif

bool tty = false;

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
         "failures    : %d\n"
         "rate        : %.3e reductions/sec\n"
         "alts used   : %d\n",
         time,
         saved_measure.alloc_cnt,
         saved_measure.max_alloc_cnt,
         saved_measure.reduce_cnt,
         saved_measure.fail_cnt,
         saved_measure.reduce_cnt / time,
         saved_measure.alt_cnt);

}

void usage() {
  printf("usage: eval [-t <test name>]\n");
}

int main(UNUSED int argc, UNUSED char *argv[]) {
  parse_init();
  cells_init();

#ifndef EMSCRIPTEN
  int ch;
  bool echo = false;
  bool stats = true;

  tty = isatty(fileno(stdin));

  while ((ch = getopt(argc, argv, "eSt:l:r:L:")) != -1) {
    switch (ch) {
    case 'e':
      echo = true;
      break;
    case 'S':
      stats = false;
      break;
    case 't':
      return run_test(optarg);
      break;
    case 'l':
    case 'r':
      load_file(optarg);
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

  run_eval(echo);
  if(stats) {
    measure_display();
    print_symbols();
  }
  free_modules();
  unload_files();
  check_free();
#else
  emscripten_exit_with_live_runtime();
#endif
  return 0;
}

#if defined(USE_LINENOISE) || defined(USE_READLINE)
/*
static seg_t last_tok(const char *str, const char *e) {
  seg_t last, n;
  while(n = tok(str, e), n.s) {
    last = n;
    str = seg_end(n);
  }
  return last;
}
*/
#endif

#ifdef USE_LINENOISE
/*
static void completion(char const *buf, linenoiseCompletions *lc) {
  unsigned int n = strlen(buf);
  seg_t t = last_tok(buf, buf + n);
  if(!t.s) return;
  word_entry_t *e = lookup_word(t);
  if(e) {
    // add completions
    char *comp = malloc(n + 64);
    strcpy(comp, buf);
    do {
      unsigned int entry_len = strlen(e->name);
      int diff = entry_len - t.n;
      if(diff > 0) {
        comp = realloc(comp, n + diff + 1);
        strcpy(comp + n, e->name + (entry_len - diff));
        linenoiseAddCompletion(lc, comp);
      }
      e++; // FIXME
    } while(strncmp(e->name, t.s, t.n) == 0);
    free(comp);
  }
}
*/
#endif

#ifdef USE_READLINE
/*
static char **completion(char *buf, UNUSED int start, UNUSED int end)
{
  unsigned int current_match = 1;
  char **matches = NULL;
  unsigned int n = strlen(buf);
  seg_t t = last_tok(buf, buf + n);
  if(t.s) {
    word_entry_t *e = lookup_word(t);
    if(e) {
      matches = malloc(sizeof(char *) * 16);
      memset(matches, 0, sizeof(char *) * 16);

      char *copy = malloc(t.n + 1);
      seg_read(t, copy, t.n + 1);
      matches[0] = copy;

      // add completions
      do {
        unsigned int entry_len = strlen(e->name);
        int diff = entry_len - t.n;
        if(diff > 0) {
          char *comp = malloc(n + diff + 1);
          memcpy(comp, buf, n);
          strcpy(comp + n, e->name + (entry_len - diff));
          matches[current_match++] = comp;
        }
        e++; // FIXME
      } while(strncmp(e->name, t.s, t.n) == 0 && current_match < 16);

      // if there is just one match, make it the substitute
      if(current_match == 2) {
        free(matches[0]);
        matches[0] = malloc(strlen(matches[1]) + 1);
        strcpy(matches[0], matches[1]);
      }
    }
  }

  return matches;
}
*/
static void initialize_readline()
{
  rl_readline_name = "Poprc";
  /*
#if defined(__clang__)
  rl_attempted_completion_function = (CPPFunction *)completion;
#else
  rl_attempted_completion_function = (rl_completion_func_t *)completion;
#endif
  */
}

#endif

#define HISTORY_FILE ".poprc_history"
#define GRAPH_FILE "cells.dot"
#define REDUCED_GRAPH_FILE "reduced.dot"
bool write_graph = false;

void run_eval(bool echo) {
  char *line_raw, *line;
#ifdef USE_LINENOISE
  //linenoiseSetCompletionCallback(completion);
  linenoiseHistoryLoad(HISTORY_FILE);
  while((line_raw = linenoise(": ")))
#elif USE_READLINE
  initialize_readline();
  while((line_raw = readline(": ")))
#else
  char buf[1024];
  while(tty && printf(": "),
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

    if(echo) puts(line);
    bool run = eval_command(line);

#ifndef RAW_LINE
    free(line_raw);
#endif
    if(!run) break;
  }
}

bool eval_command(char *line) {
  cell_t *p = lex(line, 0), *p0 = p;
  if(match(p, ":")) {
    if(!(p = p->tok_list.next)) goto fail;
    if(match(p, "m")) {
      measure_display();
    } else if(match(p, "d")) {
      print_modules();
    } else if(match(p, "g")) {
      write_graph = !write_graph;
      printf("graph %s\n", write_graph ? "ON" : "OFF");
    } else if(match(p, "l")) {
      load_file(p->tok_list.next->tok_list.location);
    } else if(match(p, "q")) {
      goto fail;
    } else if(match(p, "t")) {
      p = p->tok_list.next;
      const char *name = p ? p->tok_list.location : "";
      run_test(name);
    } else if(match(p, "a")) {
      csize_t in, out;
      if(!(p = p->tok_list.next)) goto fail;
      if(get_arity(p, &in, &out, NULL)) {
        printf("%d -> %d\n", in, out);
      }
    } else {
      printf("unknown command\n");
    }
  } else if(p) {
    cell_t *e = NULL;
    if((e = check_reserved(p))) {
      const char *line = e->tok_list.line;
      const char *loc = e->tok_list.location;
      size_t size = strlen(line);
      find_line(loc, &line, &size);
      int pos = loc - line;
      COUNTUP(i, pos + 2) putchar(' ');
      printf("^--- Parse error\n");
    } else {
      measure_start();
      eval(p);
      measure_stop();
    }
  }
  free_toks(p0);
  return true;
fail:
  free_toks(p0);
  return false;
}

void reduce_root(cell_t *c) {
  rt_init();
  if(write_graph) make_graph_all(GRAPH_FILE);
  reduce_list(c);
  if(write_graph) make_graph_all(REDUCED_GRAPH_FILE);
}

void eval(const cell_t *p) {
  cell_t *c = parse_expr(&p, NULL);
  if(!c) return;
  csize_t s = list_size(c);
  if(s > 0 && !closure_is_ready(c->value.ptr[s-1])) {
    printf("incomplete expression\n");
  } else {
    reduce_root(c);
    c = remove_row(c);
    show_list(c);
    printf("\n");
  }
  drop(c);
}

bool get_arity(const cell_t *p, csize_t *in, csize_t *out, cell_t *module) {
  set_trace(NULL);
  cell_t *c = parse_expr(&p, module);
  if(!c) return false;
  *in = fill_args(c, NULL);
  c = remove_row(c);
  *out = max(1, list_size(c));
  drop(c);
  return true;
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

static struct mmfile files[16] = {};
size_t files_cnt = 0;

bool load_file(const char *path) {
  if(files_cnt >= LENGTH(files)) return false;
  struct mmfile *f = &files[files_cnt++];
  f->path = path;
  f->read_only = true;
  if(!mmap_file(f)) return false;

  cell_t *toks = lex(f->data, f->data + f->size);
  cell_t *e = NULL;
  const char *name = NULL;
  printf("Load %s ", path);
  char *s = "(";
  while((name = parse_module(&toks, &e))) {
    printf("%s%s", s, name);
    s = ", ";
  }
  printf(")\n");

  if(e) {
    const char *line = e->tok_list.line;
    const char *loc = e->tok_list.location;
    size_t size = f->size - (line - f->data);
    find_line(loc, &line, &size);
    printf("%.*s\n", (int)size, line);
    int pos = loc - line;
    int line_no = line_number(f->data, loc);
    COUNTUP(i, pos) putchar(' ');
    printf("^--- Parse error on line %d of %s\n", line_no, path);
  }
  free_toks(toks);
  return true;
}

bool unload_files() {
  bool success = true;
  COUNTUP(i, files_cnt) {
    success &= munmap_file(&files[i]);
  }
  return success;
}
