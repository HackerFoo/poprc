/* Copyright 2012-2013 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "rt_types.h"
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#ifdef USE_LINENOISE
#include "linenoise/linenoise.h"
#endif
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/test.h"
#include "llvm.h"

word_entry_t user_word_table[64] = {{""}};
const int user_word_table_length = LENGTH(user_word_table);
word_entry_t *new_user_word_entry = user_word_table;

char *show_alt_set(uintptr_t as) {
  static char out[sizeof(as)*4+1];
  char *p = out;
  int n = sizeof(as)*4;
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

char *function_name(reduce_t *f) {
  f = clear_ptr(f, 1);
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
  CASE(dup);
  CASE(swap);
  CASE(drop);
  CASE(cut);
  CASE(alt2);
  CASE(fib);
  CASE(select);
  CASE(placeholder);
  return "?";
# undef CASE
}

char *function_token(reduce_t *f) {
  int i;
  f = clear_ptr(f, 1);
  for(i = 0; i < word_table_length; ++i) {
    if(word_table[i].func == f)
      return word_table[i].name;
  }
  return NULL;
}

/* Graphviz graph generation */
void make_graph(char *path, cell_t *c) {
  FILE *f = fopen(path, "w");
  fprintf(f, "digraph g {\n"
	     "graph [\n"
	     "rankdir = \"RL\"\n"
	     "];\n");
  zero(visited);
  graph_cell(f, c);
  fprintf(f, "}\n");
  fclose(f);
}

void make_graph_all(char *path) {
  int i;
  FILE *f = fopen(path, "w");
  fprintf(f, "digraph g {\n"
	     "graph [\n"
	     "rankdir = \"RL\"\n"
	     "];\n");
  zero(visited);
  FOREACH(cells, i) {
    graph_cell(f, &cells[i]);
  }
  fprintf(f, "}\n");
  fclose(f);
}

uint8_t visited[(LENGTH(cells)+7)/8] = {0};

void graph_cell(FILE *f, cell_t *c) {
  c = clear_ptr(c, 3);
  if(!is_closure(c) || !is_cell(c)) return;
  long unsigned int node = c - cells;
  if(check_bit(visited, node)) return;
  set_bit(visited, node);
  int n = closure_args(c);
  int i, s = calculate_cells(n);

  for(i = 0; i < s; ++i) set_bit(visited, node+i);

  /* print node attributes */
  fprintf(f, "node%ld [\nlabel =<", node);
  fprintf(f, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>(%ld) %s%s %x %x (%d)</b></font></td></tr>",
	  node,
	  function_name(c->func),
	  closure_is_ready(c) ? "" : "*",
	  (int)c->size,
	  (int)c->out,
	  (int)c->n);
  fprintf(f, "<tr><td port=\"alt\">alt: <font color=\"lightgray\">%p</font></td></tr>",
             c->alt);
  if(is_reduced(c)) {
    fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
	    show_alt_set(c->alt_set));
    if(is_list(c)) {
      int n = list_size(c);
      while(n--)
	fprintf(f, "<tr><td port=\"ptr%d\">ptr: <font color=\"lightgray\">%p</font></td></tr>",
		n, c->ptr[n]);
    } else if(is_fail(c)) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else {
      int n = val_size(c);
      while(n--)
	fprintf(f, "<tr><td bgcolor=\"yellow\">val: %ld</td></tr>", (long int)c->val[n]);
    }
  } else {
    for(i = 0; i < n; i++) {
      fprintf(f, "<tr><td port=\"arg%d\"><font color=\"lightgray\">%p</font></td></tr>", i, c->arg[i]);
    }
    if(c->func == func_id) {
      fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
	      show_alt_set((alt_set_t)c->arg[1]));
    }
  }
  fprintf(f, "</table>>\nshape = \"none\"\n];\n");

  /* print edges */
  if(is_cell(c->alt)) {
    cell_t *alt = clear_ptr(c->alt, 1);
    fprintf(f, "node%ld:alt -> node%ld:top;\n",
	    node, (long int)(alt - cells));
    graph_cell(f, c->alt);
  }
  if(is_reduced(c)) {
    if(is_list(c)) {
      int n = list_size(c);
      while(n--) {
	if(is_cell(c->ptr[n])) {
	  fprintf(f, "node%ld:ptr%d -> node%ld:top;\n",
		  node, n, (long int)(c->ptr[n] - cells));
	  graph_cell(f, c->ptr[n]);
	}
      }
    }
  } else {
    for(i = 0; i < n; i++) {
      cell_t *arg = clear_ptr(c->arg[i], 1);
      if(is_cell(arg)) {
	fprintf(f, "node%ld:arg%d -> node%ld:top%s;\n",
		(long int)(c - cells), i, (long int)(arg - cells), is_weak(c, arg) ? " [color=lightgray]" : "");
	graph_cell(f, arg);
      }
    }
  }
}

void show_val(cell_t *c) {
  assert(c && type_match(T_INT, c));
  int n = val_size(c);
  switch(n) {
  case 0: printf(" ()"); break;
  case 1: printf(" %d", (int)c->val[0]); break;
  default:
    printf(" (");
    while(n--) printf(" %d", (int)c->val[n]);
    printf(" )");
    break;
  }
}

bool reduce_list(cell_t *c) {
  bool b = true;
  cell_t **p = c->ptr;
  while(*p && is_closure(*p)) {
    *p = reduce_alt(*p);
    b &= *p != 0;
    if(*p == 0) *p = &fail_cell;
    ++p;
  }
  return b;
}

bool any_alt_overlap(cell_t **p, unsigned int size) {
  int i;
  uintptr_t t, as = 0;
  for(i = 0; i < size; ++i) {
    if((t = p[i]->alt_set) & as) return true;
    as |= t;
  }
  return false;
}

bool any_conflicts(cell_t **p, unsigned int size) {
  int i;
  uintptr_t t, as = 0;
  for(i = 0; i < size; ++i) {
    if(bm_conflict(as, t = p[i]->alt_set)) return true;
    as |= t;
  }
  return false;
}

void show_list(cell_t *c) {
  assert(c && is_list(c));
  int n = list_size(c), i;
  if(n) {
    if(any_alt_overlap(c->ptr, n)) {
      cell_t *p = 0, *m1 = 0, *m2 = 0;

      /* find first match */
      if(!any_conflicts(c->ptr, n)) {
	m1 = c;
      } else {
	p = copy(c);
	while(count(p->ptr, c->ptr, n) >= 0) {
	  if(!any_conflicts(p->ptr, n)) {
	    m1 = p;
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
	while(count(p->ptr, c->ptr, n) >= 0) {
	  if(!any_conflicts(p->ptr, n)) {
	    m2 = p;
	    break;
	  }
	}
	if(m2) printf(" {");
	/* at least one match */
 	printf(" [");
	i = n; while(i--) show_one(m1->ptr[i]);
	printf(" ]");
	if(m1 != c) closure_free(m1);
	if(m2) {
	  /* second match */
	  printf(" | [");
	  i = n; while(i--) show_one(m2->ptr[i]);
	  printf(" ]");
	  /* remaining matches */
	  while(count(p->ptr, c->ptr, n) >= 0) {
	    if(!any_conflicts(p->ptr, n)) {
	      printf(" | [");
	      i = n; while(i--) show_one(p->ptr[i]);
	      printf(" ]");
	    }
	  }
	  printf(" }");
	}
	closure_free(p);
      }
    } else {
      printf(" [");
      i = n; while(i--) show_alt(c->ptr[i]);
      printf(" ]");
    }
  } else printf(" []");
}

int count(cell_t **cnt, cell_t **reset, int size) {
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
/*
void test_count() {
  int cnt[3];
  int reset[3] = {2, 1, 3};
  memcpy(cnt, reset, sizeof(reset));
  do {
    printf("%d %d %d\n", cnt[0], cnt[1], cnt[2]);
  } while(count(cnt, reset, 3) >= 0);
}
*/
void show_func(cell_t *c) {
  int n = closure_args(c), i;
  char *s = function_token(c->func);
  if(!s) return;
  if(is_placeholder(c)) printf(" ?%ld =", c - cells);
  for(i = 0; i < n; ++i) {
    cell_t *arg = c->arg[i];
    if(is_closure(arg)) {
      show_one(arg);
    }
  }
  printf(" %s", s);
}

void show_var(cell_t *c) {
  assert(is_var(c));
  if(is_list(c)) {
    //printf(" ?l%ld =", c-cells);
    show_list(c);
  } else {
    printf(" ?%c%ld", type_char(c->type), c - cells);
  }
}

void show_one(cell_t *c) {
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
    show_val(c);
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

void show_alt(cell_t *c) {
  cell_t *p = c, *t;

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
  bzero(&measure, sizeof(measure));
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
	 "allocated   : %d bytes\n"
	 "working set : %d bytes\n"
	 "reductions  : %d\n"
	 "alts used   : %d\n",
	 time,
	 saved_measure.alloc_cnt * (int)sizeof(cell_t),
	 saved_measure.max_alloc_cnt * (int)sizeof(cell_t),
	 saved_measure.reduce_cnt,
	 saved_measure.alt_cnt);

}

#ifndef EMSCRIPTEN
int main(int argc, char *argv[]) {
  run_eval();
  measure_display();
  return 0;
}
#endif

#define HISTORY_FILE ".pegc_history"
#define GRAPH_FILE "cells.dot"
#define REDUCED_GRAPH_FILE "reduced.dot"
bool write_graph = false;

void run_eval() {
  char *line_raw, *line;
#ifdef USE_LINENOISE
  linenoiseSetCompletionCallback(completion);
  linenoiseHistoryLoad(HISTORY_FILE);
  while((line_raw = linenoise(": ")))
#else
  char buf[1024];
  while(printf(": "),
        (line_raw = fgets(buf, sizeof(buf), stdin)))
#endif
  {
#ifndef USE_LINENOISE
    char *p = line_raw;
    while(*p && *p != '\n') ++p;
    *p = 0;
#endif
    if(line_raw[0] == '\0') {
#ifdef USE_LINENOISE
      free(line_raw);
#endif
      continue;
    }
    line = line_raw;
    while(*line == ' ') ++line;
    if(strcmp(line, ":m") == 0) {
      measure_display();
    } else if(strcmp(line, ":g") == 0) {
      write_graph = !write_graph;
      printf("graph %s\n", write_graph ? "ON" : "OFF");
    } else if(strncmp(line, ":l ", 3) == 0) {
      if(line[3])
	loadSource(&line[3]);
    } else if(strcmp(line, ":q") == 0) {
#ifdef USE_LINENOISE
      free(line_raw);
#endif
      break;
    } else if(strncmp(line, ":c ", 3) == 0) {
      cells_init();
      line += 3;
      while(*line == ' ') ++line;
      char *name = line;
      while(*line != ' ') ++line;
      *line++ = 0;
      compile_expr(name, line, strlen(line));
    } else if(strncmp(line, ":a ", 3) == 0) {
      unsigned int in, out;
      cells_init();
      line += 3;
      if(get_arity(line, strlen(line), &in, &out)) {
	printf("%d -> %d\n", in, out);
      }
    } else {
#ifdef USE_LINENOISE
      linenoiseHistoryAdd(line);
      linenoiseHistorySave(HISTORY_FILE);
#endif
      cells_init();
      measure_start();
      eval(line, strlen(line));
      measure_stop();
      check_free();
    }
#ifdef USE_LINENOISE
    free(line_raw);
#endif
  }
}

#ifdef USE_LINENOISE
void completion(const char *buf, linenoiseCompletions *lc) {
  int n = strlen(buf);
  char comp[n+sizeof_field(word_entry_t, name)];
  char *insert = comp + n;
  strncpy(comp, buf, sizeof(comp));
  char *tok = rtok(comp, insert);
  int tok_len = strnlen(tok, sizeof_field(word_entry_t, name));
  if(!tok) return;
  word_entry_t *e = lookup_word(tok);
  if(e) {
    /* add completions */
    do {
      if(strnlen(e->name, sizeof_field(word_entry_t, name)) >
	 tok_len) {

	strncpy(tok, e->name, sizeof(e->name));
	linenoiseAddCompletion(lc, comp);
      }
      e++;
    } while(strncmp(e->name, tok, tok_len) == 0);
  }
}
#endif

bool is_num(char *str) {
  return char_class(str[0]) == CC_NUMERIC ||
    (str[0] == '-' && char_class(str[1]) == CC_NUMERIC);
}

word_entry_t *lookup_word(const char *w) {
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

cell_t *word(char *w) {
  unsigned int in, out;
  return word_parse(w, &in, &out);
}

cell_t *word_parse(char *w,
		   unsigned int *in,
		   unsigned int *out) {
  cell_t *c;
  if(is_num(w)) {
    c = val(atoi(w));
    *in = 0;
    *out = 1;
  } else if(w[0] == '?') {
    c = var(T_ANY);
    *in = 0;
    *out = 1;
  } else {
    word_entry_t *e = lookup_word(w);
    /* disallow partial matches */
    if(!e ||
       strnlen(w, sizeof_field(word_entry_t, name)) !=
       strnlen(e->name, sizeof_field(word_entry_t, name))) {
      // trace the name ***
      c = func(func_placeholder, 0, 1);
      *in = 0;
      *out = 1;
    } else {
      c = func(e->func, e->in, e->out);
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
  if(index("[](){}", c))
    return CC_BRACKET;
  return CC_SYMBOL;
}

char *rtok(char *str, char *ptr) {
  if(ptr <= str) return NULL;

  ptr--;

  /* remove trailing spaces */
  while(char_class(*ptr) == CC_NONE) {
    if(ptr > str) ptr--;
    else return NULL;
  }

  *(ptr+1) = '\0';

  /* move to start of token */
  char_class_t class = char_class(*ptr);

  /* allow adjacent brackets to be seperately tokenized */
  if(class == CC_BRACKET ||
     class == CC_VAR) return ptr;

  do {
    if(ptr > str) ptr--;
    else return ptr;
    if(class == CC_NUMERIC) {
      if(char_class(*ptr) == CC_ALPHA) class = CC_ALPHA;
      //if(*ptr == '?') class = CC_VAR;
    }
  } while(char_class(*ptr) == class);

  /* handle negative numbers */
  if(!(class == CC_NUMERIC &&
       *ptr == '-')) ptr++;

  return ptr;
}

cell_t *parse_vector(char *str, char **p) {
  char *tok = *p;
  cell_t *c = vector(0);
  while((tok = rtok(str, tok)) &&
	strcmp("(", tok) != 0) {
    assert(is_num(tok));
    c = pushl_val(atoi(tok), c);
  }
  *p = tok;
  return c;
}

bool parse_word(char *str, char **p, cell_t **r) {
  unsigned int in = 0, out = 1;
  char *tok = *p = rtok(str, *p);
  if(!tok || strcmp(tok, "[") == 0) return false;
  cell_t *c = strcmp(tok, "]") == 0 ? _build(str, p) :
    strcmp(tok, ")") == 0 ? parse_vector(str, p) :
    word_parse(tok, &in, &out);
  if(c) *r = compose_expand(c, out, *r);
  return true;
}

cell_t *build(char *str, unsigned int n) {
  char *p = str;
  while(*p != '\0' && n--) {
    if(*p == '\n') {
      *p = '\0';
      break;
    }
    p++;
  }
  return _build(str, &p);
}

void argf_noop(cell_t *c, int i) {}
unsigned int fill_args(cell_t *r, void (*argf)(cell_t *, int)) {
  if(!argf) argf = argf_noop;
  int n = list_size(r);
  if(n < 1) return 0;
  cell_t **l = &r->ptr[n-1];
  int i = 0;
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

cell_t *_build(char *str, char **p) {
  cell_t *r = empty_list();
  while((parse_word(str, p, &r)));
  return r;
}

void print_arg(cell_t *c, int i) {
  printf("?_%ld <- arg(%d)\n", c-cells, i);
}

void eval(char *str, unsigned int n) {
  cell_t *c = build(str, n);
  set_trace(print_trace);
  fill_args(c, print_arg);
  if(write_graph) make_graph_all(GRAPH_FILE);
  reduce_list(c);
  if(write_graph) make_graph_all(REDUCED_GRAPH_FILE);
  if(!c) return;
  if(!closure_is_ready(c))
    printf("incomplete expression\n");
  else {
    show_list(c);
    drop(c);
    printf("\n");
  }
}

bool get_arity(char *str, unsigned int n, unsigned int *in, unsigned int *out) {
  set_trace(NULL);
  cell_t *c = build(str, n);
  *in = fill_args(c, NULL);
  reduce_list(c);
  if(!c) return false;
  if(!closure_is_ready(c)) {
    printf("incomplete expression\n");
    return false;
  } else {
    *out = list_size(c);
    drop(c);
    return true;
  }
}

void compile_expr(char *name, char *str, unsigned int n) {
  word_entry_t *e = 
    lookup_linear(user_word_table,
		  WIDTH(user_word_table),
		  user_word_table_length,
		  name);
  if(!e) {
    e = new_user_word_entry++;
    strcpy(e->name, name);
    e->func = func_placeholder;
    e->in = 1;
    e->out = 0;
  }
  char *s = malloc(n);
  memcpy(s, str, n);
  get_arity(s, n, &e->in, &e->out);
  free(s);
  e->func = func_self;
  cell_t *c = build(str, n);
  if(!c) {
    --new_user_word_entry;
    return;
  }
  e->func = compile(c, &e->in, &e->out);
}

void loadSource(char *path) {
  char buf[1024];
  char *line = 0;
  FILE *f = fopen(path, "r");
  if(!f) return;
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
      compile_expr(name, line, strlen(line));
    }
  }
  fclose(f);
}

char *show_type(type_rep_t t) {
#define case(x) case x: return #x
  switch(t & T_EXCLUSIVE) {
  case(T_ANY);
  case(T_INT);
  case(T_IO);
  case(T_LIST);
  default: return "???";
  }
}

char type_char(type_rep_t t) {
  switch(t & T_EXCLUSIVE) {
  case T_ANY: return '_';
  case T_INT: return 'i';
  case T_IO: return 'w';
  case T_LIST: return 'l';
  }
  return '?';
}

void print_trace(cell_t *c, cell_t *r, trace_type_t tt) {
  switch(tt) {
  case tt_reduction:
    if(is_reduced(c) || !is_var(r)) break;
    /*
    if(c->func == func_pushl ||
       c->func == func_pushr ||
       c->func == func_popr  ||
       c->func == func_compose) break;
    */
    if(is_list(r)) break;
    if(is_dep(c)) {
      printf("?%c%ld <- type\n", type_char(r->type), c - cells);
    } else {
      int i, n = closure_args(c), in = closure_in(c), out = closure_out(c);
      for(i = 0; i < in; ++i) trace(c->arg[i], 0, tt_force); // ***
      if(!is_placeholder(c)) printf("?%c%ld ", type_char(r->type), c - cells);
      for(i = 0; i < out; ++i) {
	cell_t *a = c->arg[n - i - 1];
	if(a) printf("?%ld ", a - cells);
	else printf("__ ");
      }
      printf("<- ");
      for(i = 0; i < in; ++i) {
	if(is_cell(c->arg[i])) printf("?%ld ", c->arg[i] - cells);
	else printf("?_ ");
      }
      if(is_placeholder(c)) {
	printf("f[?%ld]\n", c-cells);
      } else {
	printf("%s\n", function_name(c->func));
      }
    }
    r->type |= T_TRACED;
    break;
  case tt_touched:
    if(!is_var(c)) break;
  case tt_force:
    if(!is_reduced(c)) break;
    if(c->type & T_TRACED) break;
    if(is_any(c)) break;
    if(is_list(c) && is_placeholder(c->ptr[0])) {
      printf("?f%ld <- ?l%ld head\n", c->ptr[0]-cells, c-cells);
    } else {
      printf("?%c%ld <-", type_char(c->type), c-cells);
      if(is_var(c)) printf(" type");
      else show_one(c);
      printf("\n");
    }
    c->type |= T_TRACED;
    break;
  case tt_select:
    printf("?%c%ld <- ?%ld ?%ld select\n", type_char(c->type), c-cells, c-cells, r-cells);
    break;
  case tt_copy:
    printf("?f%ld <- ?%ld\n", c-cells, r-cells);
    break;
  case tt_compose_placeholders:
    printf("?f%ld <- ?%ld ?%ld compose_placeholders\n",
	   c-cells,
	   ((cell_t **)r)[0]-cells,
	   ((cell_t **)r)[1]-cells);
    break;
  }
}
