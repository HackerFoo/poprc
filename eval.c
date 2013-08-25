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
  CASE(force);
  CASE(cut);
  CASE(alt2);
  CASE(ifte);
  CASE(dip11);
  CASE(dip12);
  CASE(dip21);
  CASE(head);
  CASE(fib);
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
  fprintf(f, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>(%ld) %s%s %d %d (%d)</b></font></td></tr>",
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
    } else if(c->type == T_FAIL) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else if(c->type == T_INDIRECT) {
      fprintf(f, "<tr><td port=\"ind\">ind: <font color=\"lightgray\">%p</font></td></tr>", (cell_t *)c->val[0]);
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
    } else if(c->type == T_INDIRECT) {
      if(is_cell((cell_t *)c->val[0])) {
	fprintf(f, "node%ld:ind -> node%ld:top;\n",
		node, (long int)((cell_t *)c->val[0] - cells));
	graph_cell(f, (cell_t *)c->val[0]);
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
  assert(c && c->type == T_INT);
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
  for(i = 0; i < n; ++i) {
    cell_t *arg = c->arg[i];
    if(is_closure(arg)) {
      show_one(arg);
    }
  }
  printf(" %s", s);
}

void show_var(cell_t *c) {
  assert(c->type == T_VAR);
  printf(" ?%c%ld", type_char(c->val[0]), c - cells);
}

void show_one(cell_t *c) {
  if(!c) {
    printf(" []");
  } else if(!is_closure(c)) {
    printf(" ?");
  } else if(!is_reduced(c)) {
    show_func(c);
  } else if(c->type == T_INT) {
    show_val(c);
  } else if(c->type == T_INDIRECT) {
    show_one((cell_t *)c->val[0]);
  } else if(c->type == T_VAR) {
    show_var(c);
  } else if(c->type == T_FAIL) {
    printf(" {}");
  } else if(is_list(c)) {
    show_list(c);
  } else {
    printf(" ?");
  }
}

bool reduce_one(cell_t **cp) {
  if(!closure_is_ready(*cp)) return true;
  return reduce(cp, T_ANY);
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
  char *line;
#ifdef USE_LINENOISE
  linenoiseSetCompletionCallback(completion);
  linenoiseHistoryLoad(HISTORY_FILE);
  while((line = linenoise(": ")))
#else
  char buf[1024];
  while(printf(": "),
        (line = fgets(buf, sizeof(buf), stdin)))
#endif
  {
#ifndef USE_LINENOISE
    char *p = line;
    while(*p && *p != '\n') ++p;
    *p = 0;
#endif
    if(line[0] == '\0') {
#ifdef USE_LINENOISE
      free(line);
#endif
      continue;
    }
    if(strcmp(line, ":m") == 0) {
      measure_display();
    } else if(strcmp(line, ":g") == 0) {
      write_graph = !write_graph;
      printf("graph %s\n", write_graph ? "ON" : "OFF");
    } else if(strncmp(line, ":t ", 3) == 0) {
      if(line[3])
	runTests(&line[3]);
    } else if(strcmp(line, ":q") == 0) {
#ifdef USE_LINENOISE
      free(line);
#endif
      break;
    } else if(strncmp(line, ":c ", 3) == 0) {
      cells_init();
      compile_expr(line+3, strlen(line+3));
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
    free(line);
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
    return
      lookup(word_table,
	     WIDTH(word_table),
	     word_table_length,
	     w);
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
    if(!e) return NULL;
    if(strnlen(w, sizeof_field(word_entry_t, name)) !=
       strnlen(e->name, sizeof_field(word_entry_t, name)))
      return NULL;
    c = func(e->func, e->in, e->out);
    *in = e->in;
    *out = e->out;
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
  unsigned int in = 0, out = 0;
  cell_t *c;
  char *tok = *p = rtok(str, *p);
  if(!tok || strcmp(tok, "[") == 0) return false;
  if(strcmp(tok, "]") == 0) {
    c = _build(str, p);
    *r = pushl(c, *r);
  } else if(strcmp(tok, ")") == 0) {
    c = parse_vector(str, p);
    *r = pushl(c, *r);
  } else {
    c = word_parse(tok, &in, &out);
    if(c) *r = compose_expand(c, out, *r);
  }
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

void fill_args(cell_t *r) {
  int n = list_size(r);
  cell_t *l = r->ptr[n-1];
  int i = 0;
  while(!closure_is_ready(l)) {
    cell_t *v = var(T_ANY | ++i << 8);
    arg(l, v);
  }
}

cell_t *_build(char *str, char **p) {
  cell_t *r = empty_list();
  while((parse_word(str, p, &r)));
  return r;
}

void eval(char *str, unsigned int n) {
  cell_t *c = build(str, n);
  fill_args(c);
  if(write_graph) make_graph_all(GRAPH_FILE);
  reduce_list(c);
  if(write_graph) make_graph_all(REDUCED_GRAPH_FILE);
  if(!c) return;
  if(!closure_is_ready(c))
    printf("incomplete expression\n");
  else {
    print_trace();
    show_list(c);
    drop(c);
    printf("\n");
  }
}

void compile_expr(char *str, unsigned int n) {
  cell_t *c = build(str, n);
  if(!c) return;
  word_entry_t *e = lookup_word("func");
  e->func = compile(c, &e->in, &e->out);
}

void runTests(char *path) {
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
      eval(line, strlen(line));
      check_free();
    }
  }
  fclose(f);
}

char *show_type(type_rep_t t) {
#define case(x) case x: return #x
  switch(t) {
  case(T_ANY);
  case(T_FAIL);
  case(T_INDIRECT);
  case(T_INT);
  case(T_VAR);
  case(T_IO);
  case(T_LIST);
  }
  return "???";
}

char type_char(type_rep_t t) {
  switch(t & 0xff) {
  case T_ANY: return '_';
  case T_FAIL: return 'x';
  case T_INDIRECT: return 'z';
  case T_INT: return 'i';
  case T_VAR: return 'v';
  case T_IO: return 'w';
  case T_LIST: return 'l';
  }
  return '?';
}

void print_trace() {
  cell_t *p = trace;
  int i, n;
  while(p < trace_ptr) {
    cell_t *c = p->tmp;
    if(is_reduced(p)) {
      printf("?%c%ld <-", type_char(p->n), c - cells);
      if(is_var(p)) {
	if(p->val[0] >> 8) {
	  printf(" arg(%d)", (int)(p->val[0] >> 8)-1);
	} else {
	  printf(" type");
	}
      } else {
	show_val(p);
      }
      printf("\n");
    } else if(is_dep(p)) {
      printf("?%c%ld <- type\n", type_char(p->n), c - cells);
    } else {
      n = closure_args(p);
      i = closure_out(p);
      printf("?%c%ld ", type_char(p->n), c - cells);
      while(i--) printf("?%ld ", p->arg[--n] - cells);
      printf("<- ");
      for(i = 0; i < n; i++) {
	if(is_cell(p->arg[i])) printf("?%ld ", p->arg[i] - cells);
	else printf("?_ ");
      }
      printf("%s\n", function_name(p->func));
    }
    p += closure_cells(p);
  }
}
