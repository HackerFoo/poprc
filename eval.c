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
#include "linenoise/linenoise.h"
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/test.h"

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

/*
void print_sexpr_help(cell_t *);

void print_list_help(cell_t *c) {
  if(is_nil(c)) {
    printf(" ] ");
    return;
  }
  // assert(is_cons(c));
  print_sexpr_help(c->arg[0]);
  print_list_help(c->arg[1]); 
}

void print_list(cell_t *c) {
  printf(" [");
  print_list_help(c);
}

void print_sexpr_help(cell_t *r) {
  if(!is_closure(r)) {
    printf(" 0x%.16x", (int)(intptr_t)r);
  } else if(is_reduced(r)) {
    if(r->type == T_INT) {
      printf(" %d", (int)r->val);
    } else if(is_cons(r)) {
      printf(" [");
      print_sexpr_help(r->ptr);
      printf(" ]");
    } else if(r->type == T_FAIL) {
      printf(" (FAIL)");
    }
  } else if(r->func == func_dep) {
    printf(" (dep)");
  } else {

    int i;
    int args = closure_args(r);

    printf(" (%s", function_name(r->func));

    for(i = 0; i < args; i++) {
      print_sexpr_help(r->arg[i]);
    }

    printf(")");
  }
}

void print_sexpr(cell_t *r) {
  print_sexpr_help(r);
  printf("\n");
}
*/

char *function_name(reduce_t *f) {
  f = clear_ptr(f);
  //  int i;
# define CASE(n) if(f == func_##n) return #n
  CASE(add);
  CASE(sub);
  CASE(mul);
  CASE(reduced);
  CASE(append);
  CASE(pushl);
  CASE(pushr);
  CASE(quote);
  CASE(dep);
  CASE(popr);
  CASE(alt);
  CASE(concat);
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
  /*
  FOREACH(word_table, i) {
    if(word_table[i].func == f)
      return word_table[i].name;
  }
  */
  return "?";
# undef CASE
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

uint8_t visited[(LENGTH(cells)+7)/8] = {0};

void graph_cell(FILE *f, cell_t *c) {
  if(!is_closure(c)) return;
  long unsigned int node = c - cells;
  if(check_bit(visited, node)) return;
  set_bit(visited, node);
  int n = closure_args(c);
  /* functions with extra args */
  if(c->func == func_alt ||
     c->func == func_concat) n++;
  int i;

  /* print node attributes */
  fprintf(f, "node%ld [\nlabel =<", node);
  fprintf(f, "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td port=\"top\" bgcolor=\"black\"><font color=\"white\"><b>%ld: %s%s (%d)</b></font></td></tr>",
	  node,
	  function_name(c->func),
	  closure_is_ready(c) ? "" : "*",
	  c->n);
  fprintf(f, "<tr><td port=\"alt\">alt: <font color=\"lightgray\">%p</font></td></tr>",
             c->alt);
  if(is_reduced(c)) {
    fprintf(f, "<tr><td>alt_set: X%s</td></tr>",
	    show_alt_set(c->alt_set));
    if(is_list(c)) {
      int n = list_size(c);
      while(n--)
	fprintf(f, "<tr><td port=\"ptr\">ptr: <font color=\"lightgray\">%p</font></td></tr>",
		c->ptr[n]);
    } else if(c->type == T_FAIL) {
      fprintf(f, "<tr><td bgcolor=\"red\">FAIL</td></tr>");
    } else {
      int n = c->val_size;
      while(n--)
	fprintf(f, "<tr><td bgcolor=\"yellow\">val: %ld</td></tr>", c->val[n]);
    }
  } else {
    for(i = 0; i < n; i++) {
      fprintf(f, "<tr><td port=\"arg%d\"><font color=\"lightgray\">%p</font></td></tr>", i, c->arg[i]);
    }
  }
  fprintf(f, "</table>>\nshape = \"none\"\n];\n");

  /* print edges */
  if(c->alt) {
    cell_t *alt = clear_ptr(c->alt);
    fprintf(f, "node%ld:alt -> node%ld:top;\n",
	    node, alt - cells);
    graph_cell(f, c->alt);
  }
  if(is_reduced(c)) {
    if(is_list(c)) {
      if(c->next) {
	fprintf(f, "node%ld:next -> node%ld:top;\n",
		node, c->next - cells);
	graph_cell(f, c->next);
      }
      if(is_list(c) && c->ptr) {
	int n = list_size(c);
	while(n--) {
	  fprintf(f, "node%ld:ptr -> node%ld:top;\n",
		  node, c->ptr[n] - cells);
	  graph_cell(f, c->ptr[n]);
	}
      }
    }
  } else {
    for(i = 0; i < n; i++) {
      cell_t *arg = clear_ptr(c->arg[i]);
      if(is_closure(arg)) {
	fprintf(f, "node%ld:arg%d -> node%ld:top;\n",
		c - cells, i, arg - cells);
	graph_cell(f, arg);
      }
    }
  }
}

void show_val(cell_t *c) {
  assert(c && c->type == T_INT);
  int n = c->val_size;
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
    ++p;
  }
  return b;
}

void show_list(cell_t *c) {
  assert(c && is_list(c));
  int n = list_size(c);
  if(n) {
    printf(" [");
    while(n--) show_one(c->ptr[n]);
    printf(" ]");
  } else printf(" []");
}

void show_one(cell_t *c) {
  if(!c) {
    printf(" []");
  } else if(c->type == T_INT) {
    show_val(c);
  } else if(is_list(c)) {
    show_list(c);
  } else {
    printf(" ?");
  }
}

bool reduce_one(cell_t *c) {
  return reduce(c) &&
    (!is_list(c) || reduce_list(c));
}

cell_t *reduce_alt(cell_t *c) {
  cell_t *r, *t, *p = c, *q;
  /* skip initial failures */
  while(p && !reduce_one(p)) {
    t = ref(p->alt);
    deref(p);
    p = t;
  }
  /* store first success */
  r = q = p;
  /* deref initial failures */
  if(!p) return 0;
  /* append remaining successes */
  p = p->alt;
  while(true) {
    while(p && reduce_one(p)) {
      q = p;
      p = p->alt;
    }
    /* q points to last success, p to first failure */
    if(!p) break;
    while(p && !reduce_one(p)) {
      t = ref(p->alt);
      deref(p);
      p = t;
    }
    /* p points to next success */
    q->alt = p;
    q = q->alt;
  }
  return r;
}
    
void show_alt(cell_t *c) {
  cell_t *p = c, *t;

  if(!p) return;
  //p = reduce_alt(p);
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
/*
void show_eval(cell_t *c) {
  printf("[");
  show_alt(reduce_alt(c));
  deref(c);
  printf(" ]\n");
}
*/
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

int main(int argc, char *argv[]) {
  unsigned int test_number;
  if(argc != 2) {
    run_eval();
  } else if(strcmp("all", argv[1]) == 0) {
    int i;
    FOREACH(tests, i) {
      printf("_________________________________"
	     "(( test %-3d ))"
	     "_________________________________\n", i);
      cells_init();
      measure_start();
      tests[i]();
      measure_stop();
      check_free();
    }
  } else {
    test_number = atoi(argv[1]);
    if(test_number >= LENGTH(tests)) return -2;
    cells_init();
    //alloc_test();
    measure_start();
    tests[test_number]();
    measure_stop();
    check_free();
  }
  measure_display();
  return 0;
}

#define HISTORY_FILE ".pegc_history"
#define GRAPH_FILE "cells.dot"
#define REDUCED_GRAPH_FILE "reduced.dot"
bool write_graph = false;
void run_eval() {
  char *line;
  linenoiseSetCompletionCallback(completion);
  linenoiseHistoryLoad(HISTORY_FILE);
  while((line = linenoise(": "))) {
    if(line[0] == '\0') {
      free(line);
      break;
    }
    if(strcmp(line, ":m") == 0) {
      measure_display();
    } else if(strcmp(line, ":g") == 0) {
      write_graph = !write_graph;
      printf("graph %s\n", write_graph ? "ON" : "OFF");
    } else {
      linenoiseHistoryAdd(line);
      linenoiseHistorySave(HISTORY_FILE);
      cells_init();
      measure_start();
      eval(line, strlen(line));
      measure_stop();
      check_free();
    }
    free(line);
  }
}

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

bool is_num(char *str) {
  return char_class(str[0]) == CC_NUMERIC ||
    (str[0] == '-' && char_class(str[1]) == CC_NUMERIC);
}

word_entry_t *lookup_word(const char *w) {
    return
      lookup(word_table,
	     WIDTH(word_table),
	     LENGTH(word_table),
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
  } else {
    word_entry_t *e = lookup_word(w);
    /* disallow partial matches */
    if(!e) return NULL;
    if(strnlen(w, sizeof_field(word_entry_t, name)) !=
       strnlen(e->name, sizeof_field(word_entry_t, name)))
      return NULL;
    c = func(e->func, e->in + e->out - 1);
    if(e->func == func_alt)
      c->arg[2] = (cell_t *)(intptr_t)alt_cnt++;
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
  if(class == CC_BRACKET) return ptr;

  do {
    if(ptr > str) ptr--;
    else return ptr;
    if(class == CC_NUMERIC && char_class(*ptr) == CC_ALPHA)
      class = CC_ALPHA;
  } while(char_class(*ptr) == class);

  /* handle negative numbers */
  if(!(class == CC_NUMERIC &&
       *ptr == '-')) ptr++;

  return ptr;
}

bool parse_word(char *str, char **p, cell_t **r) {
  unsigned int in = 0, out = 0;
  cell_t *c;
  char *tok = *p = rtok(str, *p);
  if(!tok || strcmp(tok, "[") == 0) return false;
  if(strcmp(tok, "]") == 0) {
    c = _build(str, p);
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

cell_t *_build(char *str, char **p) {
  cell_t *r = empty_list();
  while((parse_word(str, p, &r)));
  return r;
}

void eval(char *str, unsigned int n) {
  cell_t *c = build(str, n);
  if(write_graph) make_graph(GRAPH_FILE, c);
  c = reduce_alt(c);
  if(write_graph) make_graph(REDUCED_GRAPH_FILE, c);
  if(!c) return;
  if(!closure_is_ready(c))
    printf("incomplete expression\n");
  else {
    show_list(c);
    deref(c);
    printf("\n");
  }
}
