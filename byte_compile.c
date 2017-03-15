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
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <stdarg.h>
#include <inttypes.h>

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/byte_compile.h"
#include "gen/parse.h"
#include "gen/print.h"
#include "gen/lex.h"
#include "gen/module.h"
#include "gen/user_func.h"
#include "gen/list.h"

bool trace_enabled = false;

cell_t trace_cells[1 << 10];
cell_t *trace_cur = &trace_cells[0];
cell_t *trace_ptr = &trace_cells[0];
static cell_t *initial_word = NULL;

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

static bool pre_compile_word(cell_t *l, cell_t *module, csize_t *in, csize_t *out);
static bool compile_word(cell_t **entry, seg_t name, cell_t *module, csize_t in, csize_t out);
static cell_t *compile_quote(cell_t *parent_entry, cell_t *q);

#define DEBUG 0

#if INTERFACE
// use NIL_INDEX < -256
// so that is_offset() is false, otherwise problems with traverse/closure_next_child
#define NIL_INDEX (-4096)

#define FOR_TRACE(c, start, end) for(cell_t *(c) = (start); c < (end); c += calculate_cells(c->size))
#endif

cell_t *trace_encode(trace_index_t index) {
  return FLIP_PTR((cell_t *)index);
}

trace_index_t trace_decode(cell_t *c) {
  return (trace_index_t)FLIP_PTR(c);
}

static
cell_t *trace_get(const cell_t *r) {
  assert(r && is_var(r));
  cell_t *tc = r->value.ptr[0];
  assert(tc >= trace_cur && tc < trace_ptr);
  return tc;
}

static
cell_t *trace_lookup_value_linear(int type, val_t value) {
  FOR_TRACE(p, trace_cur, trace_ptr) {
    if(p->func == func_value &&
       p->value.type.exclusive == type &&
       p->value.integer[0] == value)
      return p;
  }
  return NULL;
}

static
trace_index_t trace_get_value(cell_t *r) {
  assert(r && is_value(r));
  if(is_list(r)) {
    return trace_build_quote(r); // *** TODO prevent building duplicate quotes
  } else if(is_var(r)) {
    return trace_get(r) - trace_cur;
  } else {
    cell_t *t = trace_lookup_value_linear(r->value.type.exclusive, r->value.integer[0]);
    if(t) return t - trace_cur;
  }
  assert(false);
  return -1;
}

cell_t *trace_alloc(csize_t args) {
  if(!trace_enabled) return NULL;
  size_t size = calculate_cells(args);
  cell_t *tc = trace_ptr;
  tc->n = -1;
  trace_ptr += size;
  tc->size = args;
  return tc;
}

void trace_shrink(cell_t *t, csize_t args) {
  csize_t prev_size = t->size;
  assert(args <= prev_size);
  csize_t
    prev_cells = calculate_cells(t->size),
    new_cells = calculate_cells(args),
    diff = prev_cells - new_cells;
  t->size = args;

  // blank the extra cells
  cell_t *p = &t[prev_cells];
  LOOP(diff) {
    memset(p, 0, sizeof(cell_t));
    p->size = 1;
  }
}

static
cell_t *trace_copy(const cell_t *c) {
  cell_t *tc = trace_alloc(c->size);
  size_t size = closure_cells(c);
  memcpy(tc, c, sizeof(cell_t) * size);
  return tc;
}

static
cell_t *trace_store_expr(const cell_t *c, const cell_t *r) {
  cell_t *tc = trace_get(r);
  type_t t = r->value.type;
  if(tc->func) {
    if(is_value(tc)) {
      tc->value.type = t;
    }
    return tc;
  }
  assert(tc->size == c->size);
  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->n = n;
  if(tc->func == func_dep_entered) tc->func = func_dep;
  cell_t **e = (tc->func == func_exec || tc->func == func_quote) ? &tc->expr.arg[closure_in(tc) - 1] : NULL;
  tc->expr.rec = 1;
  traverse(tc, {
      if(p == e) {
        *p = trace_encode(*p - trace_cells);
      } else if(*p) {
        trace_index_t x = trace_get_value(*p);
        *p = trace_encode(x);
        trace_cur[x].n++;
      }
    }, ARGS_IN);
  traverse(tc, {
      if(*p) {
        trace_index_t x = trace_get_value(*p);
        *p = trace_encode(x);
      }
    }, ARGS_OUT);
  if(is_value(c)) {
    tc->value.alt_set = 0;
    tc->value.type = t;
  }
  tc->expr_type.exclusive = t.exclusive;
  tc->alt = NULL;
  return tc;
}

static
cell_t *trace_store_value(const cell_t *c) {
  assert(!is_list(c));
  cell_t *tc = trace_lookup_value_linear(c->value.type.exclusive, c->value.integer[0]);
  if(tc) return tc;
  tc = trace_copy(c);
  tc->value.alt_set = 0;
  tc->alt = NULL;
  tc->n = -1;
  return tc;
}

static
cell_t *trace_store_self(const cell_t *c, const cell_t *r) {
  cell_t *tc = trace_get(r);
  type_t t = r->value.type;
  tc->func = func_exec;
  tc->n += c->n;
  cell_t *e = &trace_cur[-1];
  csize_t
    in = e->entry.in,
    out = e->entry.out;
  tc->expr.arg[in] = trace_encode(e - trace_cells);
  trace_shrink(tc, in + out);

  // extract variables from c and feed them into the self call
  // a little HACKy in the way this is done, but it works
  cell_t *vl = 0;
  trace_var_list((cell_t *)c, &vl);
  COUNTUP(i, in) {
    cell_t *x = trace_get(vl);
    tc->expr.arg[i] = trace_encode(x - trace_cur);
    x->n++;
    if(!(vl = vl->tmp)) break;
  }
  clean_tmp(vl);

  // TODO handle out args
  assert(out == 1);

  tc->expr_type.exclusive = t.exclusive;
  tc->alt = NULL;
  return tc;
}

static
bool trace_match_self(const cell_t *c) {
  const cell_t *s = initial_word;
  if(!s || c->expr.rec) return false;
  if(c->func != func_exec ||
     c->size != s->size) return false;
  for(csize_t i = closure_next_child(s) + 1; i < c->size; i++) {
    cell_t *a = s->expr.arg[i];
    if(a && c->expr.arg[i] != a) return false;
  }
  return true;
}

static
cell_t *trace_store(const cell_t *c, const cell_t *r) {
  if(is_var(r)) {
    if(trace_match_self(c)) {
      return trace_store_self(c, r);
    } else {
      return trace_store_expr(c, r);
    }
  } else {
    return trace_store_value(c);
  }
}

static
void trace_init() {
  trace_cur = trace_ptr;
}

void print_bytecode(cell_t *e) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;
  printf("___ %s.%s (%d -> %d) ___\n", e->module_name, e->word_name, e->entry.in, e->entry.out);
  FOR_TRACE(c, start, end) {
    int t = c - start;
    printf("[%d]", t);
    if(!c->func) {
      printf("\n");
      continue;
    }
    if(is_value(c)) {
      if(is_list(c) || c->value.type.exclusive == T_RETURN) {
        if(c->value.type.exclusive == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %" PRIdPTR, trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
      } else if(is_var(c)) {
        printf(" var");
      } else {
        printf(" val %" PRIdPTR, c->value.integer[0]);
      }
      printf(", type = %s", show_type_all_short(c->value.type));
      if(c->alt) printf(" -> %" PRIdPTR, trace_decode(c->alt));
    } else {
      const char *module_name = NULL, *word_name = NULL;
      trace_get_name(c, &module_name, &word_name);
      if(c->func == func_quote) printf(" quote");
      printf(" %s.%s", module_name, word_name);
      cell_t **e = (c->func == func_exec || c->func == func_quote) ? &c->expr.arg[closure_in(c) - 1] : NULL;
      traverse(c, {
          if(p != e) {
            trace_index_t x = trace_decode(*p);
            if(x == -1) {
              printf(" X");
            } else if(x == NIL_INDEX) {
              printf(" []");
            } else {
              printf(" %" PRIdPTR, x);
            }
          }
        }, ARGS_IN);
      if(closure_out(c)) {
        printf(" ->");
        traverse(c, {
            trace_index_t x = trace_decode(*p);
            if(x == -1) {
              printf(" X");
            } else {
              printf(" %" PRIdPTR, x);
            }
          }, ARGS_OUT);
      }
      printf(", type = %s", show_type_all_short(c->expr_type));
      if(c->alt) printf(" -> %" PRIdPTR, trace_decode(c->alt));
    }
    printf(" x%d\n", c->n + 1);
  }

  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    printf("\n");
    print_bytecode(e);
  }
}

void trace_update(cell_t *c, cell_t *r) {
  if(!trace_enabled) return;
  if(is_list(r)) return;

  trace_store(c, r);
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  cell_t *tc = r->value.ptr[0];
  if(tc && !tc->func && trace_ptr - tc == calculate_cells(tc->size)) {
    trace_ptr = tc;
  }
}

void trace_reduction(cell_t *c, cell_t *r) {
  if(!trace_enabled) return;

  if(!is_var(r) || r->value.type.exclusive == T_LIST) return;

  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }

  csize_t in = closure_in(c);
  COUNTUP(i, c->func == func_exec ? in - 1 : in) {
    cell_t *a = c->expr.arg[i];
    if(is_value(a) && !is_var(a)) {
      if(is_list(a)) {
        //trace_build_quote(a);
      } else {
        trace_store(a, a);
      }
    }
  }

  trace_store(c, r);
}

void trace_composition(cell_t *c, UNUSED cell_t *a, UNUSED cell_t *b) {
  if(!trace_enabled) return;

  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }

  assert_throw(false, "TODO: compose placeholders");
}

void trace_update_type(cell_t *c) {
  if(!trace_enabled) return;

  int t = c->value.type.exclusive;

  if(t != T_LIST) {
    cell_t *tc = trace_get(c);
    if(tc->func) {
      trace_set_type(tc, t);
    }
  }
}

static
void trace_clear(cell_t *e) {
  size_t count = e->entry.len;
  memset(e, 0, (count + 1) * sizeof(cell_t));
}

static
void trace_final_pass(cell_t *e) {
  // replace alts with trace cells
  cell_t
    *start = e + 1,
    *end = start + e->entry.len;

  FOR_TRACE(p, start, end) {
    if(p->func == func_quote) {
      cell_t *qe = compile_quote(e, p);
      if(qe) {
        p->expr.arg[closure_in(p) - 1] = trace_encode(qe - trace_cells);
      } else {
        p->size = 2;
        p->func = func_ap;
        cell_t *x = p->expr.arg[0];
        p->expr.arg[0] = p->expr.arg[1];
        p->expr.arg[1] = x;
      }
    } else if(p->func == func_placeholder) {
      trace_index_t left = trace_decode(p->expr.arg[0]);
      assert(left >= 0);
      if(closure_in(p) > 1 && trace_type(&trace_cur[left]).exclusive == T_FUNCTION) {
        p->func = func_compose;
      } else {
        p->func = func_ap;
      }
    } else if(p->func == func_fcompose) {
      p->func = func_compose;
    }
  }
}

static
bool trace_is_recursive(cell_t *e) {
  bool ret = false;
  const cell_t *encoded_entry = trace_encode(e - trace_cells);
  cell_t
    *start = e + 1,
    *end = start + e->entry.len;

  FOR_TRACE(p, start, end) {
    if(p->func == func_exec &&
       p->expr.arg[closure_in(p)-1] == encoded_entry) {
      ret = true;
      break;
    }
  }
  return ret;
}

trace_index_t trace_tail(trace_index_t t, csize_t out) {
  if(out == 0) return t;
  cell_t *tc = trace_alloc(out + 1);
  tc->func = func_ap;
  tc->expr.out = out;
  tc->expr.arg[0] = trace_encode(t);
  trace_cur[t].n++;
  return tc - trace_cur;
}

bool any_unreduced(cell_t *c) {
  traverse(c, {
      if(*p) {
        if((reduce_t *)clear_ptr((*p)->func) == func_placeholder || is_value(*p)) {
          if(any_unreduced(*p)) return true;
        } else return true;
      }
    }, PTRS | ARGS_IN);
  return false;
}

void trace_set_type(cell_t *tc, int t) {
  tc->expr_type.exclusive = t;
  if(is_value(tc)) {
    tc->value.type.exclusive = t;
  }
}

trace_index_t trace_build_quote(cell_t *l) {
  assert(is_list(l));
  if(is_empty_list(l)) return NIL_INDEX;
  if(is_var(l) && // ***
     list_size(l) == 1 &&
     is_var(l->value.ptr[0]) &&
     is_function(l->value.ptr[0])) {
    return trace_get(l->value.ptr[0]) - trace_cur;
  }
  cell_t *vl = 0;
  trace_var_list(l, &vl);
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 1);

  n->expr.out = 0;
  n->func = func_quote;
  n->n = -1;
  n->expr_type.exclusive = T_LIST;

  cell_t *p = vl;
  COUNTUP(i, in) {
    trace_index_t x = trace_get_value(p);
    trace_cur[x].n++;
    n->expr.arg[i] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  n->expr.arg[in] = ref(l); // entry points to the quote for now
  insert_root(&n->expr.arg[in]);

  return n - trace_cur;
}

static
cell_t *trace_return(cell_t *c) {
  c = flat_copy(c);
  cell_t **p;
  FORLIST(p, c, true) {
    trace_index_t x;
    if(is_list(*p)) {
      x = trace_build_quote(*p);
    } else {
      x = trace_store(*p, *p) - trace_cur;
    }
    *p = trace_encode(x);
    trace_cur[x].n++;
  }
  cell_t *t = trace_copy(c);
  closure_free(c);
  t->value.type.exclusive = T_RETURN;
  t->n = -1;
  t->alt = NULL;
  return t;
}

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp && tail != &c->tmp) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
      tail = trace_var_list(c->alt, tail);
    } else {
      c->tmp = FLIP_PTR(0); // prevent loops
      traverse(c, {
          tail = trace_var_list(*p, tail);
        }, PTRS | ARGS_IN | ALT);
      c->tmp = 0;
    }
  }
  return tail;
}

size_t tmp_list_length(cell_t *c) {
  size_t n = 0;
  while(c) {
    c = c->tmp;
    n++;
  }
  return n;
}

void tmp_list_filter(cell_t **p, int t) {
  while(*p) {
    if((*p)->value.type.exclusive == t) {
      *p = (*p)->tmp;
    } else {
      p = &(*p)->tmp;
    }
  }
}

int test_var_count() {
  cell_t *l = lex("? [? +] [[?] dup] [[[[?]]]] ? dup", 0);
  const cell_t *p = l;
  cell_t *c = parse_expr(&p, NULL);
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  size_t n = tmp_list_length(vl);
  printf("length(vl) = %d\n", (int)n);
  clean_tmp(vl);
  drop(c);
  return n == 5 ? 0 : -1;
}

static
unsigned int trace_reduce(cell_t **cp) {
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;

  insert_root(cp);

  cell_t **p = cp;
  while(*p) {
    if(!func_list(p, req_simple(T_RETURN))) continue;
    cell_t **a;
    FORLIST(a, *p, true) {
      collapse_row(a);
      reduce(a, req_any); // ***
      if(!is_list(*a)) {
        trace_store(*a, *a);
      }
    }
    cell_t *r = trace_return(*p);
    r->n++;
    *prev = trace_encode(r - trace_cur);
    alts++;
    p = &(*p)->alt;
    prev = &r->alt;
  }

  remove_root(cp);
  return alts;
}

static
void store_entries(cell_t *e, cell_t *module) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;
  module_set(module, string_seg(e->word_name), e);
  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    store_entries(e, module);
  }
}

static
cell_t *compile_entry(seg_t name, cell_t *module) {
  csize_t in, out;
  cell_t *entry = implicit_lookup(name, module);
  if(!is_list(entry)) return entry;
  cell_t *ctx = get_module(string_seg(entry->module_name)); // switch context module for words from other modules
  if(pre_compile_word(entry, ctx, &in, &out) &&
     compile_word(&entry, name, ctx, in, out)) {
    store_entries(entry, module);
    return entry;
  } else {
    return NULL;
  }
}

void compile_module(cell_t *module) {
  map_t map = module->value.map;
  FORMAP(i, map) {
    pair_t *x = &map[i];
    char *name = (char *)x->first;
    if(strcmp(name, "imports") != 0) {
      compile_entry(string_seg(name), module);
    }
  }
}

cell_t *module_lookup_compiled(seg_t path, cell_t **context) {
  cell_t *p = module_lookup(path, context);
  if(!p) return NULL;
  if(!is_list(p)) return p;
  if(p->value.type.flags & T_TRACED) {
    if(p->alt) { // HACKy
      return p->alt;
    } else {
      return lookup_word(string_seg("??"));
    }
  }
  p->value.type.flags |= T_TRACED;
  seg_t name = path_name(path);
  return compile_entry(name, *context);
}

cell_t *parse_eval_def(seg_t name, cell_t *rest) {
  cell_t *eval_module = module_get_or_create(modules, string_seg("eval"));
  cell_t *l = quote(rest);
  l->module_name = "eval";
  module_set(eval_module, name, l);
  return module_lookup_compiled(name, &eval_module);
}

static
bool pre_compile_word(cell_t *l, cell_t *module, csize_t *in, csize_t *out) {
  cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1
  // arity (HACKy)
  // must parse twice, once for arity, and then reparse with new entry
  // also compiles dependencies
  // TODO make mutual recursive words compile correctly
  bool res = get_arity(toks, in, out, module);
  return res;
}

static
bool compile_word(cell_t **entry, seg_t name, cell_t *module, csize_t in, csize_t out) {
  cell_t *l;
  if(!entry || !(l = *entry)) return false;
  if(!is_list(l)) return true;
  if(is_empty_list(l)) return false;

  // make recursive return this entry
  (*entry)->alt = trace_ptr;

  cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1

  // set up
  trace_init();

  cell_t *e = *entry = trace_ptr;
  trace_cur = ++trace_ptr;

  e->n = PERSISTENT;
  e->module_name = module_name(module);
  e->word_name = seg_string(name); // TODO fix unnecessary alloc
  e->entry.in = in;
  e->entry.out = out;
  e->entry.len = 0;

  // parse
  const cell_t *p = toks;
  e->entry.flags = ENTRY_NOINLINE;
  e->func = func_exec;
  cell_t *c = parse_expr(&p, module);

  // compile
  trace_enabled = true;
  cell_t *left = *leftmost(&c);
  if(!is_value(left) &&
     (reduce_t *)clear_ptr(left->func) == func_exec &&
     !left->expr.rec &&
     closure_out(left) + 1 == out) {
    left->expr.rec = 1; // always expand root
    initial_word = copy(left);
  } else {
    initial_word = NULL;
  }
  fill_args(c);
  e->entry.alts = trace_reduce(&c);
  drop(c);
  trace_enabled = false;
  e->entry.len = trace_ptr - trace_cur;
  trace_final_pass(e);
  e->entry.flags &= ~ENTRY_NOINLINE;
  if(!trace_is_recursive(e)) {
    e->entry.rec = 1;
  }

  // finish
  free_def(l);
  closure_free(initial_word);
  return true;
}

static
bool is_id(cell_t *e) {
  cell_t *ret = &e[4];
  return
    e->entry.in == 2 &&
    e->entry.out == 0 &&
    e->entry.len == 4 &&
    list_size(ret) == 1 &&
    trace_decode(ret->value.ptr[0]) == 2;
}

void replace_var(cell_t *c, cell_t **a, csize_t a_n, cell_t *e) {
  trace_index_t x = (c->value.ptr[0] - e) - 1;
  COUNTUP(j, a_n) {
    trace_index_t y = trace_decode(a[j]);
    if(y == x) {
      trace_cur[j].value.type.exclusive = trace_type(c->value.ptr[0]).exclusive;
      c->value.ptr[0] = &trace_cur[a_n - 1 - j];
      return;
    }
  }
  c->value.ptr[0] = trace_alloc(2);
  //assert(false);
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
static
cell_t *compile_quote(cell_t *parent_entry, cell_t *q) {
  // set up
  trace_init();
  trace_enabled = true;

  cell_t *e = trace_ptr;
  trace_cur = ++trace_ptr;

  csize_t in = closure_in(q) - 1;
  cell_t **fp = &q->expr.arg[in];
  assert(*fp);
  assert(remove_root(fp));
  cell_t *c = *fp;
  e->n = PERSISTENT;
  e->entry.len = 0;
  e->entry.flags = ENTRY_NOINLINE | ENTRY_QUOTE | (is_row_list(c) ? ENTRY_ROW : 0);
  e->func = func_exec;

  // allocate variables
  csize_t n = in;
  COUNTUP(i, n) {
    cell_t *tc = &trace_cur[i];
    tc->size = 2;
    tc->func = func_value;
    tc->value.type.flags = T_VAR;
    tc->n = -1;
  }
  trace_ptr += n;

  // free variables
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  for(cell_t *p = vl; p; p = p->tmp) {
    replace_var(p, q->expr.arg, n, parent_entry);
  }
  clean_tmp(vl);

  // compile
  e->entry.in = in + fill_args(c);
  e->entry.alts = trace_reduce(&c);
  e->entry.out = function_out(c, true);
  assert(e->entry.out);
  drop(c);
  trace_enabled = false;
  e->entry.flags &= ~ENTRY_NOINLINE;
  if(!trace_is_recursive(e)) {
    e->entry.rec = 1;
  }
  e->entry.len = trace_ptr - trace_cur;
  if(is_id(e)) {
    trace_clear(e);
    trace_ptr = trace_cur; // reset
    return NULL;
  }

  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(q - parent_entry) - 1);
  trace_final_pass(e);
  return e;
}

static
cell_t *tref(cell_t *e, cell_t *c) {
  trace_index_t i = trace_decode(c);
  return i < 0 ? NULL : &e[i+1];
}

type_t trace_type(cell_t *c) {
  return is_value(c) ? c->value.type : c->expr_type;
}

void resolve_types(cell_t *e, cell_t *c, type_t *t) {
  csize_t n = list_size(c);
  COUNTUP(i, n) {
    t[i].exclusive = trace_type(tref(e, c->value.ptr[i])).exclusive;
  }
  cell_t *p = tref(e, c->alt);
  while(p) {
    COUNTUP(i, n) {
      int pt = trace_type(tref(e, p->value.ptr[i])).exclusive;
      if(t[i].exclusive == T_BOTTOM) {
        t[i].exclusive = pt;
      } else if(t[i].exclusive != pt &&
                pt != T_BOTTOM) {
        t[i].exclusive = T_ANY;
      }
    }
    p = tref(e, p->alt);
  }
}

// very similar to get_name() but decodes entry
void trace_get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if((reduce_t *)clear_ptr(c->func) == func_exec ||
     (reduce_t *)clear_ptr(c->func) == func_quote) {
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = function_name(c->func);
  }
}

void command_bytecode(cell_t *rest) {
  if(rest) {
    command_def(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);
    if(e) {
      print_bytecode(e);
    }
  }
}
