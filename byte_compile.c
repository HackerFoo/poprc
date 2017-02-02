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

bool trace_enabled = false;

cell_t trace_cells[1 << 10];
cell_t *trace_cur = &trace_cells[0];
cell_t *trace_ptr = &trace_cells[0];
size_t trace_cnt = 0;
static MAP(trace_values, 1 << 6); // val_t -> trace_index_t

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

static void update_alt(cell_t *c, cell_t *r);
static trace_index_t trace_build_quote(cell_t *q, trace_index_t li);
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
  return r->value.ptr[0];
}

static
trace_index_t trace_get_value(const cell_t *r) {
  assert(r && is_value(r));
  if(is_var(r)) {
    return r->value.ptr[0] - trace_cur;
  } else if(r->value.type.exclusive == T_INT) { // for now
    pair_t *x = map_find(trace_values, r->value.integer[0]);
    if(x) return x->second;
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
  trace_cnt++;
  return tc;
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
  if(tc->func) return tc;
  assert(tc->size == c->size);
  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->n = n;
  if(tc->func == func_dep_entered) tc->func = func_dep;
  traverse(tc, {
      if(*p) {
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
  type_t t = r->value.type;
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
  if(c->value.type.exclusive == T_INT) {
    pair_t *x = map_find(trace_values, c->value.integer[0]);
    if(x) return &trace_cur[x->second];
  }
  cell_t *tc = trace_copy(c);
  if(c->value.type.exclusive == T_INT) {
    pair_t x = {c->value.integer[0], tc - trace_cur};
    map_insert(trace_values, x);
  }
  return tc;
}

static
cell_t *trace_store(const cell_t *c, const cell_t *r) {
  if(is_var(r)) {
    return trace_store_expr(c, r);
  } else {
    return trace_store_value(c);
  }
}

static
void trace_init() {
  trace_cur = trace_ptr;
  trace_cnt = 0;
  map_clear(trace_values);
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
      printf(" NULL!\n");
      continue;
    }
    if(is_value(c)) {
      if(is_var(c)) {
        printf(" var");
      } else if(is_list(c) || c->value.type.exclusive == T_RETURN) {
        if(c->value.type.exclusive == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %" PRIdPTR, trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
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
            if(x == NIL_INDEX) {
              printf(" []");
            } else {
              printf(" %" PRIdPTR, x);
            }
          }
        }, ARGS_IN);
      if(closure_out(c)) {
        printf(" ->");
        traverse(c, {
            printf(" %" PRIdPTR, trace_decode(*p));
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

void trace(cell_t *c, cell_t *r, trace_type_t tt) {
  if(!trace_enabled) return;

  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }

  switch(tt) {

  case tt_reduction: {
    if(!is_var(r)) break;
    if(r->value.type.exclusive == T_LIST) break;

    csize_t in = closure_in(c);
    COUNTUP(i, c->func == func_exec ? in - 1 : in) {
      cell_t *a = c->expr.arg[i];
      if(is_value(a) && !is_var(a)) {
        trace_store(a, a);
      }
    }
  }
  // continue below

  case tt_update:
    trace_store(c, r);
    break;

  case tt_compose_placeholders: {
    assert_throw(false, "TODO: compose placeholders");
    break;
  }

  case tt_fail: {
    update_alt(c, r);
    break;
  }
  }
}

// TODO replace with something more efficient
static
void update_alt(cell_t *c, cell_t *r) {
  FOR_TRACE(p, trace_cur, trace_ptr) {
    if(p->alt == c) p->alt = r;
  }
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
        p->func = func_pushl;
      }
    } else if(p->func == func_placeholder) {
      p->func = func_ap;
    }
  }
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

// TODO unevaluated functions instead for later compilation
static
trace_index_t trace_store_list(cell_t *c) {
  csize_t n = list_size(c);
  trace_index_t li = NIL_INDEX;

  if(n > 0) {
    cell_t *p = c->value.ptr[n-1];
    if(is_placeholder(p)) { // unreduced placeholder
      li = trace_get_value(p->expr.arg[closure_in(p) - 1]);
    } else if (is_var(p)) { // reduced placeholder
      cell_t *t = p->value.ptr[0];
      if(t && is_placeholder(t)) {
        li = t - trace_cur;
        n--;
      }
    }
  }

  COUNTUP(i, n) {
    li = trace_build_quote(c->value.ptr[i], li);
  }
  return li;
}

static
cell_t *trace_return(cell_t *c) {
  c = copy(c);
  traverse(c, {
      if(*p) {
        trace_index_t x;
        if(is_list(*p)) {
          x = trace_store_list(*p);
        } else {
          x = trace_store(*p, *p) - trace_cur;
        }
        *p = trace_encode(x);
        trace_cur[x].n++;
      }
    }, PTRS);
  cell_t *t = trace_copy(c);
  closure_free(c);
  t->value.type.exclusive = T_RETURN;
  t->n = -1;
  t->alt = NULL;
  return t;
}

static
trace_index_t trace_build_quote(cell_t *q, trace_index_t li) {
  cell_t *vl = 0;
  cell_t **vlp = &vl;

  vlp = trace_var_list(q, vlp);
  tmp_list_filter(&vl, T_FUNCTION);
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 2);

  n->expr.out = 0;
  n->func = func_quote;
  n->n = -1;
  n->expr_type.exclusive = T_LIST;

  cell_t *p = vl;
  COUNTUP(i, in) {
    trace_index_t x = trace_get_value(p);
    trace_cur[x].n++;
    n->expr.arg[in - i - 1] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  if(li >= 0) trace_cur[li].n++;
  n->expr.arg[in] = trace_encode(li);
  n->expr.arg[in + 1] = ref(q); // entry points to the quote for now
  insert_root(&n->expr.arg[in + 1]);

  return n - trace_cur;
}

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp && tail != &c->tmp) {
    if(is_var(c)) {
      LIST_ADD(tmp, tail, c);
    } else {
      c->tmp = FLIP_PTR(0); // prevent loops
      traverse(c, {
          tail = trace_var_list(*p, tail);
        }, PTRS | ARGS_IN);
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

void print_trace_index()
{
  //print_map(trace_index);
  print_map(trace_values);
}

static
unsigned int trace_reduce(cell_t *c) {
  csize_t n = list_size(c);
  cell_t *r = NULL;
  cell_t *first;
  unsigned int alts = 0;
  csize_t conflict = 0;

  insert_root(&c);

  // first one
  COUNTUP(i, n) {
    cell_t **a = &c->value.ptr[i];
    reduce(a, T_ANY);
    if(!is_list(*a)) {
      trace_store(*a, *a);
    }
  }
  if(!(conflict = any_conflicts((cell_t const *const *)c->value.ptr, n))) {
    r = trace_return(c);
    r->n++;
    alts++;
  }
  first = r;

  // rest
  cell_t *p = copy(c);
  while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, conflict, n)) {
    COUNTUP(i, n) {
      cell_t **a = &p->value.ptr[i];
      reduce(a, T_ANY);
      trace_store(*a, *a);
    }
    if(!(conflict = any_conflicts((cell_t const *const *)p->value.ptr, n))) {
      cell_t *prev = r;
      r = trace_return(p);
      r->n++;
      prev->alt = trace_encode(r - trace_cur);
      alts++;
    }
  }

  remove_root(&c);
  closure_free(p);
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
      return lookup_word(string_seg("_"));
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
  return compile_entry(name, eval_module);
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
  if(list_size(l) < 1) return false;

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
  fill_args(c);
  e->entry.alts = trace_reduce(c);
  drop(c);
  trace_enabled = false;
  e->entry.len = trace_cnt;
#if DEBUG
  print_trace_index();
#endif
  trace_final_pass(e);
  e->entry.flags &= ~ENTRY_NOINLINE;

  // finish
  free_def(l);
  return true;
}

static
bool is_id(cell_t *e) {
  cell_t *in_var = &e[1], *ret = &e[2];
  return
    e->entry.in == 1 &&
    e->entry.out == 1 &&
    e->entry.len == 2 &&
    is_var(in_var) &&
//    ret->expr_type == T_RETURN &&
    list_size(ret) == 1 &&
    trace_decode(ret->value.ptr[0]) == 0;
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
  assert(remove_root(&q->expr.arg[in]));
  cell_t *c = quote(q->expr.arg[in]);
  e->n = PERSISTENT;
  e->entry.out = 1;
  e->entry.len = 0;
  e->entry.flags = ENTRY_NOINLINE | ENTRY_QUOTE;
  e->func = func_exec;

  // free variables
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  for(cell_t *p = vl; p; p = p->tmp) {
    p->value.ptr[0] = trace_alloc(2);
    trace_store(p, p);
  }
  clean_tmp(vl);

  // compile
  e->entry.in = in + fill_args(c) - 1;
  e->entry.alts = trace_reduce(c);
  drop(c);
  trace_enabled = false;
  e->entry.flags &= ~ENTRY_NOINLINE;
  e->entry.len = trace_cnt;
  if(is_id(e)) {
    trace_ptr = trace_cur; // reset
    return NULL;
  }

  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(q - parent_entry) - 1);
#if DEBUG
  print_trace_index();
#endif
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

static
cell_t *map_cell(cell_t **map, intptr_t x) {
  return
    x == NIL_INDEX ? &nil_cell :
    x < 0 ? NULL :
    map[x];
}

static
cell_t *get_return_arg(cell_t **map, cell_t *returns, intptr_t x) {
  trace_index_t i = trace_decode(returns->value.ptr[x]);
  return
    i == NIL_INDEX ? empty_list() :
    i < 0 ? NULL :
    map[i];
}

bool func_exec(cell_t **cp, UNUSED int t) {
  cell_t *c = clear_ptr(*cp);
  assert(is_closure(c));

  size_t in = closure_in(c) - 1;
  cell_t *entry = c->expr.arg[in];
  cell_t *code = entry + 1;
  size_t len = entry->entry.len;
  cell_t *map[len];
  cell_t *res;
  cell_t *returns = NULL;

  // don't execute, just reduce all args and return variables
  if(entry->entry.flags & (ENTRY_NOINLINE | ENTRY_RECURSIVE)) {
    entry->entry.flags |= ENTRY_RECURSIVE;
    csize_t c_in = closure_in(c), n = closure_args(c);
    alt_set_t alt_set = 0;
    bool expnd = true;
    for(csize_t i = 0; i < c_in - 1; ++i) {
      if(!reduce_arg(c, i, &alt_set, T_ANY)) goto fail;
      // if any vars in a recursive function, don't expand
      // TODO make this less dumb
      if(is_var(clear_ptr(c->expr.arg[i]))) expnd = false;
    }
    if(expnd) goto expand;
    for(csize_t i = c_in; i < n; ++i) {
      cell_t **d = &c->expr.arg[i];
      if(*d && is_dep(*d)) {
        cell_t *v = var(T_BOTTOM, *d);
        v->value.alt_set = alt_set;
        store_reduced(d, v);
      }
    }

    cell_t *res = var(t == T_ANY ? T_BOTTOM : t, c);
    res->value.alt_set = alt_set;
    store_reduced(cp, res);
    return true;

  fail:
    fail(cp, t);
    return false;
  }

expand:

  c->expr.arg[in] = 0;
  memset(map, 0, sizeof(map[0]) * len);

  COUNTUP(i, in) {
    cell_t *p = &code[i];
    assert(is_var(p));
    map[i] = refn(c->expr.arg[in - 1 - i], p->n);
  }

  // allocate, copy, and index
  size_t s = 0;
  for(size_t i = in; i < len; i += s) {
    cell_t *p = &code[i];
    s = closure_cells(p);
    if(trace_type(p).exclusive == T_RETURN) {
      if(!returns) returns = p;
      continue;
    }
    cell_t *nc = closure_alloc_cells(s);
    memcpy(nc, p, s * sizeof(cell_t));
    nc->tmp = 0;
    map[i] = nc;
  }

  // rewrite pointers
  for(size_t i = in; i < len; i++) {
    cell_t *t = map_cell(map, i);
    if(!t) continue;

    // skip rewriting for the entry argument
    cell_t **t_entry = NULL;
    if(t->func == func_exec || t->func == func_quote) {
      t_entry = &t->expr.arg[closure_in(t) - 1];
      *t_entry = &trace_cells[trace_decode(*t_entry)];
    }

    traverse(t, {
        if(p != t_entry) {
          trace_index_t x = trace_decode(*p);
          *p = map_cell(map, x);
        }
      }, ARGS | PTRS | ALT);
  }

  // handle returns
  uint8_t alt_n = int_log2(entry->entry.alts);
  uint8_t alt_id = new_alt_id(alt_n);
  unsigned int branch = 0;
  size_t
    out = closure_out(c),
    n = closure_args(c);
  cell_t **results[out + 1];
  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i];
  }

  // first one
  alt_set_t alt_set = as_multi(alt_id, alt_n, branch++);
  res = id(get_return_arg(map, returns, out), alt_set);
  COUNTUP(i, out) {
    cell_t *d = c->expr.arg[n - 1 - i];
    d->func = func_id;
    d->expr.arg[0] = get_return_arg(map, returns, i);
    d->expr.alt_set = alt_set;
  }

  // rest
  trace_index_t next = trace_decode(returns->alt);
  while(next >= 0) {
    alt_set_t as = as_multi(alt_id, alt_n, branch++);
    returns = &code[next];
    FOREACH(i, results) {
      cell_t *a = get_return_arg(map, returns, i);
      results[i] = &(*results[i])->alt;
      *results[i] = a ? id(a, as) : NULL;
    }
    next = trace_decode(returns->alt);
  }

  // drop c from deps
  LOOP(out) {
    drop(c);
  }

  store_lazy(cp, c, res, 0);
  return false;
}

bool func_exec_recursive(cell_t **cp, int t) {
  cell_t *c = clear_ptr(*cp);
  assert(is_closure(c));

  csize_t c_in = closure_in(c);
  alt_set_t alt_set = 0;
  for(csize_t i = 0; i < c_in - 1; ++i) {
    if(!reduce_arg(c, i, &alt_set, T_ANY)) goto fail;
  }

  c->func = func_exec;
  return func_exec(cp, t);

fail:
  fail(cp, t);
  return false;
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

// takes free variables and returns a quoted function
bool func_quote(cell_t **cp, UNUSED int t) {
  cell_t *c = clear_ptr(*cp);
  assert(is_closure(c));

  csize_t in = closure_in(c) - 2;
  cell_t *entry = c->expr.arg[in + 1];
  c->expr.arg[in + 1] = 0;
  csize_t f_in = entry->entry.in;

  cell_t *f = closure_alloc(f_in + 1);
  csize_t offset = f_in - in;
  if(offset) {
    f->expr.arg[0] = (cell_t *)(trace_index_t)(offset - 1);
    f->func = (reduce_t *)mark_ptr(func_exec);
  } else {
    f->func = func_exec;
  }

  COUNTUP(i, in) {
    f->expr.arg[i + offset] = c->expr.arg[i];
  }

  f->expr.arg[f_in] = entry;

  cell_t *res = closure_alloc(2);
  res->func = func_pushl;
  res->expr.arg[0] = f;
  res->expr.arg[1] = c->expr.arg[in];

  store_lazy(cp, c, res, 0);
  return false;
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
