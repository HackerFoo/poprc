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

cell_t trace_cells[1 << 10];
cell_t *trace_cur = &trace_cells[0];
cell_t *trace_ptr = &trace_cells[0];
size_t trace_cnt = 0;
static MAP(trace_index, 1 << 10); // cell_t * -> uintptr_t (index in trace)
static MAP(trace_values, 1 << 6); // uintptr_t (value) -> uintptr_t (index in trace)

#define DEBUG 0

#if INTERFACE
#define FOR_TRACE(c, start, end) for(cell_t *(c) = (start); c < (end); c = closure_next(c))
#endif

static
pair_t *trace_find(const cell_t *c) {
  c = clear_ptr(c);
  if(is_list(c) && c->value.ptr[0] && is_placeholder(c->value.ptr[0])) {
    cell_t *ph = clear_ptr(c->value.ptr[0]);
    pair_t *res = map_find(trace_index, (uintptr_t)ph);
    if(res) return res;
  }
  return map_find(trace_index, (uintptr_t)c);
}

static
uintptr_t trace_get(cell_t *c) {
  pair_t *e = trace_find(c);
#if(DEBUG)
  if(!e) {
    return 100 + (c - cells);
  }
#else
  assert(e);
#endif
  return e->second;
}

void trace_index_add(const cell_t *c, uintptr_t x) {
  pair_t p = {(uintptr_t)c, x};
  map_insert(trace_index, p);
}

void trace_index_assign(cell_t *new, cell_t *old) {
  pair_t *p = trace_find(old);
  if(p) {
    trace_index_add(new, p->second);
  }
}

cell_t *trace_encode(uintptr_t index) {
  return (cell_t *)(-(index+1));
}

uintptr_t trace_decode(cell_t *c) {
  return -1 - (intptr_t)c;
}

static
uintptr_t map_update(map_t map, uintptr_t key, uintptr_t new_value) {
  pair_t *e = map_find(map, key);
  assert(e);
  uintptr_t old_value = e->second;
  e->second = new_value;
  return old_value;
}

cell_t *trace_store(const cell_t *c, type_t t) {
  if(is_value(c) && (c->value.type & T_EXCLUSIVE) == T_INT) {
    pair_t *x = map_find(trace_values, c->value.integer[0]);
    if(x) {
      trace_index_add(c, x->second);
      return &trace_cur[x->second];
    }
  }
  cell_t *dest = trace_ptr;
  csize_t size = closure_cells(c);
  trace_ptr += size;
  trace_cnt++;
  memcpy(dest, c, sizeof(cell_t) * size);
  if(c->func != func_assert) dest->alt = NULL;
  trace_index_add(c, dest - trace_cur);
  if(is_value(c) && (c->value.type & T_EXCLUSIVE) == T_INT) {
    pair_t p = {c->value.integer[0], dest - trace_cur};
    map_insert(trace_values, p);
  }

  dest->n = -1;

  // stuff type in expr_type
  dest->expr_type = t | T_TRACED;
  return dest;
}

cell_t *trace_dep(const cell_t *d, cell_t *c, type_t t) {
  cell_t *dest = trace_ptr++;
  trace_cnt++;
  trace_index_add(d, dest - trace_cur);
  dest->func = func_dep;
  dest->expr.arg[0] = c;
  dest->size = 1;
  dest->n = -2; // 1 less for weak reference

  // stuff type in expr_type
  dest->expr_type = t | T_TRACED;
  return dest;
}

void trace_rewrite(cell_t *c) {

  // skip returns (already rewritten)
  if((c->expr_type & T_TRACED) == 0) return;
  c->expr_type &= ~T_TRACED;

  // skip rewriting for the entry argument
  cell_t **entry = NULL;
  if(c->func == func_exec) {
    entry = &c->expr.arg[closure_in(c) - 1];
    *entry = trace_encode(*entry - trace_cells);
  }

  traverse(c, {
      if(p != entry && *p) {
        uintptr_t x = trace_get(*p);
        *p = trace_encode(x);
        trace_cur[x].n++;
      }
    }, ARGS | PTRS | ALT);
}

cell_t *trace_select(const cell_t *c, cell_t *a) {
  cell_t *dest = trace_ptr;
  csize_t size = closure_cells(c);
  trace_ptr += size;
  trace_cnt++;

  uintptr_t tc = map_update(trace_index, (uintptr_t)clear_ptr(c), dest - trace_cur);
  uintptr_t ta = trace_get(a);

  memset(dest, 0, sizeof(cell_t));
  dest->func = func_select;
  dest->expr.arg[0] = trace_encode(tc);
  dest->expr.arg[1] = trace_encode(ta);
  dest->size = 2;
  dest->n = -1;

  // TODO check types
  dest->tmp = trace_cur[tc].tmp;

  trace_cur[tc].n++;
  trace_cur[ta].n++;

  return dest;
}

void trace_update_type(const cell_t *c) {
  if(!is_value(c)) return;
  pair_t *p = trace_find(c);
  if(!p) return;
  cell_t *t = &trace_cur[p->second];
  type_t tt = trace_type(t);
  type_t ct = c->value.type;
  if((ct & T_EXCLUSIVE) == T_ANY) return;
  if((tt & T_EXCLUSIVE) != T_ANY &&
     (tt & T_EXCLUSIVE) != T_BOTTOM) return;

  if(is_value(t)) {
    t->value.type = c->value.type & ~T_TRACED;
  }

  // also stuff type in expr_type
  t->expr_type = c->value.type & ~T_TRACED;
}

void trace_init() {
  trace_cur = trace_ptr;
  trace_cnt = 0;
  map_clear(trace_index);
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
    if(is_value(c)) {
      if(is_var(c)) {
        printf(" var");
      } else if(is_list(c) || c->value.type == T_RETURN) {
        if(c->value.type == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %" PRIuPTR, trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
      } else {
        printf(" val %" PRIdPTR, c->value.integer[0]);
      }
      printf(", type = %s", show_type_all_short(c->value.type));
      if(c->alt) printf(" -> %" PRIuPTR, trace_decode(c->alt));
    } else {
      if(c->func == func_quote) {
        printf(" %s.%s_%d", e->module_name, e->word_name, t);
        COUNTUP(i, closure_in(c) - 1) {
          printf(" %" PRIuPTR, trace_decode(c->expr.arg[i]));
        }
      } else {
        const char *module_name = NULL, *word_name = NULL;
        trace_get_name(c, &module_name, &word_name);
        printf(" %s.%s", module_name, word_name);
        traverse(c, {
            printf(" %" PRIuPTR, trace_decode(*p));
          }, ARGS);
      }

      printf(", type = %s", show_type_all_short(c->expr_type));
      if(c->alt) printf(" -> %" PRIuPTR, trace_decode(c->alt));
    }
/*
    pair_t *p = map_find_value(trace_index, t);
    if(p) {
      printf(" (%d)", (int)p->first);
    }
*/
    printf(" x%d\n", c->n + 1);
  }

  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    printf("\n");
    print_bytecode(e);
  }
}

cell_t *trace_alloc(csize_t args) {
  cell_t *c = trace_ptr;
  trace_ptr += calculate_cells(args);
  trace_cnt++;
  c->size = args;
  return c;
}

uintptr_t bc_func(reduce_t f, csize_t in, csize_t out, ...) {
  assert(out > 0);
  va_list argp;
  csize_t args = in + out - 1;
  cell_t *c = trace_alloc(args);
  c->expr.out = out - 1;
  c->func = f;
  c->n = -1;

  va_start(argp, out);

  COUNTUP(i, in) {
    uintptr_t x = va_arg(argp, uintptr_t);
    trace_cur[x].n++;
    c->expr.arg[i] = trace_encode(x);
  }

  COUNTUP(i, out - 1) {
    uintptr_t d = bc_func(func_dep, 1, 1, c - trace_cur);
    uintptr_t *res = va_arg(argp, uintptr_t *);
    c->expr.arg[in + i] = trace_encode(d);
    *res = d;
  }

  va_end(argp);
  return c - trace_cur;
}

void bc_trace(cell_t *c, cell_t *r, trace_type_t tt, UNUSED csize_t n) {
  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }

  switch(tt) {

  case tt_reduction: {
    if(is_value(c) || !is_var(r)) break;
    if(c->func == func_placeholder ||
       c->func == func_dep) break;
    if(c->func == func_cut ||
       c->func == func_id) {
      trace_index_assign(c, c->expr.arg[0]);
    } else if(c->func == func_exec && !c->expr.arg[closure_in(c) - 1]) {
      // just replace exec with it's result
      trace_index_assign(c, r);
    } else if(c->func == func_compose) {
      // HACKy
      cell_t *p = c->expr.arg[1];
      if(is_var(p) && is_placeholder(p->value.ptr[0])) {
        trace_index_assign(r->value.ptr[0], p->value.ptr[0]);
      }
    } else {
      csize_t in = closure_in(c);
      csize_t out = closure_out(c);
      COUNTUP(i, c->func == func_exec ? in - 1 : in) {
        trace(c->expr.arg[i], c, tt_force, i);
      }
      COUNTUP(i, out) {
        cell_t *d = c->expr.arg[in + i];
        if(d) trace_dep(d, c, d->value.type);
      }
      trace_store(c, r->value.type);
    }
    r->value.type |= T_TRACED;
    break;
  }

  case tt_touched:
    if(!is_var(c)) break;
  case tt_force: {
    if(!is_value(c)) break;
    if(is_list(c) && is_placeholder(c->value.ptr[0])) {
      // kind of hacky; replaces placeholder its list var to be overwritten later
      trace_index_assign(c->value.ptr[0], c);
    } else if(is_var(c)) {
      trace_update_type(c);
    } else if(is_list(c)) {
      trace_store_quote(c);
    } else {
      trace_store(c, c->value.type);
    }
    c->value.type |= T_TRACED;
    break;
  }

  case tt_select: {
    trace_select(c, r);
    break;
  }

  case tt_copy: {
    trace_index_assign(c, r);
    break;
  }

  case tt_compose_placeholders: {
    /* to do *** */
    pair_t *pb = trace_find(((cell_t **)r)[1]);
    uintptr_t
      a = trace_get(((cell_t **)r)[0]),
      n = bc_func(func_compose, 2, 1, a, pb->second);
    pb->second = n;
    break;
  }

  case tt_fail: {
    update_alt(c, r);
    break;
  }
  }
}

// TODO replace with something more efficient
void update_alt(cell_t *c, cell_t *r) {
  FOR_TRACE(p, trace_cur, trace_ptr) {
    if(p->alt == c) p->alt = r;
  }
}

void trace_final_pass(cell_t *e) {
  // replace alts with trace cells
  cell_t
    *start = e + 1,
    *end = start + e->entry.len;

  FOR_TRACE(p, start, end) {
    trace_rewrite(p);
    if(p->func == func_quote) {
      cell_t *qe = compile_quote(e, p);
      assert(qe);
      p->expr.arg[closure_in(p) - 1] = trace_encode(qe - trace_cells);
    }
  }
}

void bc_arg(cell_t *c, UNUSED val_t x) {
  trace_store(c, T_ANY);
  c->value.type |= T_TRACED;
}

// TODO unevaluated functions instead for later compilation
cell_t *trace_store_list(cell_t *c) {
  traverse(c, {
      if(*p && !((*p)->value.type & T_TRACED)) {
        trace_store_quote(clear_ptr(*p));
        (*p)->value.type |= T_TRACED;
      }
    }, PTRS);
  return trace_store(c, c->value.type);
}

cell_t *trace_store_quote(cell_t *c) {
  if(list_size(c) == 0) {
    return trace_store(c, c->value.type);
  }
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 1);

  n->expr.out = 0;
  n->func = func_quote;
  n->n = -1;

  cell_t *p = vl;
  COUNTUP(i, in) {
    uintptr_t x = trace_get(p);
    trace_cur[x].n++;
    n->expr.arg[in - i - 1] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  n->expr.arg[in] = ref(c); // entry is c for now

  trace_index_add(c, n - trace_cur);
  return n;
}

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  traverse(c, {
      if(*p && !(*p)->tmp) {
        if(is_var(*p)) {
          *tail = *p;
          tail = &(*p)->tmp;
        } else {
          tail = trace_var_list(*p, tail);
        }
      }
    }, PTRS | ARGS_IN);
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
  static MAP(tmp_trace_index, 1 << 10);
  memcpy(tmp_trace_index, trace_index, sizeof(trace_index));

  // make index readable for debugging
  FORMAP(i, tmp_trace_index) {
    tmp_trace_index[i].first = (cell_t *)tmp_trace_index[i].first - cells;
  }

  print_map(tmp_trace_index);
  print_map(trace_values);
}

cell_t *trace_reduce(cell_t *c) {
  csize_t n = list_size(c);
  cell_t *r = NULL;
  cell_t *first;

  // first one
  COUNTUP(i, n) {
    reduce(&c->value.ptr[i], T_ANY);
  }
  if(!any_conflicts((cell_t const *const *)c->value.ptr, n)) {
    r = trace_store_list(c);
    trace_rewrite(r);
    r->n++;
    r->value.type = T_RETURN;
  }
  first = r;

  // rest
  cell_t *p = copy(c);
  while(count((cell_t const **)p->value.ptr, (cell_t const *const *)c->value.ptr, n) >= 0) {
    COUNTUP(i, n) {
      reduce(&p->value.ptr[i], T_ANY);
    }
    if(!any_conflicts((cell_t const *const *)p->value.ptr, n)) {
      cell_t *prev = r;
      r = trace_store_list(p);
      trace_rewrite(r);
      r->n++;
      r->value.type = T_RETURN;
      prev->alt = trace_encode(r - trace_cur);
    }
  }
  closure_free(p);
  return first;
}

cell_t *compile_entry(seg_t name, cell_t *module) {
  csize_t in, out;
  cell_t **entry = cmap_get(&module, name);
  cell_t *l = *entry;
  if(!is_list(l)) return l;
  *entry = NULL;
  bool pc_success = pre_compile_word(l, module, &in, &out);
  entry = cmap_get(&module, name);
  *entry = l;
  if(pc_success && compile_word(entry, module, in, out)) {
    return *entry;
  } else {
    return NULL;
  }
}

cell_t *module_lookup_compiled(seg_t path, cell_t **context) {
  cell_t **p = module_lookup(path, context);
  if(!p) return NULL;
  if(!*p) {
    return lookup_word(string_seg("_"));
  }
  if(!is_list(*p)) return *p;
  seg_t name = path_name(path);
  return compile_entry(name, *context);
}

cell_t *parse_eval_def(cell_t *name_tok, cell_t *rest) {
  seg_t name = tok_seg(name_tok);
  cell_t **eval_module = cmap_get(&modules, string_seg("eval"));
  cell_t **entry = cmap_get(eval_module, name);
  *entry = quote(rest);
  return compile_entry(name, *eval_module);
}

bool pre_compile_word(cell_t *l, cell_t *module, csize_t *in, csize_t *out) {
  cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1
  // arity (HACKy)
  // must parse twice, once for arity, and then reparse with new entry
  // also compiles dependencies
  // TODO make mutual recursive words compile correctly
  bool res = get_arity(toks, in, out, module);
  return res;
}

bool compile_word(cell_t **entry, cell_t *module, csize_t in, csize_t out) {
  cell_t *l;
  if(!entry || !(l = *entry)) return false;
  if(!is_list(l)) return true;
  if(list_size(l) < 1) return false;

  cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1

  // set up
  trace_init();

  cell_t *e = *entry = trace_ptr;
  trace_cur = ++trace_ptr;

  e->n = PERSISTENT;
  e->module_name = module_name(module);
  e->word_name = entry_name(module, e);
  e->entry.in = in;
  e->entry.out = out;
  e->entry.len = 0;

  // parse
  const cell_t *p = toks;
  e->entry.flags = ENTRY_NOINLINE;
  e->func = func_exec;
  cell_t *c = parse_expr(&p, module);

  // compile
  set_trace(bc_trace);
  fill_args(c, bc_arg);
  trace_reduce(c);
  drop(c);
  set_trace(NULL);
  e->entry.flags = 0;
  e->entry.len = trace_cnt;
  trace_final_pass(e);
#if DEBUG
  print_trace_index();
#endif

  // finish
  free_def(l);
  return true;
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
cell_t *compile_quote(cell_t *parent_entry, cell_t *quote) {
  // set up
  trace_init();

  cell_t *e = trace_ptr;
  trace_cur = ++trace_ptr;

  cell_t *module = get_module(string_seg(parent_entry->module_name));
  csize_t in = closure_in(quote) - 1;
  cell_t *c = quote->expr.arg[in];
  csize_t n = list_size(c);
  e->n = PERSISTENT;
  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(quote - parent_entry) - 1);
  e->entry.out = n;
  e->entry.len = 0;
  e->entry.flags = 0;
  e->func = func_exec;

  // free variables
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  for(cell_t *p = vl; p; p = p->tmp) {
    trace_store(p, p->value.type);
  }
  clean_tmp(vl);

  // compile
  set_trace(bc_trace);
  e->entry.in = in + fill_args(c, bc_arg);
  trace_reduce(c);
  drop(c);
  set_trace(NULL);
  e->entry.flags = 0;
  e->entry.len = trace_cnt;
  trace_final_pass(e);
#if DEBUG
  print_trace_index();
#endif

  cell_t **entry = cmap_get(&module, string_seg(e->word_name));
  *entry = e;

  return e;
}

cell_t *tref(cell_t *c) {
  intptr_t i = trace_decode(c);
  return i < 0 ? NULL : &trace_cur[i];
}

type_t trace_type(cell_t *c) {
  return is_value(c) ? c->value.type : c->expr_type;
}

void resolve_types(cell_t *c, type_t *t) {
  csize_t n = list_size(c);
  COUNTUP(i, n) {
    t[i] = trace_type(tref(c->value.ptr[i])) & T_EXCLUSIVE;
  }
  cell_t *p = tref(c->alt);
  while(p) {
    COUNTUP(i, n) {
      type_t pt = trace_type(tref(p->value.ptr[i])) & T_EXCLUSIVE;
      if(t[i] == T_BOTTOM) {
        *t = pt;
      } else if(t[i] != pt &&
                pt != T_BOTTOM) {
        *t = T_ANY;
      }
    }
    p = tref(p->alt);
  }
}

bool func_exec(cell_t **cp, UNUSED type_t t) {
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
  if(entry->entry.flags & ENTRY_NOINLINE) {
    csize_t c_in = closure_in(c), n = closure_args(c);
    alt_set_t alt_set = 0;
    for(csize_t i = 0; i < c_in - 1; ++i) {
      if(!reduce_arg(c, i, &alt_set, T_ANY)) goto fail;
    }
    for(csize_t i = c_in; i < n; ++i) {
      cell_t **d = &c->expr.arg[i];
      if(*d && is_dep(*d)) {
        cell_t *v = var(T_BOTTOM);
        v->value.alt_set = alt_set;
        store_reduced(d, v);
      }
    }

    cell_t *res = var(t == T_ANY ? T_BOTTOM : t);
    res->value.alt_set = alt_set;
    store_reduced(cp, res);
    return true;

  fail:
    fail(cp, t);
    return false;
  }

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
    if((trace_type(p) & T_EXCLUSIVE) == T_RETURN) {
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
    cell_t *t = map[i];
    if(!t) continue;

    // skip rewriting for the entry argument
    cell_t **t_entry = NULL;
    if(t->func == func_exec || t->func == func_quote) {
      t_entry = &t->expr.arg[closure_in(t) - 1];
      *t_entry = &trace_cells[trace_decode(*t_entry)];
    }

    traverse(t, {
        if(p != t_entry && *p) {
          uintptr_t x = trace_decode(*p);
          if(x < len) {
            *p = map[x];
          } else {
            *p = NULL;
          }
        }
      }, ARGS | PTRS | ALT);
  }

  // handle returns
  // TODO set alt_sets
  size_t
    out = closure_out(c),
    n = closure_args(c);
  cell_t **results[out + 1];
  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i];
  }

#define GET_RETURN_ARG(x) ref(map[trace_decode(returns->value.ptr[(x)])])

  // first one
  res = GET_RETURN_ARG(out);
  COUNTUP(i, out) {
    cell_t *d = c->expr.arg[n - 1 - i];
    d->func = func_id;
    d->expr.arg[0] = GET_RETURN_ARG(i);
    d->expr.arg[1] = 0;
  }

  // rest
  intptr_t next = trace_decode(returns->alt);
  while(next >= 0) {
    returns = &code[next];
    FOREACH(i, results) {
      cell_t *a = GET_RETURN_ARG(i);
      results[i] = &(*results[i])->alt;
      *results[i] = a ? id(a) : NULL;
    }
    next = trace_decode(returns->alt);
  }

#undef GET_RETURN_ARG

  // drop c from deps
  LOOP(out) {
    drop(c);
  }

  store_lazy(cp, c, res, 0);
  return false;
}

// very similar to get_name() but decodes entry
void trace_get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if((reduce_t *)clear_ptr(c->func) == func_exec) {
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c) - 1])];
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = function_name(c->func);
  }
}

// takes free variables and returns a quoted function
bool func_quote(cell_t **cp, UNUSED type_t t) {
  cell_t *c = clear_ptr(*cp);
  assert(is_closure(c));

  csize_t in = closure_in(c) - 1;
  cell_t *entry = c->expr.arg[in];
  c->expr.arg[in] = 0;
  csize_t
    f_in = entry->entry.in,
    f_out = entry->entry.out;

  cell_t *f = closure_alloc(f_in + f_out);
  f->n = f_out - 1;
  cell_t *l = make_list(f_out);
  csize_t offset = f_in - in;
  if(offset) {
    f->expr.arg[0] = (cell_t *)(uintptr_t)(offset - 1);
    f->func = (reduce_t *)mark_ptr(func_exec);
  } else {
    f->func = func_exec;
  }

  COUNTUP(i, in) {
    f->expr.arg[i + offset] = ref(c->expr.arg[i]);
  }

  f->expr.arg[f_in] = entry;

  COUNTUP(i, f_out - 1) {
    cell_t *d = dep(f);
    l->value.ptr[i] = d;
    f->expr.arg[f_in + 1 + i] = d;
  }

  l->value.ptr[f_out - 1] = f;

  store_reduced(cp, l);
  return true;
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
