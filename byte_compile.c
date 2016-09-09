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
static MAP(trace_index, 1 << 10); // cell_t * -> intptr_t (index in trace)
static MAP(trace_values, 1 << 6); // intptr_t (value) -> intptr_t (index in trace)

#define DEBUG 0

#define NIL_INDEX (-3)

#if INTERFACE
#define FOR_TRACE(c, start, end) for(cell_t *(c) = (start); c < (end); c = closure_next(c))
#endif

static
pair_t *trace_find(const cell_t *c) {
  c = clear_ptr(c);
  /*
  if(is_list(c) && c->value.ptr[0] && is_placeholder(c->value.ptr[0])) {
    cell_t *ph = clear_ptr(c->value.ptr[0]);
    pair_t *res = map_find(trace_index, (intptr_t)ph);
    if(res) return res;
  }
  */
  return map_find(trace_index, (intptr_t)c);
}

static
intptr_t trace_get(cell_t *c) {
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

void trace_index_add(const cell_t *c, intptr_t x) {
  pair_t p = {(intptr_t)c, x};
  map_insert(trace_index, p);
}

void trace_index_assign(cell_t *new, cell_t *old) {
  pair_t *p = trace_find(old);
  if(p) {
    trace_index_add(new, p->second);
  }
}

cell_t *trace_encode(intptr_t index) {
  return FLIP_PTR((cell_t *)index);
}

intptr_t trace_decode(cell_t *c) {
  return (intptr_t)FLIP_PTR(c);
}

static
intptr_t map_update(map_t map, intptr_t key, intptr_t new_value) {
  pair_t *e = map_find(map, key);
  assert(e);
  intptr_t old_value = e->second;
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
  csize_t size;
  trace_cnt++;
  if(is_placeholder(c)) {
    csize_t
      in = closure_in(c),
      out = closure_out(c);
    size = calculate_cells(closure_args(c) + 1);
    memcpy(dest, c, (char *)&c->expr.arg[in] - (char *)c);
    dest->size++;
    dest->expr.arg[in] = (cell_t *)c;
    memcpy(&dest->expr.arg[in + 1], &c->expr.arg[in + 1], sizeof(cell_t *) * out);
    if(is_value(c)) dest->value.type = (dest->value.type & ~T_EXCLUSIVE) | t;
    dest->expr_type = (dest->expr_type & ~T_EXCLUSIVE) | T_TRACED | t;
    trace_rewrite(dest);
  } else {
    size = closure_cells(c);
    memcpy(dest, c, sizeof(cell_t) * size);
    if(is_value(c)) dest->value.type = (dest->value.type & ~T_EXCLUSIVE) | t;
    dest->expr_type = (dest->expr_type & ~T_EXCLUSIVE) | T_TRACED | t;
  }

  trace_ptr += size;
  if(c->func != func_assert) dest->alt = NULL;
  trace_index_add(c, dest - trace_cur);
  if(is_value(c) && (c->value.type & T_EXCLUSIVE) == T_INT) {
    pair_t p = {c->value.integer[0], dest - trace_cur};
    map_insert(trace_values, p);
  }

  dest->n = -1;
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

cell_t *trace_var(cell_t *c, type_t t) {
  cell_t *dest = trace_ptr++;
  trace_cnt++;
  trace_index_add(c, dest - trace_cur);
  dest->func = func_value;
  dest->size = 1;
  dest->n = -1;
  dest->value.type = t | T_VAR;

  // stuff type in expr_type
  dest->expr_type = t | T_VAR | T_TRACED;
  return dest;
}

void trace_rewrite(cell_t *c) {

  // skip returns (already rewritten)
  if((c->expr_type & T_TRACED) == 0) return;

  // skip rewriting for the entry argument
  cell_t **entry = NULL;
  if(c->func == func_exec) {
    entry = &c->expr.arg[closure_in(c) - 1];
    *entry = trace_encode(*entry - trace_cells);
  }

  traverse(c, {
      if(p != entry && *p) {
        if(!trace_find(*p)) return;
      }
    }, ARGS | PTRS | ALT);

  traverse(c, {
      if(p != entry && *p) {
        intptr_t x = trace_get(*p);
        *p = trace_encode(x);
        trace_cur[x].n++;
      }
    }, ARGS | PTRS | ALT);

  c->expr_type &= ~T_TRACED;
}

void trace_move_rewrite(cell_t *c, intptr_t from, intptr_t back) {

  // skip rewriting for the entry argument
  cell_t **entry = NULL;
  if(c->func == func_exec ||
     c->func == func_quote) {
    entry = &c->expr.arg[closure_in(c) - 1];
  }

  traverse(c, {
      if(p != entry && *p) {
        if(trace_decode(*p) > from) *(intptr_t *)p += back;
      }
    }, ARGS | PTRS | ALT);
}

/*
size_t trace_delete(cell_t *e, cell_t *c) {
  cell_t *start = e + 1, *end = start + e->entry.len;
  intptr_t x = c - start;
  intptr_t s = closure_cells(c);
  cell_t *n = c + s;
  memmove(c, n, (char *)end - (char *)n);
  end -= s;
  e->entry.len -= s;
  FOR_TRACE(p, c, end) {
    trace_move_rewrite(p, x, s);
  }
  return s;
}

void trace_cleanup(cell_t *e) {
  cell_t *start = e + 1, *end = start + e->entry.len;
  cell_t *p = start;
  while(p < end) {
    if(p->expr_type & T_TRACED) {
      end -= trace_delete(e, p);
    } else p += closure_cells(p);
  }
}
*/

cell_t *trace_select(const cell_t *c, cell_t *a) {
  cell_t *dest = trace_ptr;
  csize_t size = closure_cells(c);
  trace_ptr += size;
  trace_cnt++;

  intptr_t tc = map_update(trace_index, (intptr_t)clear_ptr(c), dest - trace_cur);
  intptr_t ta = trace_get(a);

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
    t->value.type = (t->value.type & ~T_EXCLUSIVE) | (ct & T_EXCLUSIVE);
  }

  // also stuff type in expr_type
  t->expr_type = (t->expr_type & ~T_EXCLUSIVE) | (ct & T_EXCLUSIVE);
}

void trace_init() {
  trace_cur = trace_ptr;
  trace_cnt = 0;
  map_clear(trace_index);
  map_clear(trace_values);
}

#define PRINT_DECODE(x) (rewritten ? trace_decode(x) : (x) - cells)
void print_bytecode(cell_t *e) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;
  printf("___ %s.%s (%d -> %d) ___\n", e->module_name, e->word_name, e->entry.in, e->entry.out);
  FOR_TRACE(c, start, end) {
    int t = c - start;
    bool rewritten = (c->expr_type & T_TRACED) == 0;
    if(!rewritten) printf("-- ");
    printf("[%d]", t);
    if(is_value(c)) {
      if(is_var(c)) {
        printf(" var");
      } else if(is_list(c) || c->value.type == T_RETURN) {
        if(c->value.type == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %" PRIdPTR, PRINT_DECODE(c->value.ptr[i]));
        }
        printf(" ]");
      } else {
        printf(" val %" PRIdPTR, c->value.integer[0]);
      }
      printf(", type = %s", show_type_all_short(c->value.type));
      if(c->alt) printf(" -> %" PRIdPTR, PRINT_DECODE(c->alt));
    } else {
      const char *module_name = NULL, *word_name = NULL;
      trace_get_name(c, &module_name, &word_name);
      if(c->func == func_quote) printf(" quote");
      printf(" %s.%s", module_name, word_name);
      cell_t **e = (c->func == func_exec || c->func == func_quote) ? &c->expr.arg[closure_in(c) - 1] : NULL;
      traverse(c, {
          if(p != e) printf(" %" PRIdPTR, PRINT_DECODE(*p));
        }, ARGS);
      printf(", type = %s", show_type_all_short(c->expr_type));
      if(c->alt) printf(" -> %" PRIdPTR, PRINT_DECODE(c->alt));
    }
#if DEBUG
    pair_t *p = map_find_value(trace_index, t);
    if(p) {
      printf(" (%d)", (int)(((cell_t *)p->first) - cells));
    }
#endif
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

intptr_t bc_func(reduce_t f, csize_t in, csize_t out, ...) {
  assert(out > 0);
  va_list argp;
  csize_t args = in + out - 1;
  cell_t *c = trace_alloc(args);
  c->expr.out = out - 1;
  c->func = f;
  c->n = -1;

  va_start(argp, out);

  COUNTUP(i, in) {
    intptr_t x = va_arg(argp, intptr_t);
    trace_cur[x].n++;
    c->expr.arg[i] = trace_encode(x);
  }

  COUNTUP(i, out - 1) {
    intptr_t d = bc_func(func_dep, 1, 1, c - trace_cur);
    intptr_t *res = va_arg(argp, intptr_t *);
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
    if(c->func == func_dep ||
       c->func == func_placeholder) break;

    r->value.type |= T_TRACED;
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
    } else if(c->func == func_pushl) {
      trace_store_pushl(c);
    } else {
      csize_t in = closure_in(c);
      csize_t out = closure_out(c);
      COUNTUP(i, c->func == func_exec ? in - 1 : in) {
        cell_t *a = c->expr.arg[i];
        if(is_value(a) && !(a->value.type & T_TRACED) && !is_var(a)) {
          trace_store(a, a->value.type);
        }
      }
      COUNTUP(i, out) {
        cell_t *d = c->expr.arg[in + i];
        if(d) trace_dep(d, c, d->value.type);
      }
      trace_store(c, r->value.type);
    }
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
      trace_store_list(c);
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
    intptr_t
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
  }
  //trace_cleanup(e);

  FOR_TRACE(p, start, end) {
    if(p->func == func_quote) {
      cell_t *qe = compile_quote(e, p);
      if(qe) {
        p->expr.arg[closure_in(p) - 1] = trace_encode(qe - trace_cells);
      } else {
        p->size = 2;
        p->func = func_pushl;
      }
    }
  }
}

void bc_arg(cell_t *c, UNUSED val_t x) {
  trace_store(c, T_ANY);
  c->value.type |= T_TRACED;
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
cell_t *trace_store_list(cell_t *c) {
  csize_t n = list_size(c);
  intptr_t li = is_placeholder(c->value.ptr[n-1]) ? trace_get(c->value.ptr[n-1]) : NIL_INDEX;
  COUNTUP(i, n) {
    li = trace_build_quote(c->value.ptr[i], li) - trace_cur;
  }
  trace_index_add(c, li);
  return &trace_cur[li];
}

cell_t *trace_store_pushl(cell_t *c) {
  cell_t *n = trace_build_quote(c->expr.arg[0], trace_get(c->expr.arg[1]));
  trace_index_add(c, n - trace_cur);
  return n;
}

cell_t *trace_build_quote(cell_t *q, intptr_t li) {
  cell_t *vl = 0;
  cell_t **vlp = &vl;

  vlp = trace_var_list(q, vlp);
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 2);

  n->expr.out = 0;
  n->func = func_quote;
  n->n = -1;
  n->expr_type = T_LIST;

  cell_t *p = vl;
  COUNTUP(i, in) {
    intptr_t x = trace_get(p);
    trace_cur[x].n++;
    n->expr.arg[in - i - 1] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  if(li >= 0) trace_cur[li].n++;
  n->expr.arg[in] = trace_encode(li);
  n->expr.arg[in + 1] = ref(q); // entry points to the quote for now

  return n;
}

/*
cell_t *trace_store_quote(cell_t *c) {
  if(list_size(c) == 0) {
    return trace_store(c, c->value.type);
  }
  cell_t *vl = 0;
  cell_t **vlp = &vl;
  cell_t *left = c->value.ptr[list_size(c) - 1];

  // treat placeholder as a free variable
  vlp = trace_var_list(c, vlp);
  if(is_placeholder(left)) {
    LIST_ADD(tmp, vlp, left);
  }
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 1);

  n->expr.out = 0;
  n->func = func_quote;
  n->n = -1;

  cell_t *p = vl;
  COUNTUP(i, in) {
    intptr_t x = trace_get(p);
    trace_cur[x].n++;
    n->expr.arg[in - i - 1] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  n->expr.arg[in] = ref(c); // entry is c for now

  trace_index_add(c, n - trace_cur);
  return n;
}
*/

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp) {
    if(is_var(c)) {
      LIST_ADD(tmp, tail, c);
    } else {
      c->tmp = FLIP_PTR(c->tmp); // prevent loops
      traverse(c, {
          tail = trace_var_list(*p, tail);
        }, PTRS | ARGS_IN);
      c->tmp = FLIP_PTR(c->tmp);
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
    trace(c->value.ptr[i], c, tt_force, i);
  }
  if(!any_conflicts((cell_t const *const *)c->value.ptr, n)) {
    r = trace_store(c, T_LIST);
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
      trace(c->value.ptr[i], c, tt_force, i);
    }
    if(!any_conflicts((cell_t const *const *)p->value.ptr, n)) {
      cell_t *prev = r;
      r = trace_store(p, T_LIST);
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
cell_t *compile_quote(cell_t *parent_entry, cell_t *q) {
  // set up
  trace_init();

  cell_t *e = trace_ptr;
  trace_cur = ++trace_ptr;

  csize_t in = closure_in(q) - 1;
  cell_t *c = quote(q->expr.arg[in]);
  e->n = PERSISTENT;
  e->entry.out = 1;
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
  e->entry.in = in + fill_args(c, bc_arg) - 1;
  trace_reduce(c);
  drop(c);
  set_trace(NULL);
  e->entry.flags = 0;
  e->entry.len = trace_cnt;
  trace_final_pass(e);
  if(is_id(e)) {
    trace_ptr = trace_cur; // reset
    return NULL;
  }

  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(q - parent_entry) - 1);

#if DEBUG
  print_trace_index();
#endif

  cell_t *module = get_module(string_seg(parent_entry->module_name));
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
        if(p != t_entry) {
          intptr_t x = trace_decode(*p);
          if(x == NIL_INDEX) {
            *p = &nil_cell;
          } else if(x >= 0 && x < (int)len) {
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

#define GET_RETURN_ARG(x) map[trace_decode(returns->value.ptr[(x)])]

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
bool func_quote(cell_t **cp, UNUSED type_t t) {
  cell_t *c = clear_ptr(*cp);
  assert(is_closure(c));

  csize_t in = closure_in(c) - 2;
  cell_t *entry = c->expr.arg[in + 1];
  c->expr.arg[in + 1] = 0;
  csize_t f_in = entry->entry.in;

  cell_t *f = closure_alloc(f_in + 1);
  csize_t offset = f_in - in;
  if(offset) {
    f->expr.arg[0] = (cell_t *)(intptr_t)(offset - 1);
    f->func = (reduce_t *)mark_ptr(func_exec);
  } else {
    f->func = func_exec;
  }

  COUNTUP(i, in) {
    f->expr.arg[i + offset] = ref(c->expr.arg[i]);
  }

  f->expr.arg[f_in] = entry;

  cell_t *res = closure_alloc(2);
  res->func = func_pushl;
  res->expr.arg[0] = f;
  res->expr.arg[1] = ref(c->expr.arg[in]);

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
