/* Copyright 2012-2017 Dustin DeWeese
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
#include <string.h>
#include <assert.h>
#include <inttypes.h>

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/parse.h"
#include "gen/print.h"
#include "gen/lex.h"
#include "gen/user_func.h"
#include "gen/list.h"
#include "gen/log.h"
#include "gen/trace.h"

// trace cells are allocated only when this is true
bool trace_enabled = false;

bool dont_specialize = true; //false; ***

// storage for tracing
cell_t trace_cells[1 << 10] __attribute__((aligned(64)));
cell_t *trace_cur = &trace_cells[0];
cell_t *trace_ptr = &trace_cells[0];

// word to match for tail recursion
cell_t *initial_word = NULL;

// return value when disabling the trace
cell_t *return_me = NULL;

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

#if INTERFACE
// use NIL_INDEX < -256
// so that is_offset() is false, otherwise problems with traverse/closure_next_child
#define NIL_INDEX (-4096)

// cell_t *c ranges from start to end
#define FOR_TRACE(c, start, end) for(cell_t *(c) = (start); c < (end); c += calculate_cells(c->size))
#endif

cell_t *get_entry(cell_t *c) {
  if(!is_user_func(c)) return NULL;
  return &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
}

// trace_encode/decode allow small integers to be encoded as pointers
// This avoids a reference to trace_cur[0] being treated as a missing argument
cell_t *trace_encode(trace_index_t index) {
  return FLIP_PTR((cell_t *)index);
}

trace_index_t trace_decode(cell_t *c) {
  return (trace_index_t)FLIP_PTR(c);
}

// get the trace cell given a variable
static
cell_t *trace_get(const cell_t *r) {
  assert(r && is_var(r));
  cell_t *tc = r->value.ptr[0];
  assert(tc >= trace_cur && tc < trace_ptr);
  return tc;
}

// look through the trace for a matching value
static
cell_t *trace_lookup_value_linear(int type, val_t value) {
  FOR_TRACE(p, trace_cur, trace_ptr) {
    if(p->func == func_value &&
       !(p->value.type.flags & T_VAR) &&
       p->value.type.exclusive == type &&
       p->value.integer[0] == value)
      return p;
  }
  return NULL;
}

// find a matching trace cell given a variable or value
static
trace_index_t trace_get_value(cell_t *r) {
  assert(r && is_value(r));
  if(is_list(r)) {
    return trace_build_quote(r); // *** TODO prevent building duplicate quotes
  } else if(is_var(r)) {
    if(r->value.type.flags & T_DEP) return -1;
    return trace_get(r) - trace_cur;
  } else {
    cell_t *t = trace_lookup_value_linear(r->value.type.exclusive, r->value.integer[0]);
    if(t) return t - trace_cur;
  }
  assert(false);
  return -1;
}

// reserve space in the trace
cell_t *trace_alloc(csize_t args) {
  if(!trace_enabled) return NULL;
  size_t size = calculate_cells(args);
  cell_t *tc = trace_ptr;
  tc->n = -1;
  trace_ptr += size;
  tc->size = args;
  return tc;
}

#if SPECIALIZE
static
csize_t count_vars(cell_t *c) {
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  size_t vars = tmp_list_length(vl);
  clean_tmp(vl);
  return vars;
}
#endif

// reserve space for specialized c
cell_t *trace_var_specialized(uint8_t t, cell_t *c) {
#if SPECIALIZE
  return var_create(t, trace_alloc(count_vars(c) + 1 + trace_cur[-1].entry.out), 0, 0);
#else
  (void)t, (void)c;
  assert(false);
  return NULL;
#endif
}

// reduce allocated space in the trace
void trace_shrink(cell_t *t, csize_t args) {
  assert(args <= t->size);
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

// copy c into newly allocated space in the trace
static
cell_t *trace_copy(const cell_t *c) {
  cell_t *tc = trace_alloc(c->size);
  size_t size = closure_cells(c);
  memcpy(tc, c, sizeof(cell_t) * size);
  return tc;
}

// store expression c in the trace
static
cell_t *trace_store_expr(const cell_t *c, const cell_t *r) {
  cell_t *tc = trace_get(r);
  if(!tc) return NULL;
  type_t t = r->value.type;
  if(tc->func) {
    // this cell has already been written
    // update the types
    if(t.exclusive != T_ANY) {
      if(is_value(tc)) {
        tc->value.type = t;
      } else if(is_dep(tc)) {
        tc->expr_type.exclusive = t.exclusive;
      }
    }
    return tc;
  }
  assert(tc->size == c->size);
  assert(c->func != func_dep_entered &&
         c->func != func_dep);
  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->n = n;
  if(is_user_func(tc)) {
    // encode the entry
    cell_t **e = &tc->expr.arg[closure_in(tc)];
    *e = trace_encode(*e - trace_cells);
  }
  // encode inputs
  TRAVERSE(tc, in) {
    if(*p) {
      assert(!is_marked(*p));
      trace_index_t x = trace_get_value(*p);
      *p = trace_encode(x);
      trace_cur[x].n++;
    }
  }
  // encode outputs
  TRAVERSE(tc, out) {
    if(*p) {
      trace_index_t x = trace_get_value(*p);
      *p = trace_encode(x);
    }
  }
  if(is_value(c)) {
    tc->value.alt_set = 0;
    tc->value.type = t;
  }
  tc->expr_type.exclusive = t.exclusive;
  if(tc->func == func_fcompose) tc->func = func_compose; // fcompose -> compose
  if(tc->func == func_placeholder) tc->expr_type.flags |= T_INCOMPLETE;
  tc->alt = NULL;
  if(t.exclusive == T_BOTTOM) return_me = tc;
  return tc;
}

// store value c in the trace
static
cell_t *trace_store_value(const cell_t *c) {
  assert(!is_list(c));

  // look to see if the value already is in the trace
  cell_t *tc = trace_lookup_value_linear(c->value.type.exclusive, c->value.integer[0]);
  if(!tc && trace_enabled) {
    tc = trace_copy(c);
    tc->value.alt_set = 0;
    tc->alt = NULL;
    tc->n = -1;
  }
  return tc;
}

// can c be specialized?
#if SPECIALIZE
static
bool trace_match_specialize(const cell_t *c) {
  if(dont_specialize || c->func != func_exec) return false;
  TRAVERSE((cell_t *)c, in) {
    if(*p && is_list(*p)) return true;
  }
  return false;
}
#endif

// store c which reduces to r in the trace
static
cell_t *trace_store(cell_t *c, const cell_t *r) {
  if(is_var(r)) {
    if(!r->value.ptr[0]) {
      return return_me;
    } else if (r->value.type.flags & T_DEP) {
      return trace_dep(c);
#if SPECIALIZE
    } else if(trace_match_specialize(c)) {
      return trace_build_specialized(c, r);
#endif
    } else {
      return trace_store_expr(c, r);
    }
  } else {
    return trace_store_value(c);
  }
}

// setup for tracing
cell_t *trace_start() {
  trace_enabled = true;
  initial_word = NULL;
  insert_root(&initial_word);
  cell_t *e = trace_alloc(1);
  trace_cur = trace_ptr;
  return e;
}

// finish tracing
void trace_stop() {
  drop(initial_word);
  remove_root(&initial_word);
  initial_word = NULL;
  trace_enabled = false;
}

void trace_clear_alt(cell_t *e) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;
  FOR_TRACE(c, start, end) {
    if(is_value(c) && c->value.type.exclusive == T_RETURN) continue;
    c->alt = 0;
  }
}

// called to update c in the trace
void trace_update(cell_t *c, cell_t *r) {
  if(!trace_enabled) return;
  if(is_list(r)) return;

  trace_store(c, r);
}

cell_t *trace_dep(cell_t *c) {
  if(!trace_enabled || !is_var(c)) return NULL;
  if(!(c->value.type.flags & T_DEP)) return c->value.ptr[0];
  cell_t *tc = trace_alloc(1);
  cell_t *ph = trace_get(c);
  assert(ph != tc);
  ph->expr.arg[c->value.integer[1]] = trace_encode(tc - trace_cur);
  tc->func = func_dep;
  tc->expr.arg[0] = trace_encode(ph - trace_cur);
  tc->expr_type.exclusive = c->value.type.exclusive;
  ph->n++;
  c->value.ptr[0] = tc;
  FLAG_CLEAR(c->value.type.flags, T_DEP);
  return tc;
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  cell_t *tc = r->value.ptr[0];
  if(tc && !tc->func && trace_ptr - tc == calculate_cells(tc->size)) {
    trace_ptr = tc;
  }
}

// find the function variable in a list
cell_t *get_list_function_var(cell_t *c) {
  cell_t *left = *leftmost(&c);
       if(!left)                return NULL;
  else if(is_function(left))    return left;
  else if(is_placeholder(left)) return left->expr.arg[closure_in(left) - 1];
  else                          return NULL;
}

// called when c is reduced to r to copy to pre-allocated space in the trace
void trace_reduction(cell_t *c, cell_t *r) {
  //if(!trace_enabled) return;

  if(!(is_var(r) || c->func == func_exec)) return;
  if(is_list(r)) {
    r = get_list_function_var(r);
    if(!r) return;
  }

  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }

  // make sure all input arguments are stored
  TRAVERSE(c, in) {
    cell_t *a = *p;
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

// update the type of c in the trace
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

// zero space in the trace allocated to an entry
void trace_clear(cell_t *e) {
  size_t count = e->entry.len;
  memset(e, 0, (count + 1) * sizeof(cell_t));
}

// update the traced type
void trace_set_type(cell_t *tc, int t) {
  tc->expr_type.exclusive = t;
  if(is_value(tc)) {
    tc->value.type.exclusive = t;
  }
}

// store captured variables to be compiled into a quote
trace_index_t trace_build_quote(cell_t *l) {
  assert(is_list(l));
  if(is_empty_list(l)) return NIL_INDEX;

  if(is_row_list(l) && // ***
     list_size(l) == 1) {
    cell_t *p = l->value.ptr[0];
    if(is_placeholder(p) &&
       closure_in(p) == 1 &&
       closure_out(p) == 0) p = p->expr.arg[0]; //***
    if(is_var(p) && is_function(p)) {
      // identity list, so just return the trace cell for the item in the list
      return trace_get(p) - trace_cur;
    }
  }

  cell_t *vl = 0;
  trace_var_list(l, &vl);
  size_t in = tmp_list_length(vl);
  cell_t *n = trace_alloc(in + 1);

  n->expr.out = 0;
  n->func = func_quote;
  FLAG_SET(n->expr.flags, FLAGS_USER_FUNC);
  n->n = -1;
  n->expr_type.exclusive = T_LIST;
  n->expr_type.flags |= T_INCOMPLETE;

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

cell_t *trace_quote_var(cell_t *l) {
  trace_index_t x = trace_build_quote(l);
  return x == NIL_INDEX ? &nil_cell : var_create_nonlist(T_FUNCTION, &trace_cur[x]);
}

// store captured variables to be compiled into a specialized function, similar to trace_build_quote
#if SPECIALIZE
cell_t *trace_build_specialized(cell_t *c, const cell_t *r) {
  assert(c->func == func_exec);
  //assert(c->expr.flags & FLAGS_RECURSIVE == 0);

  cell_t *vl = 0;
  trace_var_list(c, &vl);
  csize_t
    in = tmp_list_length(vl),
    c_in = closure_in(c),
    out = closure_out(c);
  cell_t *n = trace_get(r);

  n->expr.out = out;
  n->func = func_exec;
  FLAG_SET(n->expr.flags, FLAGS_USER_FUNC);
  n->n = -1;
  n->expr_type = r->value.type;
  n->expr_type.flags |= T_INCOMPLETE;

  cell_t *p = vl;
  COUNTUP(i, in) {
    trace_index_t x = trace_get_value(p);
    trace_cur[x].n++;
    n->expr.arg[i] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  c = copy(c);
  FLAG_CLEAR(c->expr.flags, FLAGS_RECURSIVE);
  c->alt = 0;
  TRAVERSE_REF(c, args);
  n->expr.arg[in] = c;
  insert_root(&n->expr.arg[in]);

  COUNTUP(i, out) { // ***
    trace_index_t x = trace_get_value(c->expr.arg[c_in + i]);
    n->expr.arg[in + i + 1] = trace_encode(x);
  }

  return n;
}
#endif

// store a return
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
    if(x >= 0) trace_cur[x].n++;
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
      TRAVERSE(c, alt, in, ptrs) {
        tail = trace_var_list(*p, tail);
      }
      c->tmp = 0;
    }
  }
  return tail;
}

size_t tmp_list_length(cell_t *c) {
  size_t n = 0;
  FOLLOW(p, c, tmp) {
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

// reduce for tracing & compilation
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
      reduce(a, req_simple(T_ANY)); // ***
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

void trace_allocate_vars(csize_t n) {
  COUNTUP(i, n) {
    cell_t *tc = &trace_cur[i];
    tc->size = 2;
    tc->func = func_value;
    tc->value.type.flags = T_VAR;
    tc->n = -1;
  }
  trace_ptr += n;
}
