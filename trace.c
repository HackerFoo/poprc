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
#include <inttypes.h>

#include "gen/error.h"
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

// storage for tracing
static cell_t trace_cells[1 << 16] __attribute__((aligned(64)));
static cell_t *trace_ptr = &trace_cells[0];
static cell_t *active_entries[1 << 6];
static int prev_entry_pos = 0;

const trace_cell_t nulltc = {NULL, 0};

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

#if INTERFACE
// use NIL_INDEX < -256
// so that is_offset() is false, otherwise problems with traverse/closure_next_child
#define NIL_INDEX (-4096)

// cell_t *c ranges from start to end
#define FOR_TRACE_0(c, e, n, ...)                             \
  for(cell_t                                                  \
        *_entry = (e),                                        \
        *c = _entry + 1 + (n),                                \
        *_end = c + _entry->entry.len;                        \
      c < _end;                                               \
      c += calculate_cells(c->size))
#define FOR_TRACE_1(c, e, ...) FOR_TRACE_0(c, e, 0)
#define FOR_TRACE(...) DISPATCH(FOR_TRACE, 3, __VA_ARGS__)
#endif

void trace_init() {
  prev_entry_pos = 0;
}

cell_t *get_entry(cell_t const *c) {
  if(!is_user_func(c)) return NULL;
  return &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
}

int entry_number(cell_t const *e) {
  return e - trace_cells;
}

// trace_encode/decode allow small integers to be encoded as pointers
// This avoids a reference to index 0 being treated as a missing argument
cell_t *trace_encode(int index) {
  return FLIP_PTR((cell_t *)(intptr_t)index);
}

int trace_decode(cell_t *c) {
  return (int)(intptr_t)FLIP_PTR(c);
}

// look through the trace for a matching value
static
int trace_lookup_value_linear(cell_t *entry, int type, val_t value) {
  FOR_TRACE(p, entry) {
    if(p->func == func_value &&
       NOT_FLAG(p->value.type, T_VAR) &&
       p->value.type.exclusive == type &&
       p->value.integer[0] == value)
      return p - entry;
  }
  return -1;
}

// find a matching trace cell given a variable or value
static
int trace_get_value(cell_t *entry, cell_t *r) {
  assert_error(r && is_value(r));
  if(is_list(r)) {
    return trace_build_quote(entry, r); // *** TODO prevent building duplicate quotes
  } else if(is_var(r)) {
    if(FLAG(r->value.type, T_DEP)) return -1;
    if(r->value.tc.entry != entry) {
      cell_t *tc = trace_cell_ptr(r->value.tc);
      tc->alt = r;
      r->value.tc = (trace_cell_t) {
        entry,
        trace_allocate_var(entry)
      };
      entry->entry.in++;
    }
    return r->value.tc.index;
  } else {
    int t = trace_lookup_value_linear(entry, r->value.type.exclusive, r->value.integer[0]);
    if(t) return t;
  }
  assert_error(false);
  return -1;
}

// reserve space in the trace
int trace_alloc(cell_t *entry, csize_t args) {
  if(!entry) return -1;
  int index = entry->entry.len + 1;
  size_t size = calculate_cells(args);
  cell_t *tc = trace_cell_ptr((trace_cell_t) {entry, index});
  entry->entry.len += size;
  tc->n = -1;
  tc->size = args;
  return index;
}

cell_t *trace_cell_ptr(trace_cell_t tc) {
  return tc.entry && tc.index > 0 ? &tc.entry[tc.index] : NULL;
}

// reduce allocated space in the trace
void trace_shrink(cell_t *t, csize_t args) {
  assert_error(args <= t->size);
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
int trace_copy(cell_t *entry, const cell_t *c) {
  int index = trace_alloc(entry, c->size);
  size_t n = closure_cells(c);
  memcpy(&entry[index], c, sizeof(cell_t) * n);
  return index;
}

void trace_write_graph(cell_t *c, trace_cell_t tc) {
  if(write_graph) {
    char label[64];
    const char
      *module_name = tc.entry->module_name,
      *word_name = tc.entry->word_name;

    snprintf(label, sizeof(label), "%s.%s [%d]",
             module_name,
             word_name,
             (int)tc.index);
    mark_cell(c);
    make_graph_all(NULL, label);
  }
}

// store expression c in the trace
static
void trace_store_expr(cell_t *c, const cell_t *r) {
  cell_t *entry = r->value.tc.entry;
  cell_t *tc = trace_cell_ptr(r->value.tc);
  if(!tc) return;
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
    return;
  }
  assert_error(tc->size == c->size);
  assert_error(c->func != func_dep_entered &&
         c->func != func_dep);
  LOG("trace_store_expr: %d[%d] <- %d %d",
      entry_number(entry), (int)r->value.tc.index,
      CELL_INDEX(c), CELL_INDEX(r));
  if(!is_value(c)) trace_write_graph(c, r->value.tc);

  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->n = n;
  if(is_user_func(tc)) {
    // encode the entry
    cell_t **e = &tc->expr.arg[closure_in(tc)];
    *e = trace_encode(entry_number(*e));
  }
  // encode inputs
  TRAVERSE(tc, in) {
    if(*p) {
      assert_error(!is_marked(*p));
      int x = trace_get_value(entry, *p);
      *p = trace_encode(x);
      if(x >= 0) entry[x].n++;
    }
  }
  // encode outputs
  TRAVERSE(tc, out) {
    if(*p) {
      int x = trace_get_value(entry, *p);
      *p = trace_encode(x);
    }
  }
  if(is_value(c)) {
    tc->value.alt_set = 0;
    tc->value.type = t;
  }
  tc->expr_type.exclusive = t.exclusive;
  if(tc->func == func_placeholder) FLAG_SET(tc->expr_type, T_INCOMPLETE);
  tc->alt = NULL;
}

// store value c in the trace
static
int trace_store_value(cell_t *entry, const cell_t *c) {
  if(!entry) return -1;
  assert_error(!is_list(c));

  // look to see if the value already is in the trace
  int x = trace_lookup_value_linear(entry, c->value.type.exclusive, c->value.integer[0]);
  if(x == -1) {
    x = trace_copy(entry, c);
    cell_t *tc = &entry[x];
    tc->value.alt_set = 0;
    tc->alt = NULL;
    tc->n = -1;
  }
  return x;
}

// store c which reduces to r in the trace
static
void trace_store(cell_t *c, const cell_t *r) {
  assert_error(is_var(r));
  if(FLAG(r->value.type, T_DEP)) {
    trace_dep(c);
  } else {
    trace_store_expr(c, r);
  }
}

// count the maximum number of changed variables in recursive calls
static
uint8_t trace_recursive_changes(cell_t *entry) {
  unsigned int changes = 0;
  const cell_t *encoded_entry = trace_encode(entry_number(entry));

  FOR_TRACE(p, entry) {
    csize_t in;
    if(p->func == func_exec &&
       p->expr.arg[in = closure_in(p)] == encoded_entry) {
      unsigned int cnt = 0;
      COUNTUP(i, in) {
        if(trace_decode(p->expr.arg[i]) != (trace_index_t)(in - 1 - i)) cnt++;
      }

      // if cnt == 0, a recusive call has been made without modifying any arguments
      // so it will loop forever
      assert_throw(cnt, "infinite recursion");
      if(cnt > changes) changes = cnt;
    }
  }
  return changes;
}

cell_t *get_trace_ptr(size_t size) {
  assert_error((void *)(trace_ptr + size) < (void *)(&trace_cells+1));
  return trace_ptr;
}

// setup for tracing
cell_t *trace_start_entry(csize_t in, csize_t out) {
  cell_t *e = get_trace_ptr(64);
  trace_ptr += 64; // TODO
  e->n = PERSISTENT;
  e->entry = (struct entry) {
    .in = in,
    .out = out
  };
  e->func = func_exec;
  insert_root(&e->entry.initial);

  // active_entries[e->entry.pos-1] = e
  active_entries[prev_entry_pos++] = e;
  e->entry.pos = prev_entry_pos;

  return e;
}

// finish tracing
void trace_end_entry(cell_t *e, cell_t *parent) {
  drop(e->entry.initial);
  remove_root(&e->entry.initial);
  clear_initial(e);
  FLAG_SET(e->entry, ENTRY_COMPLETE);
  e->entry.rec = trace_recursive_changes(e);
  e->entry.parent = parent;
}

void set_initial(cell_t *e, cell_t *initial) {
  assert_error(NOT_FLAG(e->entry, ENTRY_COMPLETE));
  // assert_error(e->entry.initial == NULL);
  e->entry.initial = initial;
  FLAG_SET(e->entry, ENTRY_INITIAL);
}

void clear_initial(cell_t *e) {
  assert_error(NOT_FLAG(e->entry, ENTRY_COMPLETE));
  e->entry.initial = NULL;
  FLAG_CLEAR(e->entry, ENTRY_INITIAL);
}

void trace_clear_alt(cell_t *entry) {
  FOR_TRACE(c, entry) {
    if(is_value(c) && c->value.type.exclusive == T_RETURN) continue;
    c->alt = 0;
  }
}

// called to update c in the trace
void trace_update(cell_t *c, cell_t *r) {
  if(is_list(r)) return;
  trace_store(c, r);
}

void trace_dep(cell_t *c) {
  if(!is_var(c)) return;
  if(NOT_FLAG(c->value.type, T_DEP)) return;
  cell_t *entry = c->value.tc.entry;
  if(!entry) return;
  int x = trace_alloc(entry, 1);
  cell_t *tc = &entry[x];
  cell_t *ph = trace_cell_ptr(c->value.tc);
  int ph_x = c->value.tc.index;
  ph->expr.arg[c->pos] = trace_encode(x);
  LOG("trace_dep: %d <- %d %d[%d]", (int)x, CELL_INDEX(c), ph_x, c->pos);
  tc->func = func_dep;
  tc->expr.arg[0] = trace_encode(ph_x);
  tc->expr_type.exclusive = c->value.type.exclusive;
  ph->n++;
  c->value.tc.index = x;
  FLAG_CLEAR(c->value.type, T_DEP);
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  trace_cell_t tc = r->value.tc;
  cell_t *c = trace_cell_ptr(tc);
  if(tc.entry && !c->func &&
     tc.entry->entry.len - (tc.index - 1) == calculate_cells(c->size)) {
    tc.entry->entry.len = tc.index - 1;
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
  cell_t *new_entry = trace_expr_entry(c->pos);
  if(!(is_var(r) || c->func == func_exec)) return;
  if(is_list(r)) {
    r = get_list_function_var(r);
    if(!r) return;
  }

  cell_t *entry = r->value.tc.entry;

  // make sure all input arguments are stored
  TRAVERSE(c, in) {
    cell_t *a = *p;
    if(is_value(a) && !is_var(a)) {
      if(is_list(a)) {
        //trace_build_quote(a);
      } else {
        trace_store_value(entry, a);
      }
    }
  }

  trace_store(c, r);
  if(new_entry) {
    cell_t *tc = trace_cell_ptr(r->value.tc);
    tc->alt = r;
    r->value.tc = (trace_cell_t) {
      new_entry,
      trace_allocate_var(new_entry)
    };
  }
}

// update the type of c in the trace
void trace_update_type(cell_t *c) {
  int t = c->value.type.exclusive;
  if(t != T_LIST) {
    cell_t *tc = trace_cell_ptr(c->value.tc);
    if(tc && tc->func) {
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
int trace_build_quote(cell_t *entry, cell_t *l) {
  assert_error(is_list(l));
  if(is_empty_list(l)) return NIL_INDEX;

  if(is_row_list(l) && // ***
     list_size(l) == 1) {
    cell_t *p = l->value.ptr[0];
    if(is_placeholder(p) &&
       closure_in(p) == 1 &&
       closure_out(p) == 0) p = p->expr.arg[0]; //***
    if(is_var(p) && is_function(p)) {
      // identity list, so just return the trace cell for the item in the list
      return p->value.tc.index;
    }
  }

  cell_t *vl = 0;
  trace_var_list(l, &vl);
  size_t in = tmp_list_length(vl);
  int x = trace_alloc(entry, in + 1);
  cell_t *n = &entry[x];

  LOG("build quote: %d[%d] <- %d", entry_number(entry), x, CELL_INDEX(l));

  n->expr.out = 0;
  n->func = func_exec;
  n->n = -1;
  n->expr_type.exclusive = T_LIST;
  FLAG_SET(n->expr_type, T_INCOMPLETE);

  cell_t *p = vl;
  COUNTUP(i, in) {
    trace_index_t x = trace_get_value(entry, p);
    entry[x].n++;
    n->expr.arg[i] = trace_encode(x);
    p = p->tmp;
  }

  clean_tmp(vl);

  n->expr.arg[in] = ref(l); // entry points to the quote for now
  insert_root(&n->expr.arg[in]);

  return x;
}

cell_t *trace_quote_var(cell_t *l) {
  cell_t *f = *leftmost_row(&l);
  assert_error(is_var(f));
  cell_t *entry = f->value.tc.entry;
  int x = trace_build_quote(entry, l);
  return x == NIL_INDEX ? &nil_cell : var_create_nonlist(T_FUNCTION, (trace_cell_t) {entry, x});
}

// store a return
static
int trace_return(cell_t *entry, cell_t *c) {
  cell_t *c0 = c;
  c = flat_copy(c);
  cell_t **p;
  FORLIST(p, c, true) {
    trace_index_t x;
    if(is_var(*p)) {
      x = (*p)->value.tc.index;
    } else if(is_list(*p)) {
      x = trace_build_quote(entry, *p);
    } else {
      x = trace_store_value(entry, *p);
    }
    *p = trace_encode(x);
    if(x >= 0) entry[x].n++;
  }
  int x = trace_copy(entry, c);
  trace_cell_t t = {entry, x};
  trace_write_graph(c0, t);
  closure_free(c);
  cell_t *tc = trace_cell_ptr(t);
  tc->value.type.exclusive = T_RETURN;
  tc->n = -1;
  tc->alt = NULL;
  return x;
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
  cell_t *c = parse_expr(&p, NULL, NULL);
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  size_t n = tmp_list_length(vl);
  printf("length(vl) = %d\n", (int)n);
  clean_tmp(vl);
  drop(c);
  return n == 5 ? 0 : -1;
}

// HACK to make convert tail calls with type T_ANY to T_BOTTOM
bool tail_call_to_bottom(cell_t *entry, int x) {
  if(x < 0) return false;
  cell_t *tc = &entry[x];
  bool is_assert = tc->func == func_assert;
  if((is_assert || (tc->func == func_exec &&
                    trace_decode(tc->expr.arg[closure_in(tc)]) == (int)entry->entry.len-1)) &&
     tc->expr_type.exclusive == T_ANY) {
    if(is_assert) {
      if(tail_call_to_bottom(entry, trace_decode(tc->expr.arg[0]))) {
        tc->expr_type.exclusive = T_BOTTOM;
        return true;
      }
    } else {
      tc->expr_type.exclusive = T_BOTTOM;
      return true;
    }
  }
  return false;
}

// reduce for tracing & compilation
unsigned int trace_reduce(cell_t *entry, cell_t **cp) {
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;

  insert_root(cp);

  cell_t **p = cp;
  while(*p) {
    if(!func_list(p, REQ(return))) continue;
    cell_t **a;
    FORLIST(a, *p, true) {
      collapse_row(a);
      reduce(a, REQ(any)); // ***
      if(is_value(*a) &&
         !is_list(*a) &&
         !is_var(*a)) { // TODO deps?
        trace_store_value(entry, *a);
      }
    }
    int x = trace_return(entry, *p);
    cell_t *r = &entry[x];
    COUNTUP(i, list_size(r)) {
      tail_call_to_bottom(entry, trace_decode(r->value.ptr[i]));
    }
    r->n++;
    *prev = trace_encode(x);
    alts++;
    p = &(*p)->alt;
    prev = &r->alt;
  }

  remove_root(cp);
  return alts;
}

int trace_allocate_var(cell_t *entry) {
  int x = trace_alloc(entry, 2);
  cell_t *tc = &entry[x];
  tc->func = func_value;
  tc->value.type.flags = T_VAR;
  return x;
}

void trace_allocate_vars(cell_t *entry, csize_t n) {
  LOOP(n) {
    trace_allocate_var(entry);
  }
}

cell_t *trace_expr_entry(uint8_t pos) {
  if(pos == 0) return NULL;
  assert_error(pos <= prev_entry_pos);
  return active_entries[pos - 1];
}
