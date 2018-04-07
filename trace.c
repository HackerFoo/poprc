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

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "eval.h"
#include "primitive.h"
#include "special.h"
#include "parse.h"
#include "print.h"
#include "lex.h"
#include "user_func.h"
#include "list.h"
#include "trace.h"
#include "byte_compile.h"

// storage for tracing
static cell_t trace_cells[1 << 16] __attribute__((aligned(64)));
static cell_t *trace_ptr = &trace_cells[0];
static cell_t *active_entries[1 << 6];
static int prev_entry_pos = 0;

const trace_cell_t nulltc = {NULL, 0, 0};

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

#if INTERFACE
// use NIL_INDEX < -256
// so that is_offset() is false, otherwise problems with traverse/closure_next_child
#define NIL_INDEX (-4096)

// cell_t *c ranges from start to end
#define FOR_TRACE_3(c, e, n)                                  \
  for(cell_t                                                  \
        *_entry = (e),                                        \
        *c = _entry + 1 + (n);                                \
      c - _entry - 1 < _entry->entry.len;                     \
      c += calculate_cells(c->size))
#define FOR_TRACE_2(c, e) FOR_TRACE_3(c, e, 0)
#define FOR_TRACE(...) DISPATCH(FOR_TRACE, __VA_ARGS__)
#endif

bool is_trace_cell(void const *p) {
  return p >= (void *)&trace_cells && p < (void *)(&trace_cells+1);
}

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

cell_t *entry_from_number(int n) {
  assert_throw(n >= 0 &&
               n % 64 == 0 &&
               n < trace_ptr - trace_cells,
               "invalid entry number");
  return &trace_cells[n];
}

// trace_encode/decode allow small integers to be encoded as pointers
// This avoids a reference to index 0 being treated as a missing argument
cell_t *trace_encode(int index) {
  return FLIP_PTR((cell_t *)(intptr_t)index);
}

int trace_decode(cell_t *c) {
  return (int)(intptr_t)FLIP_PTR(c);
}

bool equal_value(const cell_t *a, const cell_t *b) {
  int type = a->value.type;
  if(b->value.type != type) return false;
  switch(type) {
  case T_INT:
  case T_SYMBOL:
    return a->value.integer == b->value.integer;
  case T_FLOAT:
    return a->value.flt == b->value.flt;
  default:
    return false;
  }
}

// look through the trace for a matching value
static
int trace_lookup_value_linear(cell_t *entry, const cell_t *c) {
  FOR_TRACE(p, entry) {
    if(p->op == OP_value &&
       NOT_FLAG(p->value, VALUE_VAR) &&
       equal_value(p, c))
      return p - entry;
  }
  return -1;
}

bool is_ancestor_of(cell_t *ancestor, cell_t *entry) {
  while(entry) {
    if(entry == ancestor) {
      return true;
    } else {
      entry = entry->entry.parent;
    }
  }
  return false;
}

static
void switch_entry_(cell_t *entry, trace_cell_t *tc) {
  if(tc->entry != entry->entry.parent) {
    switch_entry_(entry->entry.parent, tc);
  }
  trace_cell_t old = *tc;
  // a little hacky because ideally variables shouldn't be duplicated
  // see TODO in func_value
  FOR_TRACE(c, entry) {
    if(is_var(c) &&
       c->value.tc.entry == old.entry &&
       c->value.tc.index == old.index) {
      *tc = (trace_cell_t) {
        entry,
        c-entry,
        0
      };
      goto end;
    }
  }
  *tc = (trace_cell_t) {
    entry,
    trace_alloc_var(entry),
    0
  };
  cell_t *p = trace_cell_ptr(*tc);
  p->value.tc = old;
  p->value.type = trace_type(trace_cell_ptr(old));
end:
  LOG("%e[%d] -> %e[%d]",
      old.entry, old.index,
      entry, tc->index);
}

//static
void switch_entry(cell_t *entry, cell_t *r) {
  CONTEXT("switch_entry %E %C", entry, r);
  assert_error(is_var(r));
  if(r->value.tc.entry != entry) {
    assert_error(is_ancestor_of(r->value.tc.entry, entry));
    switch_entry_(entry, &r->value.tc);
  }
}

// find a matching trace cell given a variable or value
static
int trace_get_value(cell_t *entry, cell_t *r) {
  assert_error(r && is_value(r), "%C", r);
  if(is_list(r)) {
    return trace_build_quote(entry, r); // *** TODO prevent building duplicate quotes
  } else if(is_var(r)) {
    if(FLAG(r->value, VALUE_DEP)) return 0;
    switch_entry(entry, r);
    return r->value.tc.index;
  } else {
    int t = trace_lookup_value_linear(entry, r);
    if(t) return t;
  }
  assert_error(false);
  return -1;
}

// reserve space in the trace
int trace_alloc(cell_t *entry, csize_t args) {
  if(!entry) {
    LOG(MARK("WARN") " NULL entry");
    return -1;
  }
  int index = entry->entry.len + 1;
  size_t size = calculate_cells(args);
  cell_t *tc = trace_cell_ptr((trace_cell_t) {entry, index, 0});
  entry->entry.len += size;
  tc->n = -1;
  tc->size = args;
  LOG("trace_alloc %e[%d] size = %d", entry, index, args);
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

// store expression c in the trace
static
void trace_store_expr(cell_t *c, const cell_t *r) {
  cell_t *entry = r->value.tc.entry;
  cell_t *tc = trace_cell_ptr(r->value.tc);
  if(!tc) return;
  type_t t = r->value.type;
  if(tc->op) {
    // this cell has already been written
    // update the types
    if(t != T_ANY) {
      if(is_value(tc)) {
        trace_set_type(tc, t);
        FOR_TRACE(x, entry) {
          // update through assertions
          if(x->op == OP_assert) {
            if(trace_decode(x->expr.arg[0]) == r->value.tc.index) {
              x->trace.type = t;
            }
          }
        }
      } else if(is_dep(tc)) {
        tc->trace.type = t;
      }
    }
    return;
  }
  assert_error(tc->size == c->size);
  assert_error(c->op != OP_dep);
  CONTEXT_LOG("trace_store_expr: %e[%d] <- %s %C %C",
              entry, r->value.tc.index, op_name(c->op), c, r);

  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->pos = 0;
  tc->n = n;
  FLAG_CLEAR(tc->expr, EXPR_DELAYED);
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
    int x = 0;
    if(*p) {
      x = trace_get_value(entry, *p);
    }
    *p = trace_encode(x);
  }
  if(is_value(c)) {
    tc->value.alt_set = 0;
    tc->value.type = t;
  }
  tc->trace.type = t;
  if(tc->op == OP_placeholder) FLAG_SET(tc->trace, TRACE_INCOMPLETE);
  tc->alt = NULL;
}

void trace_store_row_assert(cell_t *c, cell_t *r) {
  cell_t
    *p = c->expr.arg[0],
    *q = c->expr.arg[1];
  if(!is_var(q) || !is_list(p)) return;
  assert_error(is_row_list(r));
  cell_t *f = *left_elem(r);
  if(!is_var(f)) return;
  assert_error(is_function(f));
  cell_t *entry = f->value.tc.entry;
  cell_t *t = trace_cell_ptr(f->value.tc);
  if(t->op) return;
  t->op = OP_assert;
  t->expr.arg[0] = trace_encode(is_row_list(p) ?
                                trace_get_value(entry, *left_elem(p)) :
                                NIL_INDEX);
  int tq = trace_get_value(entry, q);
  entry[tq].n++;
  t->expr.arg[1] = trace_encode(tq);
  t->trace.type = T_LIST;
}

int trace_otherwise(cell_t *p) {
  cell_t *entry = p->value.tc.entry;
  int x = trace_alloc(entry, 2);
  cell_t *tc = &entry[x];
  tc->op = OP_otherwise;
  tc->expr.arg[0] = trace_encode(p->value.tc.index);
  entry[p->value.tc.index].n++;
  return x;
}

// store value c in the trace
int trace_store_value(cell_t *entry, const cell_t *c) {
  if(!entry) return -1;
  assert_error(!is_list(c));

  // look to see if the value already is in the trace
  int x = trace_lookup_value_linear(entry, c);
  if(x == -1) {
    x = trace_copy(entry, c);
    cell_t *tc = &entry[x];
    tc->value.alt_set = 0;
    tc->alt = NULL;
    tc->pos = 0;
    tc->n = -1;
  }

  if(c->value.otherwise) {
    int ow = c->value.otherwise;
    cell_t *tc = &entry[ow];
    if(!tc->expr.arg[1]) {
      tc->expr.arg[1] = trace_encode(x);
      entry[x].n++;
    }
    x = ow;
  }

  return x;
}

// store c which reduces to r in the trace
static
void trace_store(cell_t *c, const cell_t *r) {
  assert_error(is_var(r));
  if(FLAG(r->value, VALUE_DEP)) {
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
    if(p->op == OP_exec &&
       p->expr.arg[in = closure_in(p)] == encoded_entry) {
      unsigned int cnt = 0;
      assert_error(in == entry->entry.in, "incorrect self call arity at %E %d", entry, p-entry);
      COUNTUP(i, in) {
        trace_index_t v = in - i;
        if(trace_decode(p->expr.arg[i]) != v) {
          cell_t *a = &entry[v];
          assert_error(is_var(a));
          cnt++;
          // mark variables that change during recursion
          FLAG_SET(a->value, VALUE_CHANGES);
        }
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
  (void)size;
  assert_error((void *)(trace_ptr + size) < (void *)(&trace_cells+1));
  return trace_ptr;
}

// setup for tracing
cell_t *trace_start_entry(cell_t *parent, csize_t out) {
  cell_t *e = get_trace_ptr(64);
  trace_ptr += 64; // TODO
  e->n = PERSISTENT;
  e->entry = (struct entry) {
    .in = 0,
    .out = out
  };
  e->entry.parent = parent;
  e->entry.wrap = NULL;

  // active_entries[e->pos-1] = e
  active_entries[prev_entry_pos++] = e;
  e->pos = prev_entry_pos;

  return e;
}

// finish tracing
void trace_end_entry(cell_t *e) {
  e->entry.wrap = NULL;
  FLAG_SET(e->entry, ENTRY_COMPLETE);
  e->entry.rec = trace_recursive_changes(e);
}

void trace_clear_alt(cell_t *entry) {
  FOR_TRACE(c, entry) {
    if(is_value(c) && c->value.type == T_RETURN) continue;
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
  if(NOT_FLAG(c->value, VALUE_DEP)) return;
  cell_t *entry = c->value.tc.entry;
  if(!entry) return;
  int x = trace_alloc(entry, 1);
  cell_t *tc = &entry[x];
  cell_t *ph = trace_cell_ptr(c->value.tc);
  int ph_x = c->value.tc.index;
  ph->expr.arg[c->pos] = trace_encode(x);
  LOG("trace_dep: %d <- %C %d[%d]", x, c, ph_x, c->pos);
  tc->op = OP_dep;
  tc->expr.arg[0] = trace_encode(ph_x);
  tc->trace.type = c->value.type;
  ph->n++;
  c->value.tc.index = x;
  c->pos = 0;
  FLAG_CLEAR(c->value, VALUE_DEP);
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  trace_cell_t tc = r->value.tc;
  cell_t *c = trace_cell_ptr(tc);
  if(tc.entry && !c->op &&
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
  WATCH(c, "trace_reduction", "%C", r);
  cell_t *new_entry = trace_expr_entry(c->pos);
  if(!is_var(r)) {
    // print tracing information for a reduction
    if(FLAG(c->expr, EXPR_TRACE)) {
      printf("TRACE: %s", op_name(c->op));
      TRAVERSE(c, in) {
        show_one(*p);
      }
      printf(" ->");
      show_one(r);
      printf("\n");
    }
    if(c->op != OP_exec) { // is this still necessary?
      return;
    }
  }
  if(is_list(r)) {
    r = get_list_function_var(r);
    if(!r) return;
  }

  cell_t *entry = r->value.tc.entry;
  if(!entry) entry = new_entry;

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
  if(is_var(r) && new_entry && new_entry != entry) {
    switch_entry(new_entry, r);
  }
}

// update the type of c in the trace
void trace_update_type(cell_t *c) {
  type_t t = c->value.type;
  if(t != T_ANY) {
    cell_t *tc = trace_cell_ptr(c->value.tc);
    if(tc && tc->op) {
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
void trace_set_type(cell_t *tc, type_t t) {
  tc->trace.type = t;
  if(is_value(tc)) {
    tc->value.type = t;
    if(is_var(tc)) {
      cell_t *p = trace_cell_ptr(tc->value.tc);
      if(p) {
        trace_set_type(p, t);
      }
    }
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
      switch_entry(entry, p);
      return p->value.tc.index;
    }
  }

  return compile_quote(entry, l);
}

cell_t *trace_quote_var(cell_t *l) {
  if(l == &nil_cell) return l;
  assert_error(l != NULL);
  cell_t *f = *leftmost_row(&l);
  while(is_placeholder(f)) f = f->expr.arg[closure_in(f) - 1];
  assert_error(is_var(f), "not a var: %O %C", f->op, f);
  cell_t *entry = f->value.tc.entry;
  int x = trace_build_quote(entry, l);
  return x == NIL_INDEX ? &nil_cell : var_create_nonlist(T_LIST, (trace_cell_t) {entry, x, 0});
}

// store a return
static
int trace_return(cell_t *entry, cell_t *c_) {
  cell_t *c = flat_copy(c_);
  cell_t **p;
  FORLIST(p, c, true) {
    trace_index_t x;
    if(is_var(*p)) {
      switch_entry(entry, *p);
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
  LOG("trace_return: %e[%d] <- %C", entry, x, c_);
  trace_cell_t t = {entry, x, 0};
  closure_free(c);
  cell_t *tc = trace_cell_ptr(t);
  tc->value.type = T_RETURN;
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

TEST(var_count) {
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
  bool is_assert = tc->op == OP_assert;
  if((is_assert || (tc->op == OP_exec &&
                    trace_decode(tc->expr.arg[closure_in(tc)]) == (int)entry->entry.len-1)) &&
     tc->trace.type == T_ANY) {
    if(is_assert) {
      if(tail_call_to_bottom(entry, trace_decode(tc->expr.arg[0]))) {
        tc->trace.type = T_BOTTOM;
        return true;
      }
    } else {
      tc->trace.type = T_BOTTOM;
      return true;
    }
  }
  return false;
}

// reduce for tracing & compilation
unsigned int trace_reduce(cell_t *entry, cell_t **cp) {
  cell_t *c = *cp;
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;

  CONTEXT("trace_reduce %E %C", entry, c);
  insert_root(cp);

  cell_t **p = cp;
  while(*p) {
    CONTEXT("branch %d: %C", alts, *p);
    response rsp = func_list(p, req_pos(REQ(return), entry->pos));
    // TODO handle rotating alts
    if(rsp != SUCCESS) continue;
    assert_alt(c, *p); // O(alts^2)
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
  if(!alts) {
    LOG("reduction failed for %E", entry);
  }
  return alts;
}

unsigned int trace_reduce_one(cell_t *entry, cell_t *c) {
  cell_t *l = quote(c);
  int alts = trace_reduce(entry, &l);
  drop(l);
  return alts;
}

int trace_alloc_var(cell_t *entry) {
  int x = trace_alloc(entry, 2);
  if(x <= 0) return x;
  cell_t *tc = &entry[x];
  tc->op = OP_value;
  tc->value.flags = VALUE_VAR;
  tc->pos = ++entry->entry.in;
  if(tc->pos != x) {
    FLAG_SET(entry->entry, ENTRY_MOV_VARS);
  }
  return x;
}

bool valid_pos(uint8_t pos) {
  return INRANGE(pos, 1, prev_entry_pos);
}

cell_t *trace_expr_entry(uint8_t pos) {
  return valid_pos(pos) ?
    active_entries[pos - 1] : NULL;
}

cell_t *param(int t, cell_t *entry) {
  return var_create_nonlist(t, (trace_cell_t) {entry, trace_alloc_var(entry), 0});
}

void print_active_entries(const char *msg) {
  const char *sep = msg;
  COUNTUP(i, prev_entry_pos) {
    cell_t *e = active_entries[i];
    printf("%s%s.%s (%d)", sep,
           e->module_name, e->word_name,
           entry_number(e));
    sep = ", ";
  }
  if(sep != msg) printf("\n");
}
