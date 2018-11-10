/* Copyright 2012-2018 Dustin DeWeese
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
#include "byte_compile.h"

// storage for tracing
#define ALIGN64 __attribute__((aligned(64)))

#define ENTRY_BLOCK_SIZE 1024
#define MAP_BLOCK_SIZE 64
#define MAX_TRACE_CELLS (1 << 14)
#define BLOCK_MAP_SIZE ((MAX_TRACE_CELLS + MAP_BLOCK_SIZE - 1) / MAP_BLOCK_SIZE)

static cell_t trace_cells[MAX_TRACE_CELLS] ALIGN64;
static cell_t *trace_ptr = &trace_cells[0];
static cell_t *active_entries[1 << 4];
static unsigned int prev_entry_pos = 0;
static cell_t *trace_block_map[BLOCK_MAP_SIZE] = {0};

#include "trace-local.h"

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

#if INTERFACE

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

void set_entry(cell_t *c, cell_t *e) {
  assert_error(is_user_func(c));
  c->expr.arg[closure_in(c)] = trace_encode(e - trace_cells);
}

int entry_number(cell_t const *e) {
  return e - trace_cells;
}

cell_t *entry_from_number(int n) {
  assert_throw(n >= 0 &&
               n < trace_ptr - trace_cells,
               "invalid entry number");
  return &trace_cells[n];
}

static
void trace_update_block_map(cell_t *entry) {
  cell_t *e = entry, *ne = trace_entry_next(e);
  RANGEUP(i, DIV_UP(entry - trace_cells, MAP_BLOCK_SIZE), BLOCK_MAP_SIZE) {
    cell_t *block_start = &trace_cells[i * MAP_BLOCK_SIZE];
    while(ne <= block_start) {
      e = ne;
      ne = trace_entry_next(ne);
    }
    trace_block_map[i] = e;
    if(e >= trace_ptr) break;
  }
}

bool entry_has(cell_t *entry, cell_t *v) {
  if(!entry) return false;
  cell_t *end = entry + trace_entry_size(entry);
  return v > entry && v < end;
}

cell_t *trace_entry_next(cell_t *e) {
  return e + (FLAG(e->entry, ENTRY_BLOCK) ?
              ENTRY_BLOCK_SIZE :
              trace_entry_size(e));
}

bool closure_match(cell_t *a, cell_t *b) {
  if(a->op != b->op ||
     a->size != b->size) return false;
  if(is_var(a)) {
    return a->value.type == b->value.type;
  } else if(!is_value(a)) {
    return memcmp(a->expr.arg, b->expr.arg,
                  sizeof_field(cell_t, expr.arg[0]) * closure_args(a)) == 0;
  } else if(ONEOF(a->value.type, T_LIST, T_RETURN)) {
    return memcmp(a->value.ptr, b->value.ptr,
                  sizeof_field(cell_t, expr.arg[0]) * list_size(a)) == 0;
  } else {
    return equal_value(a, b);
  }
}

bool entries_match(cell_t *entry_a, cell_t *entry_b) {
  int len = entry_a->entry.len;
  if(len != entry_b->entry.len) return false;
  int offset = entry_b - entry_a;
  FOR_TRACE(a, entry_a) {
    if(!closure_match(a, a + offset)) return false;
  }
  return true;
}

cell_t *block_first_entry(cell_t *v) {
  int i = (v - trace_cells) / MAP_BLOCK_SIZE;
  assert_error(i < BLOCK_MAP_SIZE);
  return trace_block_map[i];
}

cell_t *top_parent(cell_t *e) {
  cell_t *r = NULL;
  FOLLOW(p, e, entry.parent) {
    r = p;
  }
  return r;
}

cell_t *trace_matching_entry(cell_t *x) {
  assert_error(is_trace_cell(x));
  cell_t *top = top_parent(x);
  if(!top) return x;
  for(cell_t *e = top;
      e < x;
      e = trace_entry_next(e)) {
    if(entries_match(e, x)) return e;
  }
  return x;
}

void dedup_entry(cell_t **e) {
  cell_t *p = trace_matching_entry(*e);
  if(p < *e) {
    (*e)->n = 0; // tag for deletion
    *e = p;
  }
}

cell_t *var_entry(cell_t *v) {
  assert_error(is_trace_cell(v));
  for(cell_t *e = block_first_entry(v);
      e < trace_ptr;
      e = trace_entry_next(e)) {
    if(entry_has(e, v)) return e;
  }
  return NULL;
}

int entry_pos(cell_t *e) {
  FOLLOW(p, e, entry.parent) {
    if(e->pos) return e->pos;
  }
  return 0;
}

cell_t *get_var(cell_t *entry, cell_t *c) {
  assert_error(is_value(c));
  return var_for_entry(entry, c->value.var);
}

cell_t *var_for_entry(cell_t *entry, cell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  FOLLOW(p, v, value.var) {
    if(entry_has(entry, p)) return p;
    if(!is_value(p)) break;
  }
  return NULL;
}

int var_index(cell_t *entry, cell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  assert_error(v);
  return v - entry;
}

int var_index_nofail(cell_t *entry, cell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  if(!v) return -1;
  return v - entry;
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
  case T_LIST:
    // only handle nil
    return is_empty_list(a) && is_empty_list(b);
  case T_STRING:
    return strcmp(a->value.str, b->value.str) == 0;
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

// Change the active entry, and add to the list if needed.
void switch_entry(cell_t *entry, cell_t *r) {
  CONTEXT("switch_entry %s %C", entry->word_name, r);
  assert_error(is_var(r));
  if(!get_var(entry, r)) {
    if(entry->entry.parent)
      switch_entry(entry->entry.parent, r);

    WATCH(r, "switch_entry", "%s", entry->word_name);
    cell_t *v = r->value.var;

    // a little hacky because ideally variables shouldn't be duplicated
    // see TODO in func_value
    FOR_TRACE(c, entry) {
      if(is_var(c) &&
         c->value.var == v) {
        r->value.var = c;
        return;
      }
    }

    cell_t *n = trace_alloc_var(entry);
    n->value.type = trace_type(v);
    //if(is_var(v)) v = &var_entry(v)[v->pos]; // variables move *** DOESN'T WORK
    n->value.var = v;
    r->value.var = n;
  }
}

void mark_pos(cell_t *c, int pos) {
  if(!pos || c->pos == pos) return;
  assert_error(c->n != PERSISTENT);
  cell_t *entry = trace_expr_entry(pos);
  WATCH(c, "mark_pos", "%s", entry->word_name);
  if(is_var(c)) {
    switch_entry(entry, c);
  } else {
    c->pos = pos;
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
    return var_index(entry, r->value.var);
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
  entry->entry.len += size;
  assert_error(entry->entry.len <= ENTRY_BLOCK_SIZE - 1);
  cell_t *tc = &entry[index];
  tc->n = -1;
  tc->size = args;
  LOG("trace_alloc %s[%d] size = %d", entry->word_name, tc-entry, args);
  return index;
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

void trace_arg(cell_t *tc, int n, cell_t *a) {
  assert_error(is_var(a));
  cell_t **p = &tc->expr.arg[n];
  if(!*p) {
    cell_t *v = a->value.var;
    *p = trace_encode(var_index(NULL, v));
    v->n++;
  }
  trace_set_type(tc, a->value.type);
}

static
int trace_value(cell_t *entry, cell_t *v) {
  return is_var(v) || is_list(v) ?
    trace_get_value(entry, v) :
    trace_store_value(entry, v);
}

// store expression c in the trace
static
void trace_store_expr(cell_t *c, const cell_t *r) {
  cell_t *entry = var_entry(r->value.var);
  cell_t *tc = r->value.var;
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
            if(trace_decode(x->expr.arg[0]) == var_index(entry, r->value.var)) {
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
  CONTEXT_LOG("trace_store_expr: %s[%d] <- %O %C %C",
              entry->word_name,
              r->value.var-entry,
              c->op, c, r);

  refcount_t n = tc->n;
  memcpy(tc, c, sizeof(cell_t) * closure_cells(c));
  tc->pos = 0;
  tc->n = n;
  if(is_user_func(tc)) {
    // encode the entry
    cell_t **e = &tc->expr.arg[closure_in(tc)];
    *e = trace_encode(entry_number(*e));
  }

  // encode inputs
  TRAVERSE(tc, in) {
    cell_t *a = *p;
    if(a) {
      assert_error(!is_marked(a));
      int x = trace_value(entry, a);
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
  cell_t *entry = var_entry(f->value.var);
  cell_t *t = f->value.var;
  if(t->op) return;
  t->op = OP_assert;
  t->expr.arg[0] = trace_encode(is_row_list(p) ?
                                trace_get_value(entry, *left_elem(p)) :
                                trace_store_value(entry, &nil_cell));
  int tq = trace_get_value(entry, q);
  entry[tq].n++;
  t->expr.arg[1] = trace_encode(tq);
  t->trace.type = T_LIST;
}

// for otherwise and assert
cell_t *trace_partial(op op, int n, cell_t *p) {
  CONTEXT_LOG("trace_partial %O, arg[%d] = %C", op, n, p);
  cell_t *entry = var_entry(p->value.var);
  int a = is_var(p) ? var_index(entry, p->value.var) :
    op == OP_otherwise && p->value.var ? trace_store_something(entry, &p->value.var) : // (*)
    trace_store_value(entry, p);
  // (*) Because the value will be discarded, just use Something
  int x = trace_alloc(entry, 2);
  cell_t *tc = &entry[x];
  tc->op = op;
  tc->expr.arg[n] = trace_encode(a);
  entry[a].n++;
  return tc;
}

void apply_condition(cell_t *c, int *x) {
  if(c->value.var) {
    cell_t *entry = var_entry(c->value.var);
    cell_t *t = concatenate_conditions(c->value.var, &entry[*x]);
    c->value.var = t;
    *x = var_index(entry, t);
  }
}

// store value c in the trace
int trace_store_value(cell_t *entry, cell_t *c) {
  if(!entry) return -1;

  // look to see if the value already is in the trace
  int x = trace_lookup_value_linear(entry, c);
  if(x == -1) {
    assert_error(!is_list(c) || list_size(c) == 0);
    x = trace_copy(entry, c);
    cell_t *tc = &entry[x];
    tc->value.alt_set = 0;
    tc->value.var = NULL;
    tc->alt = NULL;
    tc->pos = 0;
    tc->n = -1;
  }

  apply_condition(c, &x);
  return x;
}

int trace_store_something(cell_t *entry, cell_t **v) {
  cell_t c = (cell_t) {
    .op = OP_value,
    .value = {
      .type = T_SYMBOL,
      .integer = SYM_Something,
      .var = *v
    },
    .size = 2,
    .n = -1
  };
  int x = trace_store_value(entry, &c);
  *v = c.value.var;
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
      assert_error(in == entry->entry.in,
                   "incorrect self call arity at %s %d",
                   entry->word_name, p-entry);
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
  assert_error((void *)(trace_ptr + size) < (void *)(&trace_cells+1));
  memset(trace_ptr, 0, sizeof(*trace_ptr) * size);
  return trace_ptr;
}

// setup for tracing
cell_t *trace_start_entry(cell_t *parent, csize_t out) {
  cell_t *e = get_trace_ptr(ENTRY_BLOCK_SIZE);
  trace_ptr += ENTRY_BLOCK_SIZE; // TODO
  e->n = PERSISTENT;
  e->entry = (struct entry) {
    .out = out,
    .parent = parent,
    .flags = ENTRY_BLOCK
  };

  // active_entries[e->pos-1] = e
  assert_error(prev_entry_pos < LENGTH(active_entries));
  active_entries[prev_entry_pos++] = e;
  e->pos = prev_entry_pos;
  trace_update_block_map(e);

  return e;
}

// finish tracing
void trace_end_entry(cell_t *e) {
  e->entry.wrap = NULL;
  assert_error(prev_entry_pos && e->pos == prev_entry_pos, "out of order start/end entry");
  active_entries[--prev_entry_pos] = NULL;
  e->pos = 0;
  FLAG_SET(e->entry, ENTRY_COMPLETE);
  e->entry.rec = trace_recursive_changes(e);
}

cell_t *trace_current_entry() {
  if(prev_entry_pos) {
    return active_entries[prev_entry_pos - 1];
  } else {
    return NULL;
  }
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
  cell_t *entry = var_entry(c->value.var);
  if(!entry) return;
  int x = trace_alloc(entry, 1);
  cell_t *tc = &entry[x];
  cell_t *ph = c->value.var;
  int ph_x = var_index(entry, c->value.var);
  ph->expr.arg[c->pos] = trace_encode(x);
  LOG("trace_dep: %d <- %C %d[%d]", x, c, ph_x, c->pos);
  tc->op = OP_dep;
  tc->expr.arg[0] = trace_encode(ph_x);
  tc->trace.type = c->value.type;
  ph->n++;
  c->value.var = tc;
  c->pos = 0;
  FLAG_CLEAR(c->value, VALUE_DEP);
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  cell_t *v = r->value.var;
  if(v && !v->op) {
    cell_t *e = var_entry(v);
    if(e) {
      int ix = var_index(e, v);
      if(e->entry.len - (ix - 1) == calculate_cells(v->size)) {
        e->entry.len = ix - 1;
      }
    }
  }
}

// find the function variable in a list
static
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
  trace_update_type(r);
  if(!is_var(r)) {
    // print tracing information for a reduction
    if(FLAG(c->expr, EXPR_TRACE)) {
      printf("TRACE: %s", op_name(c->op));
      TRAVERSE(c, in) {
        putchar(' ');
        show_one(*p);
      }
      printf(" -> ");
      show_one(r);
      printf("\n");
      if(break_on_trace) {
        LOG("break on trace: %C -> %C", c, r);
        breakpoint();
      }
    }
    if(c->op != OP_exec) { // is this still necessary?
      return;
    }
  }
  if(is_list(r)) {
    r = get_list_function_var(r);
    if(!r) return;
  }

  cell_t *entry = var_entry(r->value.var);
  if(!entry) entry = new_entry;

  trace_store(c, r);
  if(is_var(r) && new_entry && new_entry != entry) {
    switch_entry(new_entry, r);
  }
}

// update the type of c in the trace
void trace_update_type(cell_t *c) {
  type_t t = c->value.type;
  if(t != T_ANY) {
    cell_t *tc = c->value.var;
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
      cell_t *p = tc->value.var;
      if(p) {
        trace_set_type(p, t);
      }
    }
  }
}

bool reduced_list(cell_t *l) {
  cell_t **p;
  FORLIST(p, l, true) {
    if(!is_value(*p)) {
      return false;
    }
  }
  return true;
}

// store captured variables to be compiled into a quote
int trace_build_quote(cell_t *entry, cell_t *l) {
  assert_error(is_list(l));
  if(is_empty_list(l)) return trace_store_value(entry, l);
  if(is_row_list(l) && // ***
     list_size(l) == 1) {
    cell_t *p = l->value.ptr[0];
    if(is_placeholder(p) &&
       closure_in(p) == 1 &&
       closure_out(p) == 0) p = p->expr.arg[0]; //***
    if(is_var(p) && is_function(p)) {
      // identity list, so just return the trace cell for the item in the list
      switch_entry(entry, p);
      return var_index(entry, p->value.var);
    }
  }

  // if there is no computation in the quote, convert to ap or compose
  if(reduced_list(l)) {
    LOG("all var list %C", l);
    bool row = is_row_list(l);
    const int size = function_out(l, true);
    int x = trace_alloc(entry, size);
    cell_t *tc = &entry[x];
    tc->op = row ? OP_pushr : OP_quote;
    int n = size;

    cell_t **p;
    FORLIST(p, l, true) {
      int x = trace_value(entry, *p);
      tc->expr.arg[--n] = trace_encode(x);
      entry[x].n++;
    }
    return x;
  }

  return compile_quote(entry, l);
}

cell_t *trace_quote_var(cell_t *entry, cell_t *l) {
  assert_error(l != NULL);
  if(is_empty_list(l)) return l;
  if(is_var(l)) return l;
  int x = trace_build_quote(entry, l);
  drop(l);
  return var_create_nonlist(T_LIST, &entry[x]);
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
      x = var_index(entry, (*p)->value.var);
    } else if(is_list(*p)) {
      x = trace_build_quote(entry, *p);
    } else {
      x = trace_store_value(entry, *p);
    }
    *p = trace_encode(x);
    if(x >= 0) entry[x].n++;
  }
  int x = trace_copy(entry, c);
  cell_t *tc = &entry[x];
  LOG("trace_return: %s[%d] <- %C", entry->word_name, tc-entry, c_);
  closure_free(c);
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

// reduce for tracing & compilation
unsigned int trace_reduce(cell_t *entry, cell_t **cp) {
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;
  context_t *ctx = ctx_pos(&CTX(return), entry->pos);

  CONTEXT("trace_reduce %s %C", entry->word_name, *cp);
  insert_root(cp);

  COUNTUP(priority, PRIORITY_MAX) {
    cell_t **p = cp;
    bool delay = false;
    CONTEXT("priority = %d", priority);
    while(*p) {
      CONTEXT("branch %d: %C", alts, *p);
      response rsp = func_list(p, WITH(ctx, priority, priority));
      if(rsp == DELAY) {
        delay = true;
        p = &(*p)->alt;
        continue;
      }
      // TODO handle rotating alts
      if(rsp != SUCCESS) continue;
      if(!delay) assert_alt(*cp, *p); // O(alts^2)
      cell_t **a;
      FORLIST(a, *p, true) {
        collapse_row(a);
        force(a); // ***
        if(is_value(*a) &&
           !is_list(*a) &&
           !is_var(*a)) { // TODO deps?
          LOG(TODO " use return value of trace_store_value");
          trace_store_value(entry, *a); // TODO use return value
        }
      }
      int x = trace_return(entry, *p);
      cell_t *r = &entry[x];

      LOG("branch %d finished %C", alts, *p);

      r->n++;
      alts++;

      *prev = trace_encode(x);
      prev = &r->alt;
      *p = CUT(*p, alt);
    }
    if(!delay) break;
  }

  remove_root(cp);
  if(!alts) {
    LOG("reduction failed for %s", entry->word_name);
  }
  return alts;
}

unsigned int trace_reduce_one(cell_t *entry, cell_t *c) {
  cell_t *l = quote(c);
  int alts = trace_reduce(entry, &l);
  drop(l);
  return alts;
}

cell_t *trace_alloc_var(cell_t *entry) {
  int x = trace_alloc(entry, 2);
  if(x <= 0) return NULL;
  cell_t *tc = &entry[x];
  tc->op = OP_value;
  tc->value.flags = VALUE_VAR;
  tc->pos = ++entry->entry.in;
  if(tc->pos != x) {
    FLAG_SET(entry->entry, ENTRY_MOV_VARS);
  }
  return tc;
}

bool valid_pos(uint8_t pos) {
  return INRANGE(pos, 1, prev_entry_pos);
}

cell_t *trace_expr_entry(uint8_t pos) {
  return valid_pos(pos) ?
    active_entries[pos - 1] : NULL;
}

cell_t *param(int t, cell_t *entry) {
  return var_create_nonlist(t, trace_alloc_var(entry));
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

cell_t *tc_get(cell_t *entry, int x) {
  return entry && x > 0 ? &entry[x] : NULL;
}

FORMAT(entry, 'E') {
  if(i) {
    cell_t *entry = (cell_t *)i;
    printf("%s.%s(%d)",
           entry->module_name,
           entry->word_name,
           entry_number(entry));
  } else {
    printf("null_entry");
  }
}

FORMAT(entry_short, 'e') {
  cell_t *entry = (cell_t *)i;
  if(!entry) {
    printf("???");
  } else if(entry->word_name) {
    printf("%s", entry->word_name);
  } else {
    printf("%d", entry_number(entry));
  }
}

FORMAT(trace_cell, 'T') {
  cell_t *tc = (cell_t *)i;
  cell_t *entry = var_entry(tc);
  format_entry_short((val_t)entry);
  printf("[%d]", var_index(entry, tc));
}

/* condiitons */

// assert & otherwise form lists that can be concatenated
// TODO use refcounting to avoid destructive concatenation
cell_t *concatenate_conditions(cell_t *a, cell_t *b) {
  if(a == NULL) return b;
  if(b == NULL) return a;
  cell_t **arg = NULL;
  cell_t *entry = var_entry(a);
  b = var_for_entry(entry, b);
  assert_error(b, "%s %d %d", entry->word_name, a-entry, b-entry);
  cell_t *p = a;
  cell_t *bi = trace_encode(var_index(entry, b));

  // find the end of a
  while(p != b) {
    switch(p->op) {
    case OP_assert:
    case OP_seq:
      arg = &p->expr.arg[0];
      break;
    case OP_otherwise:
      arg = &p->expr.arg[1];
      LOG_WHEN(p->expr.arg[0] == bi,
               "duplicate arg for otherwise: %s %d <- %d",
               entry->word_name, p-entry, b-entry);
      break;
    default:
      LOG("trace_seq: %s %d ... %d <- %d",
          entry->word_name,
          a-entry, p-entry, b-entry);
      return trace_seq(b, a);
    }
    if(*arg) {
      p = &entry[trace_decode(*arg)];
      assert_error(p != a, "loop");
    } else { // found empty arg
      LOG("condition %s %d ... %d %O arg <- %d",
          entry->word_name, a-entry, p-entry, p->op, b-entry);
      *arg = bi;
      b->n++;
      return a;
    }
  }
  return a;
}

cell_t *trace_seq(cell_t *a, cell_t *b) {
  cell_t *entry = var_entry(a);
  assert_error(entry_has(entry, b));
  int x = trace_alloc(entry, 2);
  cell_t *tc = &entry[x];
  cell_t *p, *q;
  if(b->op == OP_assert) {
    p = a;
    q = &entry[trace_decode(b->expr.arg[1])];
    tc->op = OP_assert;
  } else if(b->op == OP_otherwise) {
    p = &entry[trace_decode(b->expr.arg[0])];
    q = a;
    tc->op = OP_otherwise;
  } else {
    p = a;
    q = b;
    tc->op = OP_seq;
  }
  tc->pos = 0;
  tc->expr.arg[0] = trace_encode(var_index(entry, p));
  p->n++;
  tc->expr.arg[1] = trace_encode(var_index(entry, q));
  q->n++;
  return tc;
}

cell_t *value_condition(cell_t *a) {
  return a && is_value(a) && !is_var(a) ? a->value.var : NULL;
}

void add_conditions_2(cell_t *res, cell_t *a0) {
  cell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v, value_condition(a0));
  }
}

void add_conditions_3(cell_t *res, cell_t *a0, cell_t *a1) {
  cell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
                                  value_condition(a1)));
  }
}

void add_conditions_from_array(cell_t *res, cell_t **a, unsigned int n) {
  cell_t **v = &res->value.var;
  if(!is_var(res) && n) {
    cell_t *cs = value_condition(a[n-1]);
    COUNTDOWN(i, n-1) {
      cs = concatenate_conditions(value_condition(a[i]), cs);
    }
    *v = concatenate_conditions(*v, cs);
  }
}

void add_conditions_var_2(cell_t *res, cell_t *t) {
  cell_t **v = &res->value.var;
  *v = concatenate_conditions(t, *v);
}

void add_conditions_var_3(cell_t *res, cell_t *t, cell_t *a0) {
  cell_t **v = &res->value.var;
  *v = concatenate_conditions(t,
         concatenate_conditions(value_condition(a0), *v));
}

#if INTERFACE
#define add_conditions(...) DISPATCH(add_conditions, __VA_ARGS__)
#define add_conditions_var(...) DISPATCH(add_conditions_var, __VA_ARGS__)
#endif

int trace_entry_size(cell_t *e) {
  return e->entry.len + 1;
}

// Compact from blocks to minimum size
void trace_compact(cell_t *entry) {
  // build map
  cell_t *end = trace_ptr;
  cell_t *ne = entry;
  for(cell_t *e = entry;
      e < end;
      e += ENTRY_BLOCK_SIZE) {
    if(e->n) {
      e->entry.compact = ne;
      ne += trace_entry_size(e);
    } else {
      memset(e, 0, sizeof(trace_cells[0]) * ENTRY_BLOCK_SIZE);
    }
  }

  // update references
  for(cell_t *e = entry; e < end; e += ENTRY_BLOCK_SIZE) {
    if(!e->n) continue;

    // update calls
    FOR_TRACE(c, e) {
      if(is_user_func(c)) {
        cell_t *ce = get_entry(c);
        if(ce > entry && ce->entry.compact) {
          set_entry(c, ce->entry.compact);
        }
      }
    }

    // update parent
    if(e->entry.parent > entry) {
      e->entry.parent = e->entry.parent->entry.compact;
    }
  }

  // move and clean up
  for(cell_t *e = entry; e < end; e += ENTRY_BLOCK_SIZE) {
    if(!e->n) continue;
    FLAG_CLEAR(e->entry, ENTRY_BLOCK);

    // move
    cell_t *compact = e->entry.compact;
    e->entry.compact = NULL;
    if(e > compact) {
      int size = trace_entry_size(e);
      memmove(compact, e, sizeof(trace_cells[0]) * size);
      int zero_size = min(size, e - compact);
      int offset = size - zero_size;
      memset(e + offset, 0, sizeof(trace_cells[0]) * zero_size);
    }
  }

  trace_ptr = ne;
  trace_update_block_map(entry);
}

int trace_count() {
  return trace_ptr - trace_cells;
}

void delay_branch(context_t *ctx, int priority) {
  FOLLOW(p, ctx, up) {
    cell_t *c = p->src;
    if(is_list(c)) {
      FLAG_SET(c->value, VALUE_DELAY);
      c->priority = priority;
      LOG("delay branch %C %d", c, priority);
    }
  }
}
