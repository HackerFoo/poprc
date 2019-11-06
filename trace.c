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
#define MAX_TRACE_CELLS (1 << 15)
#define BLOCK_MAP_SIZE ((MAX_TRACE_CELLS + MAP_BLOCK_SIZE - 1) / MAP_BLOCK_SIZE)

static tcell_t trace_cells[MAX_TRACE_CELLS] ALIGN64;
static tcell_t *trace_ptr = &trace_cells[0];
static tcell_t *active_entries[1 << 4];
static unsigned int prev_entry_pos = 0;
static tcell_t *trace_block_map[BLOCK_MAP_SIZE] = {0};

#include "trace-local.h"

#if INTERFACE
typedef intptr_t trace_index_t;
#endif

#if INTERFACE

// tcell_t *c ranges from start to end
#define FOR_TRACE_3(_c, e, n)                   \
  for(tcell_t                                   \
        *_entry = (e),                          \
        *_c = _entry + (n);                     \
      _c - _entry - 1 < _entry->entry.len;      \
      _c += calculate_tcells(_c->size))
#define FOR_TRACE_2(_c, e) FOR_TRACE_3(_c, e, 1)
#define FOR_TRACE(...) DISPATCH(FOR_TRACE, __VA_ARGS__)

// tcell_t *c ranges from end to start
#define FOR_TRACE_REV_3(_c, e, n)               \
  for(tcell_t                                   \
        *_entry = (e),                          \
        *_c = _entry + (n);                     \
      _c > _entry && _c->trace.prev_cells;      \
      _c -= _c->trace.prev_cells)
#define FOR_TRACE_REV_2(_c, e) FOR_TRACE_REV_3(_c, e, (e)->entry.len - ((e) + 1)->trace.prev_cells + 1)
#define FOR_TRACE_REV(...) DISPATCH(FOR_TRACE_REV, __VA_ARGS__)

/* const versions */

// tcell_t *c ranges from start to end
#define FOR_TRACE_CONST_3(_c, e, n)             \
  for(const tcell_t                             \
        *_entry = (e),                          \
        *_c = _entry + (n);                     \
      _c - _entry - 1 < _entry->entry.len;      \
      _c += calculate_tcells(_c->size))
#define FOR_TRACE_CONST_2(_c, e) FOR_TRACE_CONST_3(_c, e, 1)
#define FOR_TRACE_CONST(...) DISPATCH(FOR_TRACE_CONST, __VA_ARGS__)

// tcell_t *c ranges from end to start
#define FOR_TRACE_CONST_REV_3(_c, e, n)         \
  for(const tcell_t                             \
        *_entry = (e),                          \
        *_c = _entry + (n);                     \
      _c > _entry && _c->trace.prev_cells;      \
      _c -= _c->trace.prev_cells)
#define FOR_TRACE_CONST_REV_2(_c, e) FOR_TRACE_CONST_REV_3(_c, e, (e)->entry.len - ((e) + 1)->trace.prev_cells + 1)
#define FOR_TRACE_CONST_REV(...) DISPATCH(FOR_TRACE_CONST_REV, __VA_ARGS__)

#endif

bool is_trace_cell(void const *p) {
  return p >= (void *)&trace_cells && p < (void *)(&trace_cells+1);
}

void trace_init() {
  prev_entry_pos = 0;
}

#if INTERFACE
#define get_entry(c) _get_entry(GET_CELL(c))
#endif

tcell_t *_get_entry(cell_t const *c) {
  return is_user_func(c) ?
    &trace_cells[tr_entry(c->expr.arg[closure_in(c)])] :
    NULL;
}

void set_entry(tcell_t *c, tcell_t *e) {
  assert_error(is_user_func(c));
  c->expr.arg[closure_in(c)] = entry_tr(e - trace_cells);
}

int entry_number(tcell_t const *e) {
  return e - trace_cells;
}

tcell_t *entry_from_number(int n) {
  assert_throw(n >= 0 &&
               n < trace_ptr - trace_cells,
               "invalid entry number");
  return &trace_cells[n];
}

// trace_block_map[i] points to the last entry in the previous block,
// or the start of the block, which is the first entry of the block
static
void trace_update_block_map(tcell_t *entry) {
  tcell_t *e = entry, *ne = trace_entry_next(e);
  RANGEUP(i, DIV_UP(entry - trace_cells, MAP_BLOCK_SIZE), BLOCK_MAP_SIZE) {
    tcell_t *block_start = &trace_cells[i * MAP_BLOCK_SIZE];
    while(ne <= block_start) {
      e = ne;
      ne = trace_entry_next(ne);
    }
    trace_block_map[i] = e;
    if(e >= trace_ptr) break;
  }
}

bool entry_has(tcell_t *entry, tcell_t *v) {
  if(!entry) return false;
  tcell_t *end = entry + trace_entry_size(entry);
  return v > entry && v < end;
}

tcell_t *trace_entry_next(tcell_t *e) {
  return e + (FLAG(*e, entry, BLOCK) ?
              ENTRY_BLOCK_SIZE :
              trace_entry_size(e));
}

// TODO don't use memcmp, which causes false negatives
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

bool entries_match(tcell_t *entry_a, tcell_t *entry_b) {
  int len = entry_a->entry.len;
  if(len != entry_b->entry.len) return false;
  int offset = entry_b - entry_a;
  FOR_TRACE(a, entry_a) {
    if(!closure_match(&a->c, &(a + offset)->c)) return false;
  }
  return true;
}

tcell_t *block_first_entry(tcell_t *v) {
  int i = (v - trace_cells) / MAP_BLOCK_SIZE;
  assert_error(i < BLOCK_MAP_SIZE);
  return trace_block_map[i];
}

tcell_t *top_parent(tcell_t *e) {
  tcell_t *r = NULL;
  FOLLOW(p, e, entry.parent) {
    r = p;
  }
  return r;
}

tcell_t *trace_matching_entry(tcell_t *x) {
  assert_error(is_trace_cell(x));
  tcell_t *top = top_parent(x);
  if(!top) return x;
  for(tcell_t *e = top;
      e < x;
      e = trace_entry_next(e)) {
    if(entries_match(e, x)) return e;
  }
  return x;
}

void dedup_entry(tcell_t **e) {
  tcell_t *p = trace_matching_entry(*e);
  if(p < *e) {
    (*e)->n = 0; // tag for deletion
    *e = p;
  }
}

// CLEANUP bundle var and entry, no more ints

tcell_t *var_entry(tcell_t *v) {
  assert_error(is_trace_cell(v));
  for(tcell_t *e = block_first_entry(v);
      e < trace_ptr;
      e = trace_entry_next(e)) {
    assert_error(e->n == PERSISTENT);
    if(entry_has(e, v)) return e;
  }
  return NULL;
}

int entry_pos(tcell_t *e) {
  FOLLOW(p, e, entry.parent) {
    if(e->pos) return e->pos;
  }
  return 0;
}

tcell_t *get_var(tcell_t *entry, cell_t *c) {
  assert_error(is_value(c));
  return var_for_entry(entry, c->value.var);
}

tcell_t *var_for_entry(tcell_t *entry, tcell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  FOLLOW(p, v, value.var) {
    if(entry_has(entry, p)) return p;
    if(!is_value(p)) break;
  }
  return NULL;
}

int var_index(tcell_t *entry, tcell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  assert_error(v);
  return v - entry;
}

int var_index_nofail(tcell_t *entry, tcell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  if(!v) return -1;
  return v - entry;
}

cell_t *index_tr(int index) {
  tr x = {};
  x.index = index;
  return x.ptr;
}

int tr_index(const cell_t *c) {
  return ((tr) { .ptr = (cell_t *)c }).index;
}

cell_t *entry_tr(int entry) {
  tr x = {};
  x.entry = entry;
  return x.ptr;
}

int tr_entry(const cell_t *c) {
  return ((tr) { .ptr = (cell_t *)c }).entry;
}

void tr_set_flags(cell_t *const *cp, uint8_t flags) {
  ((tr *)cp)->flags = flags;
}

bool tr_flags(const cell_t *c, uint8_t flags) {
  tr *x = (tr *)&c;
  return !(~(x->flags) & flags);
}

void tr_set_index(cell_t *const *cp, int index) {
  ((tr *)cp)->index = index;
}

TEST(trace_encode) {
  return tr_index(index_tr(0x5ac3)) == 0x5ac3 ? 0 : -1;
}

bool equal_value(const cell_t *a, const cell_t *b) {
  int type = a->value.type;
  if(b->value.type != type) return false;
  switch(type) {
  case T_INT:
    return a->value.integer == b->value.integer;
  case T_SYMBOL:
    return a->value.symbol == b->value.symbol;
  case T_FLOAT:
    return a->value.flt == b->value.flt;
  case T_LIST:
    // only handle nil
    return is_empty_list(a) && is_empty_list(b);
  case T_STRING:
    return eq_seg(value_seg(a), value_seg(b));
  default:
    return false;
  }
}

// look through the trace for a matching value
static
int trace_lookup_value_linear(tcell_t *entry, const cell_t *c) {
  FOR_TRACE(p, entry) {
    if(p->op == OP_value &&
       NOT_FLAG(*p, value, VAR) &&
       equal_value(&p->c, c))
      return p - entry;
  }
  return -1;
}

// Change the active entry, and add to the list if needed.
void switch_entry(tcell_t *entry, cell_t *r) {
  CONTEXT("switch_entry %s %C", entry->word_name, r);
  assert_error(NOT_FLAG(*entry, entry, COMPLETE));
  assert_error(is_var(r));
  if(!get_var(entry, r)) {
    if(entry->entry.parent)
      switch_entry(entry->entry.parent, r);

    WATCH(r, "switch_entry", "%s", entry->word_name);
    tcell_t *v = r->value.var;

    // a little hacky because ideally variables shouldn't be duplicated
    // see TODO in func_value
    FOR_TRACE(c, entry) {
      if(is_var(c) &&
         c->value.var == v) {
        r->value.var = c;
        return;
      }
    }

    type_t t = trace_type(v);
    tcell_t *n = trace_alloc_var(entry, t);
    if(t == T_OPAQUE) n->value.symbol = r->value.symbol;
    //if(is_var(v)) v = &var_entry(v)[v->pos]; // variables move *** DOESN'T WORK
    n->value.var = v;
    r->value.var = n;
  }
}

void mark_pos(cell_t *c, int pos) {
  if(!pos || c->pos == pos) return;
  assert_error(c->n != PERSISTENT);
  tcell_t *entry = trace_expr_entry(pos);
  WATCH(c, "mark_pos", "%s", entry->word_name);
  if(is_var(c)) {
    switch_entry(entry, c);
  } else {
    c->pos = pos;
  }
}

// find a matching trace cell given a variable or value
static
int trace_get_value(tcell_t *entry, cell_t *r) {
  assert_error(r && is_value(r), "%C", r);
  if(is_list(r)) {
    return trace_build_quote(entry, r); // *** TODO prevent building duplicate quotes
  } else if(is_var(r)) {
    if(FLAG(*r, value, DEP)) return 0;
    switch_entry(entry, r);
    return var_index(entry, r->value.var);
  } else {
    int t = trace_lookup_value_linear(entry, r);
    if(t) return t;
  }
  assert_error(false);
  return -1;
}

csize_t calculate_tcells(csize_t n) {
  const csize_t args_in_first_cell =
    (sizeof(tcell_t) - offsetof(tcell_t, expr.arg)) / sizeof(tcell_t *);
  return n <= args_in_first_cell ? 1 : calc_size(tcell_t, expr.arg, n);
}

// reserve space in the trace
int trace_alloc(tcell_t *entry, csize_t args) {
  if(!entry) {
    LOG(MARK("WARN") " NULL entry");
    return -1;
  }
  assert_error(NOT_FLAG(*entry, entry, COMPLETE));
  int index = entry->entry.len + 1;
  size_t size = calculate_tcells(args);
  entry->entry.len += size;
  assert_error(entry->entry.len <= ENTRY_BLOCK_SIZE - 1);
  tcell_t *tc = &entry[index];
  tc->n = -1;
  tc->size = args;
  LOG("trace_alloc %s[%d] size = %d", entry->word_name, tc-entry, args);
  return index;
}

// reduce allocated space in the trace
void trace_shrink(tcell_t *t, csize_t args) {
  assert_error(args <= t->size);
  csize_t
    prev_cells = calculate_tcells(t->size),
    new_cells = calculate_tcells(args),
    diff = prev_cells - new_cells;
  t->size = args;

  // blank the extra cells
  tcell_t *p = &t[prev_cells];
  LOOP(diff) {
    memset(p, 0, sizeof(tcell_t));
    p->size = 1;
  }
}

#define trace_copy(entry, _c)                           \
  _Generic(*(_c),                                       \
           cell_t: trace_copy_cell,                     \
           tcell_t: trace_copy_tcell)((entry), (_c))

// copy c into newly allocated space in the trace
static
int trace_copy_cell(tcell_t *entry, const cell_t *c) {
  int index = trace_alloc(entry, c->size);
  size_t n = closure_cells(c);
  memcpy(&entry[index].c, c, sizeof(cell_t) * n);
  return index;
}

static
int trace_copy_tcell(tcell_t *entry, const tcell_t *tc) {
  int index = trace_alloc(entry, tc->size);
  size_t n = closure_tcells(tc);
  memcpy(&entry[index], tc, sizeof(tcell_t) * n);
  return index;
}

void trace_arg(tcell_t *tc, int n, cell_t *a) {
  assert_error(is_var(a));
  cell_t **p = &tc->expr.arg[n];
  tcell_t *entry = var_entry(tc);
  if(!*p) {
    int x = var_index(entry, a->value.var);
    *p = index_tr(x);
    entry[x].n++;
  }
  trace_set_type(tc, a->value.type, a->value.symbol);
}

static
int trace_value(tcell_t *entry, cell_t *v) {
  return is_var(v) || is_list(v) ?
    trace_get_value(entry, v) :
    trace_store_value(entry, v);
}

// store expression c in the trace
static
void trace_store_expr(cell_t *c, const cell_t *r) {
  tcell_t *entry = var_entry(r->value.var);
  assert_error(entry);
  tcell_t *tc = r->value.var;
  if(!tc) return;
  type_t t = r->value.type;
  if(tc->op) {
    // this cell has already been written
    // update the types
    if(t != T_ANY) {
      if(is_value(tc)) {
        trace_set_type(tc, t, r->value.symbol);
        FOR_TRACE(x, entry) {
          // update through assertions
          if(x->op == OP_assert) {
            if(tr_index(x->expr.arg[0]) == var_index(entry, r->value.var)) {
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
  memcpy(&tc->c, c, sizeof(cell_t) * closure_cells(c));
  tc->pos = 0;
  tc->n = n;
  if(is_user_func(tc)) {
    // encode the entry
    cell_t **e = (cell_t **)&tc->expr.arg[closure_in(tc)];
    *e = entry_tr(entry_number((tcell_t *)*e));
  }

  // encode inputs
  TRAVERSE(tc, in) {
    cell_t *a = *p;
    if(a) {
      int x = trace_value(entry, a);
      *p = index_tr(x);
      if(x >= 0) entry[x].n++;
    }
  }

  if(tc->op == OP_external) { // a little HACKy
    tcell_t *name = &entry[tr_index(tc->expr.arg[closure_in(tc) - 1])];
    if(!name->n && is_value(name)) FLAG_SET(*name, trace, IMMEDIATE);
  }

  // encode outputs
  TRAVERSE(tc, out) {
    int x = 0;
    if(*p) {
      x = trace_get_value(entry, *p);
    }
    *p = index_tr(x);
  }
  if(is_value(c)) {
    tc->value.alt_set = 0;
    tc->value.type = t;
  }
  tc->trace.type = t;
  if(tc->op == OP_placeholder) {
    FLAG_SET(*tc, trace, INCOMPLETE);
    trace_extend(entry, tc);
  }
  if(FLAG(*c, expr, PARTIAL)) {
    FLAG_SET(*entry, entry, PARTIAL);
  }
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
  tcell_t *entry = var_entry(f->value.var);
  tcell_t *t = f->value.var;
  if(t->op) return;
  t->op = OP_assert;
  t->expr.arg[0] = index_tr(is_row_list(p) ?
                              trace_get_value(entry, *left_elem(p)) :
                              trace_store_value(entry, &nil_cell));
  int tq = trace_get_value(entry, q);
  entry[tq].n++;
  t->expr.arg[1] = index_tr(tq);
  t->trace.type = T_LIST;
}

// for unless and assert
tcell_t *trace_partial(op op, int n, cell_t *p) {
  CONTEXT_LOG("trace_partial %O, arg[%d] = %C", op, n, p);
  tcell_t *entry = var_entry(p->value.var);
  int a = is_var(p) ? var_index(entry, p->value.var) :
    op == OP_unless && p->value.var ? trace_store_something(entry, &p->value.var) : // (*)
    trace_store_value(entry, p);
  // (*) Because the value will be discarded, just use Something
  int x = trace_alloc(entry, 2);
  tcell_t *tc = &entry[x];
  tc->op = op;
  tc->expr.arg[n] = index_tr(a);
  if(op == OP_assert) {
    FLAG_SET(*tc, expr, PARTIAL);
    FLAG_SET(*entry, entry, PARTIAL);
  }
  entry[a].n++;
  return tc;
}

void apply_condition(cell_t *c, int *x) {
  assert_error(is_value(c));
  if(c->value.var) {
    tcell_t *entry = var_entry(c->value.var);
    tcell_t *t = concatenate_conditions(c->value.var, &entry[*x]);
    c->value.var = t;
    *x = var_index(entry, t);
  }
}

// store value c in the trace
int trace_store_value(tcell_t *entry, cell_t *c) {
  if(!entry) return -1;

  // look to see if the value already is in the trace
  int x = trace_lookup_value_linear(entry, c);
  if(x == -1) {
    assert_error(!is_list(c) || list_size(c) == 0);
    x = trace_copy(entry, c);
    tcell_t *tc = &entry[x];
    tc->value.alt_set = 0;
    tc->value.var = NULL;
    tc->alt = NULL;
    tc->pos = 0;
    tc->n = -1;
  }

  apply_condition(c, &x);
  return x;
}

int trace_store_something(tcell_t *entry, tcell_t **v) {
  cell_t c = (cell_t) {
    .op = OP_value,
    .value = {
      .type = T_SYMBOL,
      .symbol = SYM_Something,
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
  if(FLAG(*r, value, DEP)) {
    trace_dep(c);
  } else {
    trace_store_expr(c, r);
  }
}

static
bool quote_has_call(const tcell_t *e, const tcell_t *call) {
  if(NOT_FLAG(*e, entry, QUOTE)) return false;
  FOR_TRACE_CONST(p, e) {
    if(is_user_func(p)) {
      const tcell_t *pe = get_entry(p);
      if(pe == call ||
         quote_has_call(pe, call)) return true;
    }
  }
  return false;
}

// check for recursion and throw errors on nontermination
static
bool trace_recursive_changes(tcell_t *entry) {
  bool changes = false;
  bool non_tail_call = false;
  bool forced_inline = FLAG(*entry, entry, FORCED_INLINE);

  FOR_TRACE(p, entry) {
    if(is_user_func(p)) {
      const tcell_t *pe = get_entry(p);
      if(pe == entry) {
        bool branch_changes = false;
        csize_t in = closure_in(p);
        assert_error(in == entry->entry.in,
                     "incorrect self call arity at %s %d",
                     entry->word_name, p-entry);
        COUNTUP(i, in) {
          trace_index_t v = in - i;
          if(tr_index(p->expr.arg[i]) != v) {
            tcell_t *a = &entry[v];
            assert_error(is_var(a));
            branch_changes = true;
            // mark variables that change during recursion
            FLAG_SET(*a, trace, CHANGES);
          }
        }

        // if !branch_changes, a recusive call has been made without modifying any arguments
        // so a tail call will loop forever without producing anything
        non_tail_call |= NOT_FLAG(*p, trace, JUMP);
        if(!forced_inline)
          assert_throw(NOT_FLAG(*p, trace, JUMP) || branch_changes, "infinite tail recursion, tail call with constant args");
        changes = true;
      } else if(quote_has_call(pe, entry)) {
        non_tail_call = true;
        changes = true;
      }
    }
  }

  // if there's only one path, even if the arguments change, it will loop forever
  if(!forced_inline)
    assert_throw(!changes || non_tail_call || entry->entry.alts > 1, "infinite tail recursion, single alt");
  return changes;
}

tcell_t *get_trace_ptr(size_t size) {
  assert_error((void *)(trace_ptr + size) < (void *)(&trace_cells+1));
  memset(trace_ptr, 0, sizeof(*trace_ptr) * size);
  return trace_ptr;
}

// setup for tracing
tcell_t *trace_start_entry(tcell_t *parent, csize_t out) {
  tcell_t *e = get_trace_ptr(ENTRY_BLOCK_SIZE);
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

void hw_analysis(tcell_t *e) {
  // mark functions that use recursive functions
  if(FLAG(*e, entry, RECURSIVE)) {
    FLAG_SET(*e, entry, SYNC);
  }
  FOR_TRACE_CONST(c, e) {
    if(ONEOF(trace_type(c), T_LIST, T_OPAQUE)) {
      FLAG_SET(*e, entry, SYNC);
    }
    if(is_user_func(c)) {
      tcell_t *entry = get_entry(c);
      if(!entry) continue;
      if(entry == e && NOT_FLAG(*c, trace, JUMP)) {
        if(FLAG(*e, entry, STACK)) {
          FLAG_SET(*e, entry, RETURN_ADDR);
        }
        FLAG_SET(*e, entry, STACK);
      }
      if(FLAG(*entry, entry, RECURSIVE) ||
         FLAG(*entry, entry, SYNC)) {
        FLAG_SET(*e, entry, SYNC);
      }
      if(FLAG(*entry, entry, RAM)) {
        FLAG_SET(*e, entry, RAM);
      }
    } else if(c->op == OP_ap) {
      FLAG_SET(*e, entry, RAM);
    }
  }
}

static
bool has_outside_call(const tcell_t *entry) {
  FOR_TRACE_CONST(c, entry) {
    if(!is_user_func(c)) continue;
    const tcell_t *e = get_entry(c);
    FOLLOW(pe, entry, entry.parent) {
      if(pe == e) return true;
    }
  }
  return false;
}

// finish tracing
void trace_end_entry(tcell_t *e) {
  e->entry.wrap = NULL;
  assert_error(prev_entry_pos && e->pos == prev_entry_pos, "out of order start/end entry");
  active_entries[--prev_entry_pos] = NULL;
  e->pos = 0;
  FLAG_SET(*e, entry, COMPLETE);
  if(FLAG(*e, entry, QUOTE)) {
    FLAG_SET_TO(*e, entry, MUTUAL, has_outside_call(e));
  } else {
    FLAG_SET_TO(*e, entry, RECURSIVE, trace_recursive_changes(e));
  }
  hw_analysis(e);
}

tcell_t *trace_current_entry() {
  if(prev_entry_pos) {
    return active_entries[prev_entry_pos - 1];
  } else {
    return NULL;
  }
}

tcell_t *trace_wrap_entry(tcell_t *entry) {
  COUNTDOWN(i, prev_entry_pos) {
    tcell_t *e = active_entries[i];
    if(e->entry.wrap &&
       e->entry.wrap->entry == entry) {
      return e;
    }
  }
  return NULL;
}

void trace_clear_alt(tcell_t *entry) {
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
  if(NOT_FLAG(*c, value, DEP)) return;
  tcell_t *entry = var_entry(c->value.var);
  if(!entry) return;
  int x = trace_alloc(entry, 1);
  tcell_t *tc = &entry[x];
  tcell_t *ph = c->value.var;
  int ph_x = var_index(entry, ph);
  ph->expr.arg[c->pos] = index_tr(x);
  LOG("trace_dep: %d <- %C %d[%d]", x, c, ph_x, c->pos);
  tc->op = OP_dep;
  tc->expr.arg[0] = index_tr(ph_x);
  assert_error(c->value.type != T_OPAQUE || c->value.symbol);
  tc->expr.symbol = c->value.symbol;
  tc->trace.type = c->value.type;
  ph->n++;
  c->value.var = tc;
  c->pos = 0;
  FLAG_CLEAR(*c, value, DEP);
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_var(r)) return;
  tcell_t *v = r->value.var;
  if(v && !v->op) {
    tcell_t *e = var_entry(v);
    if(e) {
      int ix = var_index(e, v);
      if(e->entry.len - (ix - 1) == calculate_tcells(v->size)) {
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
  tcell_t *new_entry = trace_expr_entry(c->pos);
  trace_update_type(r);
  if(!is_var(r)) {
    // print tracing information for a reduction
    if(FLAG(*c, expr, TRACE)) {
      printf(NOTE("TRACE") " %s", op_name(c->op));
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

  tcell_t *entry = var_entry(r->value.var);
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
    tcell_t *tc = c->value.var;
    if(tc && tc->op) {
      trace_set_type(tc, t, c->value.symbol);
    }
  }
}

// zero space in the trace allocated to an entry
void trace_clear(tcell_t *e) {
  size_t count = e->entry.len;
  memset(e, 0, (count + 1) * sizeof(tcell_t));
}

// update the traced type
void trace_set_type(tcell_t *tc, type_t t, val_t sym) {
  tc->trace.type = t;
  if(is_value(tc)) {
    tc->value.type = t;
    if(is_var(tc)) {
      if(t == T_OPAQUE) {
        tc->value.symbol = sym;
      }
      tcell_t *p = tc->value.var;
      if(p) {
        trace_set_type(p, t, sym);
      }
    }
  }
}

// returns true if there's a function yet to be expanded (recursive) or a partial function
bool has_computation(const cell_t *c) {
  if(!c) return false;
  if(is_value(c) && !is_list(c)) { // HACK simplify should be able to reduce through compose,
    return is_fail(c);             // removing the need to examine lists.
  }
  if(ONEOF(c->op, OP_exec, OP_assert)) return true;
  TRAVERSE(c, const, in, alt, ptrs) {
    if(has_computation(*p)) return true;
  }
  return false;
}

bool reduced_list(cell_t *l) {
  cell_t **p;
  FORLIST(p, l, true) {
    simplify(p);
    if(has_computation(*p)) {
      return false;
    }
  }
  return true;
}

// store captured variables to be compiled into a quote
int trace_build_quote(tcell_t *entry, cell_t *l) {
  LOG("trace_build_quote %E %C", entry, l);
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
  bool inline_quote = FLAG(*l, value, INLINE) || reduced_list(l);

  // inline if the function would be empty
  // TODO check for barriers? ***
  if(!inline_quote && entry->entry.out == 1) {
    inline_quote = true;
    FOR_TRACE(c, entry) {
      if(!is_var(c)) {
        inline_quote = false;
      }
    }
  }
  if(inline_quote) {
    LOG("all var list %C", l);
    FLAG_SET(*entry, entry, FORCED_INLINE);
    cell_t **p;
    FORLIST(p, l, true) {
      reduce(p, is_row_arg(&__it) ?
                  &CTX(list, 0, 0) :
                  &CTX(any));
    }
    while(is_row_list(l) && list_size(l) == 1) l = l->value.ptr[0];
    if(is_var(l)) {
      switch_entry(entry, l);
      return var_index(entry, l->value.var);
    }
    bool row = is_row_list(l);
    const int size = function_out(l, true);
    assert_error(!row || size > 1);
    FORLIST(p, l, true) force(p); // ***
    int x = trace_alloc(entry, size);
    tcell_t *tc = &entry[x];
    cell_t *p0 = l->value.ptr[0];
    tc->op = row ? (is_var(p0) && FLAG(*p0, value, ROW) ? OP_compose :
                    OP_pushr) : OP_quote;
    tc->trace.type = T_LIST;
    int n = size;

    FORLIST(p, l, true) {
      int x = trace_value(entry, *p);
      tc->expr.arg[--n] = index_tr(x);
      entry[x].n++;
    }

    apply_condition(l, &x);
    return x;
  }

  return compile_quote(entry, l);
}

cell_t *trace_quote_var(tcell_t *entry, cell_t *l) {
  assert_error(l != NULL);
  if(is_empty_list(l)) return l;
  if(is_var(l)) return l;
  int x = trace_build_quote(entry, l);
  drop(l);
  return var_create_nonlist(T_LIST, &entry[x]);
}

// store a return
static
int trace_return(tcell_t *entry, cell_t *c_) {
  cell_t *c = flat_copy(c_);
  cell_t **p;
  FORLIST(p, c, true) {
    trace_index_t x;
    assert_error(is_value(*p));
    if(is_var(*p)) {
      switch_entry(entry, *p);
      x = var_index(entry, (*p)->value.var);
    } else if(is_list(*p)) {
      x = trace_build_quote(entry, *p);
    } else {
      x = trace_store_value(entry, *p);
    }
    *p = index_tr(x);
    if(x >= 0) entry[x].n++;
  }
  int x = trace_copy(entry, c);
  tcell_t *tc = &entry[x];
  LOG("trace_return: %s[%d] <- %C", entry->word_name, tc-entry, c_);
  closure_free(c);
  tc->value.type = T_RETURN;
  tc->n = -1;
  tc->alt = NULL;
  return x;
}

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp_val && tail != &c->tmp) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
      tail = trace_var_list(c->alt, tail);
    } else {
      c->tmp_val = true; // prevent loops
      TRAVERSE(c, alt, in, ptrs) {
        tail = trace_var_list(*p, tail);
      }
      c->tmp_val = false;
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
unsigned int trace_reduce(tcell_t *entry, cell_t **cp) {
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;
  context_t *ctx = ctx_pos(&CTX(return), entry->pos);

  CONTEXT("trace_reduce %s %C", entry->word_name, *cp);
  insert_root(cp);

  COUNTUP(priority, PRIORITY_MAX) {
    cell_t **p = cp;
    bool delay = false;
    CONTEXT("priority = %d", priority);

  loop_start:
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

      cell_t **a;
      FORLIST(a, *p, true) {
        collapse_row(a);
        if(force(a) != SUCCESS) goto loop_start; // ***
        if(is_value(*a) &&
           !is_list(*a) &&
           !is_var(*a)) { // TODO deps?
          LOG(TODO " use return value of trace_store_value");
          trace_store_value(entry, *a); // TODO use return value
        }
      }
      int x = trace_return(entry, *p);
      tcell_t *r = &entry[x];
      FLAG_CLEAR(*entry, entry, PARTIAL);

      LOG("branch %d finished %C", alts, *p);

      r->n++;
      alts++;

      *prev = index_tr(x);
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

unsigned int trace_reduce_one(tcell_t *entry, cell_t *c) {
  cell_t *l = quote(c);
  int alts = trace_reduce(entry, &l);
  drop(l);
  return alts;
}

tcell_t *trace_alloc_var(tcell_t *entry, type_t t) {
  int x = trace_alloc(entry, 2);
  if(x <= 0) return NULL;
  tcell_t *tc = &entry[x];
  tc->op = OP_value;
  tc->value.type = t;
  tc->value.flags = VALUE_VAR;
  tc->pos = ++entry->entry.in;
  return tc;
}

bool valid_pos(uint8_t pos) {
  return INRANGE(pos, 1, prev_entry_pos);
}

tcell_t *trace_expr_entry(uint8_t pos) {
  return valid_pos(pos) ?
    active_entries[pos - 1] : NULL;
}

cell_t *param(int t, tcell_t *entry) {
  return var_create_nonlist(t, trace_alloc_var(entry, t));
}

void print_active_entries(const char *msg) {
  const char *sep = msg;
  COUNTUP(i, prev_entry_pos) {
    tcell_t *e = active_entries[i];
    printf("%s%s.%s (%d)", sep,
           e->module_name, e->word_name,
           entry_number(e));
    sep = ", ";
  }
  if(sep != msg) printf("\n");
}

tcell_t *tc_get(tcell_t *entry, int x) {
  return entry && x > 0 ? &entry[x] : NULL;
}

FORMAT(entry, 'E') {
  if(i) {
    tcell_t *entry = (tcell_t *)i;
    printf("%s.%s(%d)",
           entry->module_name,
           entry->word_name,
           entry_number(entry));
  } else {
    printf("null_entry");
  }
}

FORMAT(entry_short, 'e') {
  tcell_t *entry = (tcell_t *)i;
  if(!entry) {
    printf("???");
  } else if(entry->word_name) {
    printf("%s", entry->word_name);
  } else {
    printf("%d", entry_number(entry));
  }
}

FORMAT(trace_cell, 'T') {
  tcell_t *tc = (tcell_t *)i;
  tcell_t *entry = var_entry(tc);
  format_entry_short((val_t)entry);
  if(entry) printf("[%d]", var_index(entry, tc));
}

/* conditions */

int copy_conditions(tcell_t *entry, int i, int v) {
  if(!i || i == v) return v;
  tcell_t *tc = &entry[i];
  if(!ONEOF(tc->op, OP_assert, OP_seq, OP_unless)) return v;
  int a = tr_index(tc->expr.arg[0]);
  int an = copy_conditions(entry, a, v);
  if(a == an) return i;
  int x = i;
  if(a) {
    x = trace_copy(entry, tc);
    entry[x].n = ~0;
    entry[tr_index(tc->expr.arg[1])].n++;
  }
  entry[x].expr.arg[0] = index_tr(an);
  if(an) entry[an].n++;
  LOG("copy condition in %s: %d -> %d", entry->word_name, i, x);
  return x;
}

void reserve_condition(tcell_t **p) {
  if(*p) {
    tcell_t *entry = var_entry(*p);
    *p = &entry[copy_conditions(entry, var_index(entry, *p), 0)];
  }
}

// assert & unless form lists that can be concatenated
// TODO use refcounting to avoid destructive concatenation
tcell_t *concatenate_conditions(tcell_t *a, tcell_t *b) {
  if(a == NULL) return b;
  if(b == NULL) return a;
  tcell_t *entry = var_entry(a);
  b = var_for_entry(entry, b);
  assert_error(b, "%s %d %d", entry->word_name, a-entry, b-entry);
  int bi = var_index(entry, b);
  tcell_t *an = &entry[copy_conditions(entry, var_index(entry, a), bi)];
  LOG("condition %s %d ... %d %O arg <- %d",
      entry->word_name, a-entry, an-entry, an->op, b-entry);
  return an;
}

tcell_t *value_condition(cell_t *a) {
  return a && is_value(a) && !is_var(a) ? a->value.var : NULL;
}

void add_conditions_2(cell_t *res, cell_t *a0) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v, value_condition(a0));
  }
}

void add_conditions_3(cell_t *res, cell_t *a0, cell_t *a1) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
                                  value_condition(a1)));
  }
}

void add_conditions_4(cell_t *res, cell_t *a0, cell_t *a1, cell_t *a2) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
             concatenate_conditions(value_condition(a1),
                                    value_condition(a2))));
  }
}

void add_conditions_from_array(cell_t *res, cell_t **a, unsigned int n) {
  tcell_t **v = &res->value.var;
  if(!is_var(res) && n) {
    tcell_t *cs = value_condition(a[n-1]);
    COUNTDOWN(i, n-1) {
      cs = concatenate_conditions(value_condition(a[i]), cs);
    }
    *v = concatenate_conditions(*v, cs);
  }
}

void add_conditions_var_2(cell_t *res, tcell_t *t) {
  tcell_t **v = &res->value.var;
  *v = concatenate_conditions(t, *v);
}

void add_conditions_var_3(cell_t *res, tcell_t *t, cell_t *a0) {
  tcell_t **v = &res->value.var;
  *v = concatenate_conditions(t,
         concatenate_conditions(value_condition(a0), *v));
}

#if INTERFACE
#define add_conditions(...) DISPATCH(add_conditions, __VA_ARGS__)
#define add_conditions_var(...) DISPATCH(add_conditions_var, __VA_ARGS__)
#endif

int trace_entry_size(const tcell_t *e) {
  return e->entry.len + 1;
}

// Compact from blocks to minimum size
void trace_compact(tcell_t *entry) {
  // build map
  tcell_t *end = trace_ptr;
  tcell_t *ne = entry;
  for(tcell_t *e = entry;
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
  for(tcell_t *e = entry; e < end; e += ENTRY_BLOCK_SIZE) {
    if(!e->n) continue;

    // update calls
    FOR_TRACE(c, e) {
      if(is_user_func(c)) {
        tcell_t *ce = get_entry(c);
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
  for(tcell_t *e = entry; e < end; e += ENTRY_BLOCK_SIZE) {
    if(!e->n) continue;
    FLAG_CLEAR(*e, entry, BLOCK);

    // move
    tcell_t *compact = e->entry.compact;
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
      c->priority = priority;
      LOG("delay branch %C %d", c, priority);
    }
  }
}

void trace_extend(tcell_t *entry, tcell_t *tc) {
  assert_error(entry && tc->op == OP_placeholder);
  if(FLAG(*entry, entry, PARTIAL) ||
     closure_in(tc) != 1) return;
  tcell_t *l = &entry[tr_index(tc->expr.arg[0])];
  l->trace.extension = tc - entry;
}

cell_t *trace_extension(cell_t *l, int in, int out) {
  tcell_t *v = l->value.var;
  if(!v->trace.extension) return NULL;
  assert_lt(v->trace.extension, ENTRY_BLOCK_SIZE);
  tcell_t *entry = var_entry(v);
  if(entry != trace_current_entry()) return NULL; // HACK to avoid switch_entry
  tcell_t *tc = &entry[v->trace.extension];
  if(closure_in(tc) == in + 1 && closure_out(tc) >= out) {
    LOG("trace_extension %s %d -> %d", entry->word_name, v-entry, tc-entry);
    drop(l);
    cell_t *ex = make_list(out + 1);
    COUNTDOWN(i, out) {
      tcell_t *x = &entry[tr_index(tc->expr.arg[1 + REVI(i)])];
      ex->value.ptr[i] = var_create_nonlist(trace_type(x), x);
    }
    ex->value.ptr[out] = var_create_nonlist(T_LIST, tc);
    ex->value.flags = VALUE_ROW;
    return ex;
  } else {
    return NULL;
  }
}

// *** temporary
bool is_tail_call(const tcell_t *entry, const tcell_t *c) {
  return
    FLAG(*c, trace, JUMP) &&
    FLAG(*entry, entry, RECURSIVE);
}

val_t trace_get_opaque_symbol(const tcell_t *e, const tcell_t *c) {
  assert_error(trace_type(c) == T_OPAQUE);
  const tcell_t *p = c;
  while(!is_value(p) && !is_dep(p)) {
    p = &e[tr_index(p->expr.arg[0])];
  }

  if(trace_type(p) == T_OPAQUE) {
    if(is_var(p)) {
      return p->value.symbol;
    } else if(is_dep(p)) {
      return p->expr.symbol;
    }
  }
  return -1;
}

static
const tcell_t *out_arg_of(const tcell_t *e, const tcell_t *c, int *x) {
  if(is_dep(c)) {
    int i = 1;
    int di = c - e;
    const tcell_t *pc = &e[tr_index(c->expr.arg[0])];
    TRAVERSE(pc, const, out) {
      if(p && tr_index(*p) == di) {
        *x = i;
        return pc;
      }
      i++;
    }
    assert_error(false, "out argument not found");
    return NULL;
  } else {
    *x = 0;
    return c;
  }
}

const tcell_t *trace_get_linear_var(const tcell_t *e, const tcell_t *c) {
  assert_error(trace_type(c) == T_OPAQUE);
  const tcell_t *p = c;
  while(!is_value(p)) {
    int x = 0;
    if(is_dep(p)) {
      p = out_arg_of(e, p, &x);
    }
    p = &e[tr_index(p->expr.arg[x])];
  }
  assert_error(is_var(p));
  return p;
}

void trace_update_range(cell_t *c) {
  tcell_t *v = c->value.var;
  if(FLAG(*v, value, BOUNDED) ||
     v->trace.min != INTPTR_MIN ||
     v->trace.max != INTPTR_MAX) {
    v->trace.min = c->value.min;
    v->trace.max = c->value.max;
    if(is_var(v)) FLAG_SET(*v, value, BOUNDED); //***
  }
}

void trace_unbound(cell_t *c) {
  tcell_t *v = c->value.var;
  v->trace.min = INTPTR_MIN;
  v->trace.max = INTPTR_MAX;
  if(is_var(v)) FLAG_CLEAR(*v, value, BOUNDED); //***
}

tcell_t *tcell_entry(cell_t *e) {
  assert_error(e->op == OP_null && e->n == PERSISTENT);
  return (tcell_t *)((char *)e - offsetof(tcell_t, c));
}

csize_t closure_tcells(tcell_t const *c) {
  return calculate_tcells(closure_args(c));
}

tcell_t *closure_next(tcell_t *c) {
  return c + closure_tcells(c);
}

const tcell_t *closure_next_const(const tcell_t *c) {
  return c + closure_tcells(c);
}
