/* Copyright 2012-2020 Dustin DeWeese
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
#include "startle/static_alloc.h"
#include "startle/map.h"

#include "cells.h"
#include "rt.h"
#include "eval.h"
#include "special.h"
#include "parse/parse.h"
#include "debug/print.h"
#include "parse/lex.h"
#include "user_func.h"
#include "list.h"
#include "ir/compile.h"
#include "var.h"
#include "ir/analysis.h"
#include "parameters.h"

// parameters
#define ENTRY_BLOCK_SIZE 1024
#define MAP_BLOCK_SIZE 64

// storage for all trace entries
STATIC_ALLOC_ALIGNED(trace_cells, tcell_t, 8000, 64);
static tcell_t *trace_ptr = NULL;

// scratch spaces at the end of trace_cells
static scratch_t *scratch_top = NULL;
static scratch_t *scratch_ptr = NULL;

// stack of entries that are being compiled
STATIC_ALLOC(active_entries, tcell_t *, 1 << 4);
static unsigned int prev_entry_pos = 0;

// index to quickly find an entry for a var inside
STATIC_ALLOC_DEPENDENT(trace_block_map, tcell_t *, DIV_UP(trace_cells_size, 64));

STATIC_ALLOC(switch_map, pair_t, 64);
STATIC_ALLOC(switch_rev_map, pair_t, 64);

#include "ir/trace-local.h"

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
  return p >= (void *)trace_cells && p < (void *)(trace_cells + trace_cells_size);
}

void trace_reinit() {
  trace_ptr = NULL;
}

void trace_init() {
  prev_entry_pos = 0;
  reset_scratch();
  if(!trace_ptr) trace_ptr = trace_cells;
  scratch_top = (scratch_t *)(trace_cells + trace_cells_size);
  scratch_ptr = (scratch_t *)(trace_cells + trace_cells_size);
  init_map(switch_map, switch_map_size);
  init_map(switch_rev_map, switch_rev_map_size);
}

#if INTERFACE
#define get_entry(c) _get_entry(GET_CELL(c))
#endif

tcell_t *_get_entry(cell_t const *c) {
  return is_user_func(c) ?
    tr_entry(c->expr.arg[closure_in(c)]) :
    NULL;
}

void set_entry(tcell_t *c, tcell_t *e) {
  assert_error(is_user_func(c));
  c->expr.arg[closure_in(c)] = entry_tr(e);
}

int entry_number(tcell_t const *e) {
  return e ? e - trace_cells : -1;
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
  RANGEUP(i, DIV_UP(entry - trace_cells, MAP_BLOCK_SIZE), trace_block_map_size) {
    tcell_t *block_start = &trace_cells[i * MAP_BLOCK_SIZE];
    while(ne <= block_start) {
      e = ne;
      ne = trace_entry_next(ne);
    }
    trace_block_map[i] = e;
    if(e >= trace_ptr) break;
  }
}

// is v somewhere within the entry?
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

// compare two closures
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

// check if two entries are equivalent
bool entries_match(tcell_t *entry_a, tcell_t *entry_b) {
  int len = entry_a->entry.len;
  if(len != entry_b->entry.len) return false;
  int offset = entry_b - entry_a;
  FOR_TRACE(a, entry_a) {
    if(!closure_match(&a->c, &(a + offset)->c)) return false;
  }
  return true;
}

// get first entry in the same block as v
tcell_t *block_first_entry(tcell_t *v) {
  int i = (v - trace_cells) / MAP_BLOCK_SIZE;
  assert_error(i < (int)trace_block_map_size);
  return trace_block_map[i];
}

tcell_t *top_parent(tcell_t *e) {
  tcell_t *r = NULL;
  FOLLOW(p, e, entry.parent) {
    r = p;
  }
  return r;
}

// look for an active entry matching x
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

// tag duplicate entries for deletion
void dedup_entry(tcell_t **e) {
  tcell_t *p = trace_matching_entry(*e);
  if(p < *e) {
    (*e)->n = 0; // tag for deletion
    *e = p;
  }
}

// get the entry for a var
tcell_t *var_entry(tcell_t *v) {
  if(!v) return NULL;
  assert_error(is_trace_cell(v));
  for(tcell_t *e = block_first_entry(v);
      e < trace_ptr;
      e = trace_entry_next(e)) {
    if(!is_persistent(e)) return NULL;
    if(entry_has(e, v)) return e;
  }
  return NULL;
}

// get the pos for an entry
int entry_pos(tcell_t *e) {
  FOLLOW(p, e, entry.parent) {
    if(e->pos) return e->pos;
  }
  return 0;
}

// get the var within the entry corresponding to c
tcell_t *get_var(tcell_t *entry, cell_t *c) {
  assert_error(is_value(c));
  return var_for_entry(entry, c->value.var);
}

#if INTERFACE
#define FOR_SWITCH_MAP(tc, v, p, it)                                    \
  for(tcell_t *__tc = is_var(v) && v->value.var ? v->value.var : v,     \
        *tc = ((p = NULL,                                               \
                it = map_iterator_begin(switch_map, (uintptr_t)__tc)), __tc); \
      tc;                                                               \
      p = map_next(&it, p),                                             \
        tc = p ? (tcell_t *)p->second : NULL)
#endif

// get the var within the entry corresponding to v
tcell_t *var_for_entry(tcell_t *entry, tcell_t *v) {
  if(!entry || entry_has(entry, v)) return v;

  // get outermost variable (lowest entry)
  tcell_t *ve = var_entry(v);
  if(FLAG(*ve, entry, COMPLETE)) {

    // complete entries have been rearranged, so must lookup
    v = MAP_GET(switch_rev_map, v, v);
    if(entry_has(entry, v)) return v;
  }

  map_iterator it;
  pair_t *p;
  FOR_SWITCH_MAP(tc, v, p, it) {
    if(entry_has(entry, tc)) return tc;
  }
  return NULL;
}

// get the index of the var in entry for v
int var_index(tcell_t *entry, tcell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  assert_error(v);
  return v - entry;
}

// get the index of the var in entry for v
// returns -1 on failure
int var_index_nofail(tcell_t *entry, tcell_t *v) {
  if(!entry) {
    entry = var_entry(v);
  }
  v = var_for_entry(entry, v);
  if(!v) return -1;
  return v - entry;
}

// convert an index to a trace argument
cell_t *index_tr(int index) {
  tr x = {};
  x.index = index;
  return x.ptr;
}

// get an index from a trace argument
int tr_index(const cell_t *c) {
  return ((tr) { .ptr = (cell_t *)c }).index;
}

// convert an entry to a trace argument
cell_t *entry_tr(tcell_t *entry) {
  tr x = {};
  x.entry = entry - trace_cells;
  return x.ptr;
}

// get an entry from a trace argument
tcell_t *tr_entry(const cell_t *c) {
  return &trace_cells[((tr) { .ptr = (cell_t *)c }).entry];
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

tcell_t *get_arg(tcell_t *entry, const cell_t *a) {
  return a ? &entry[tr_index(a)] : NULL;
}

const tcell_t *get_arg_const(const tcell_t *entry, const cell_t *a) {
  return a ? &entry[tr_index(a)] : NULL;
}

TEST(trace_encode) {
  return tr_index(index_tr(0x5ac3)) == 0x5ac3 ? 0 : -1;
}

// are the values equal?
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
  if(entry_has(entry, r->value.var)) return;

  WATCH(r, "switch_entry", "%s", entry->word_name);
  r->value.var = switch_entry_var(entry, r->value.var, false);
}

tcell_t *switch_entry_var(tcell_t *entry, tcell_t *v, bool always_allocate) {
  assert_error(NOT_FLAG(*entry, entry, COMPLETE));
  tcell_t *n;

  if(!always_allocate &&
     (n = var_for_entry(entry, v))) return n;

  n = trace_alloc_var(entry, trace_type(v));
  tcell_t *t = MAP_GET(switch_rev_map, v, v); // TODO duplicate lookup, first in var_for_entry
  LOG("switch %T <- %T from %T", t, n, v);
  assert_throw(map_insert(switch_map, PAIR(t, n)) &&
               map_insert(switch_rev_map, PAIR(n, t)), "switch_map overflow");
  n->value.var = t;
  return n;
}

void print_switch_maps() {
  printf("___ switch_map ___\n");
  FORMAP(i, switch_map) {
    printf("%d -> %d\n",
           (int)((tcell_t *)switch_map[i].first - trace_cells),
           (int)((tcell_t *)switch_map[i].second - trace_cells));
  }
  printf("___ switch_rev_map ___\n");
  FORMAP(i, switch_rev_map) {
    printf("%d -> %d\n",
           (int)((tcell_t *)switch_rev_map[i].first - trace_cells),
           (int)((tcell_t *)switch_rev_map[i].second - trace_cells));
  }
}

void mark_pos(cell_t *c, int pos) {
  while(is_id_list(c)) c = c->value.ptr[0];
  if(!pos || c->pos == pos) return;
  assert_error(!is_persistent(c));
  tcell_t *entry = pos_entry(pos);
  if(!entry) {
    LOG("mark_pos: stale %C %d", c, pos);
    return;
  }
  WATCH(c, "mark_pos", "%s", entry->word_name);
  if(is_var(c)) {
    switch_entry(entry, c);
  } else {
    c->pos = pos;
    if(c->op == OP_seq) { // HACK
      mark_pos(c->expr.arg[0], pos);
    }
  }
}

// find a matching trace cell given a variable or value
static
int trace_get_value(tcell_t *entry, cell_t *r) {
  assert_error(r && is_value(r), "%C", r);
  if(is_list(r)) {
    return trace_build_quote(entry, r, false); // *** TODO prevent building duplicate quotes
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

// calculate the number of tcells required to hold n arguments
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
  tc->trace.range = RANGE_NONE;
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

// copy c into newly allocated space in the trace
int trace_copy_cell(tcell_t *entry, const cell_t *c) {
  int index = trace_alloc(entry, c->size);
  size_t n = closure_cells(c);
  memcpy(&entry[index].c, c, sizeof(cell_t) * n);
  return index;
}

// get temporary storage to use with a trace cell
scratch_t *get_scratch(tcell_t *tc) {
  assert_error(prev_entry_pos);
  int x = tc - active_entries[0];
  assert_error(x > 0);
  scratch_t *s = &scratch_top[-x];
  if(s < scratch_ptr) {
    assert_error((char *)s >= (char *)trace_ptr);
    scratch_ptr = s;
    memset(s, 0, (scratch_ptr - s) * sizeof(scratch_t));
  }
  return s;
}

static
void reset_scratch() {
  scratch_ptr = scratch_top;
}

// fill in an argument to a function
void trace_arg(tcell_t *tc, int n, cell_t *a) {
  assert_error(is_var(a));
  cell_t **p = &tc->expr.arg[n];
  tcell_t *entry = var_entry(tc);
  if(!*p) {
    int x = var_index(entry, a->value.var);
    *p = index_tr(x);
    entry[x].n++;
  }
}

// store the value v in the entry
static
int trace_value(tcell_t *entry, cell_t *v) {
  return is_var(v) || is_list(v) ?
    trace_get_value(entry, v) :
    trace_store_value(entry, v);
}

// store expression c in the trace
static
void trace_store_expr(tcell_t *entry, cell_t *c, cell_t *r) {
  if(!entry) return;
  assert_error(NOT_FLAG(*r, value, DEP));
  assert_error(is_var(r));
  tcell_t *tc = r->value.var;
  if(!tc || tc->op) return;
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
    *e = entry_tr((tcell_t *)*e);
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
  type_t t = r->value.type;
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
  hash_trace_cell(entry, tc);
  dedup_trace_cell(entry, r);
}

// mark duplicate expressions for deletion i.e. CSE
void dedup_trace_cell(tcell_t *entry, cell_t *r) {
  tcell_t *v = r->value.var;
  if(is_var(v) ||
     closure_out(v) > 0 ||
     ONEOF(v->op, OP_dep, OP_placeholder, OP_unless, OP_assert, OP_seq)) return;
  uint32_t hash = v->trace.hash;
  if(!hash) return;
  FOR_TRACE(tc, entry) {
    if(tc >= v) break;
    if(tc->trace.hash == hash) {
      if(!trace_cell_eq(entry, v, tc)) continue;
      v->n = -1;
      r->value.var = tc;
      LOG("dedup %T (%C) -> %T", v, r, tc);
      break;
    }
  }
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

// store value c in the trace
int trace_store_value(tcell_t *entry, cell_t *c) {
  if(!entry) return -1;

  // look to see if the value already is in the trace
  int x = trace_lookup_value_linear(entry, c);
  if(x == -1) {
    assert_error(!is_list(c) || list_size(c) == 0);
    x = trace_copy_cell(entry, c);
    tcell_t *tc = &entry[x];
    tc->value.alt_set = 0;
    tc->value.var = NULL;
    tc->alt = NULL;
    tc->pos = 0;
    tc->n = -1;
    tc->trace.type = c->value.type;
    tc->trace.range = get_range(c);
  }

  apply_condition(c, &x);
  return x;
}

// Store something where the value doesn't matter
int trace_store_something(tcell_t *entry, tcell_t **v) {
  cell_t c = (cell_t) {
    .op = OP_value,
    .value = {
      .type = T_SYMBOL,
      .var = *v,
      .range = {
        .min = SYM_Something,
        .max = SYM_Something
      }
    },
    .size = 2,
    .n = -1
  };
  int x = trace_store_value(entry, &c);
  *v = c.value.var;
  return x;
}

tcell_t *get_trace_ptr(size_t size) {
  assert_throw((void *)(trace_ptr + size) < (void *)(trace_cells + trace_cells_size),
    "`trace_cells` too small");
  memset(trace_ptr, 0, sizeof(*trace_ptr) * size);
  return trace_ptr;
}

// setup for tracing
tcell_t *trace_start_entry(tcell_t *parent, csize_t out) {
  tcell_t *e = get_trace_ptr(ENTRY_BLOCK_SIZE);
  trace_ptr += ENTRY_BLOCK_SIZE; // TODO
  assert_error((char *)trace_ptr < (char *)scratch_ptr);
  e->n = PERSISTENT;
  e->entry = (struct entry) {
    .out = out,
    .parent = parent,
    .flags = ENTRY_BLOCK
  };

  // active_entries[e->pos-1] = e
  assert_throw(prev_entry_pos < active_entries_size, "`active_entries` too small");
  active_entries[prev_entry_pos++] = e;
  e->pos = prev_entry_pos;
  trace_update_block_map(e);

  return e;
}

static
bool has_mutual_recursion(const tcell_t *entry) {
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
  trace_final_pass(e);
  e->entry.specialize = NULL;
  assert_error(prev_entry_pos && e->pos == prev_entry_pos, "out of order start/end entry");
  active_entries[--prev_entry_pos] = NULL;
  e->pos = 0;
  FLAG_SET(*e, entry, COMPLETE);
  if(FLAG(*e, entry, QUOTE)) {
    FLAG_SET_TO(*e, entry, MUTUAL, has_mutual_recursion(e));
  } else {
    FLAG_SET_TO(*e, entry, RECURSIVE, trace_recursive_changes(e));
  }
  hw_analysis(e);
  hash_entry(e);
}

tcell_t *trace_current_entry() {
  return prev_entry_pos ? active_entries[prev_entry_pos - 1] : NULL;
}

// remove entry and all subentries
void trace_reset(tcell_t *entry) {
  COUNTDOWN(i, prev_entry_pos) {
    tcell_t *e = active_entries[i];
    if(e == entry) {
      prev_entry_pos = i;
      trace_ptr = entry;
      break;
    }
  }
}

void trace_reset_active() {
  if(prev_entry_pos) {
    prev_entry_pos = 0;
    trace_ptr = active_entries[0];
  }
}

void trace_clear_alt(tcell_t *entry) {
  FOR_TRACE(c, entry) {
    if(is_value(c) && c->value.type == T_RETURN) continue;
    c->alt = 0;
  }
}

// store a dep
void trace_dep(cell_t *c) {
  if(!is_var(c)) return;
  if(NOT_FLAG(*c, value, DEP)) return;
  tcell_t *entry = var_entry(c->value.var);
  if(!entry) return;
  int x = trace_alloc(entry, 1);
  tcell_t *tc = &entry[x];
  tcell_t *ph = c->value.var;
  int ph_x = var_index(entry, ph);
  assert_error(c->arg_index < ph->size);
  ph->expr.arg[c->arg_index] = index_tr(x);
  LOG("trace_dep: %d <- %C %d[%d]", x, c, ph_x, c->arg_index);
  tc->op = OP_dep;
  tc->expr.arg[0] = index_tr(ph_x);
  assert_error(c->value.type != T_OPAQUE || range_singleton(c->value.range));
  ph->n++;
  c->value.var = tc;
  c->arg_index = 0;
  FLAG_CLEAR(*c, value, DEP);
}

// reclaim the storage used for tc
void trace_reclaim(tcell_t *e, tcell_t *tc) {
  int ix = var_index(e, tc);
  if(e->entry.len - (ix - 1) == calculate_tcells(tc->size)) {
    if(tc->op) {
      TRAVERSE(tc, in) {
        if(*p) {
          int x = tr_index(*p);
          if(x >= 0) e[x].n--;
        }
      }
    }
    e->entry.len = ix - 1;
  } else {
    tc->n = -1;
  }
}

// reclaim failed allocation if possible
void trace_drop(cell_t *r) {
  if(!r || !is_value(r)) return;
  tcell_t *tc = r->value.var;
  if(tc) {
    WATCH(r, "trace_drop");
    if(!tc->op) {
      tcell_t *e = var_entry(tc);
      if(e) {
        trace_reclaim(e, tc);
      }
    } else {
      trace_update(r);
    }
  }
}

// called when c is reduced to r to copy to pre-allocated space in the trace
void trace_reduction(cell_t *c, cell_t *r) {
  WATCH(c, "trace_reduction", "%C", r);
  tcell_t *new_entry = pos_entry(c->pos);
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

  trace_store_expr(entry, c, r);
  if(is_var(r) && new_entry && new_entry != entry) {
    switch_entry(new_entry, r);
  }
}

// sets {trace,value}.type
void trace_set_type(tcell_t *tc, type_t t) {
  tc->trace.type = t;
  if(is_value(tc)) tc->value.type = t;
}

// updates types and ranges for a value c and var v (optional)
#if INTERFACE
#define trace_update(...) DISPATCH(trace_update, __VA_ARGS__)
#endif

void trace_update_1(cell_t *c) {
  if(c->value.var) {
    trace_update_2(c->value.var, c);
  }
}

void trace_update_2(tcell_t *v, cell_t *c) {
  type_t t = c->value.type;
  if(t == T_ANY) return;
  range_t c_range = get_range(c);
  map_iterator it;
  pair_t *p;
  tcell_t *v_e = var_entry(v);
  if(FLAG(*v_e, entry, COMPLETE)) { // ***
    LOG(MARK("WARN") " attempt to update with %t[%d, %d] for completed trace cell %T from %C",
        t, c_range.min, c_range.max, v, c);
    return;
  }
  FOR_SWITCH_MAP(tc, v, p, it) {
    tcell_t *entry = var_entry(tc);
    if(!entry) {
      LOG(MARK("WARN") " stale var %C", c);
      continue;
    }
    if(FLAG(*entry, entry, COMPLETE)) {
      LOG(MARK("WARN") " attempt to update with %t[%d, %d] for completed trace cell %T from %C",
          t, c_range.min, c_range.max, tc, c);
    } else {
      tcell_t *p = tc;
      do {
        assert_error(p->op);
        range_t r = range_union(p->trace.range, c_range);
        type_t pt = trace_type(p);
        if(pt != t || !range_eq(r, p->trace.range)) {
          p->trace.range = r;
          if(ONEOF(pt, T_ANY, T_BOTTOM) || ONEOF(t, T_BOTTOM, pt)) {
            trace_set_type(p, t);
            LOG("updated var %C (%s[%d]) to %t[%d, %d]",
                c, entry->word_name, p-entry, t, r.min, r.max);
            assert_error(trace_type(p) != T_OPAQUE || range_singleton(r));
          } else {
            assert_error(!is_var(c) || FLAG(*c, value, DEP), "@type"); // ***
          }
        }
        int i = tr_index(p->expr.arg[0]);
        p = i && ONEOF(p->op, OP_assert, OP_unless, OP_seq) ? &entry[i] : NULL;

        assert_counter(1000); // ***
      } while(p);
    }
  }

  // optimize this ***
  tcell_t *entry = var_entry(v);
  int ix = var_index(entry, v);
  FOR_TRACE(x, entry) {
    // update through conditions
    if(ONEOF(x->op, OP_assert, OP_unless, OP_seq) && tr_index(x->expr.arg[0]) == ix) {
      x->trace.type = t;
      x->trace.range = range_union(x->trace.range, c_range);
    }
  }
}

// zero space in the trace allocated to an entry
void trace_clear(tcell_t *e) {
  size_t count = e->entry.len;
  memset(e, 0, (count + 1) * sizeof(tcell_t));
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

// check if a list is fully reduced i.e. no residual
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
int trace_build_quote(tcell_t *entry, cell_t *l, bool from_return) {
  LOG("trace_build_quote %E %C", entry, l);
  WATCH(l, "build quote");
  assert_error(is_list(l));
  if(is_empty_list(l)) return trace_store_value(entry, l);
  if(is_id_list(l)) {
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

  if(closure_is_ready(*leftmost(&l))) {
    return inline_quote(entry, l, from_return);
  } else {
    return compile_quote(entry, l);
  }
}

bool is_compose_arg(const cell_t *c) {
  return is_var(c) && FLAG(*c, value, ROW);
}

int inline_quote(tcell_t *entry, cell_t *l, bool from_return) {
  LOG("inline quote %C", l);

  FLAG_SET(*entry, entry, FORCED_INLINE);
  cell_t **p;
  FORLIST(p, l, true) { // ***
    reduce(p, is_row_arg(&__it) ?
           &CTX(list, 0, 0) :
           &CTX(any));
  }
  unwrap_id_lists(&l);
  if(is_var(l)) {
    return var_index(entry, l->value.var);
  }
  FORLIST(p, l, true) {
    reduce(p, is_row_arg(&__it) ?
           &CTX(list, 0, 0) :
           &CTX(any));
  }
  if(is_var(l)) {
    switch_entry(entry, l);
    return var_index(entry, l->value.var);
  }

  // need to split alts from reductions inside lists

  // convert spans between `compose_args` (right arguments of compose)
  // into a chain of composes
  list_iterator_t right = list_begin(l);
  cell_t *res = NULL, **left = &res, **q;
  int n = 0;
  bool row = false;
  FORLIST(p, l, true) {
    n++;
    row = false;
    if((is_row_arg(&__it) || is_compose_arg(*p)) && n > 1) {
      int x = trace_alloc(entry, n);
      tcell_t *tc = &entry[x];
      tc->trace.type = T_LIST;
      tc->op = OP_null;
      tc->n = 0;
      RANGEDOWN(i, 1, n) {
        q = list_next(&right, true);
        if(!tc->op) {
          tc->op = is_compose_arg(*q) ? OP_compose : OP_pushr;
        }
        assert_error(q && q != p);
        force(q);
        int y = trace_value(entry, *q);
        tc->expr.arg[i] = index_tr(y);
        entry[y].n++;
      }
      assert_error(tc->op);
      n = 1;
      *left = index_tr(x);
      left = &tc->expr.arg[0];
      row = true;
    }
  }
  if(row) { // final left var, TODO handle nil?
    if(n == 1) {
      if(from_return || // ***
         (is_var(l->value.ptr[0]) &&
          FLAG(*l->value.ptr[0], value, ROW))) FLAG_SET(*entry, entry, ROW); // this entry returns a row
      q = list_next(&right, true);
      force(q);
      int x = trace_value(entry, *q);
      *left = index_tr(x);
      entry[x].n++;
    } else { // ap
      int x = trace_alloc(entry, n);
      tcell_t *tc = &entry[x];
      tc->op = OP_ap;
      tc->trace.type = T_LIST;
      tc->n = 0;
      COUNTDOWN(i, n) {
        q = list_next(&right, false);
        assert_error(q);
        int y = trace_value(entry, *q);
        tc->expr.arg[i] = index_tr(y);
        entry[y].n++;
      }
      *left = index_tr(x);
    }
  } else { // otherwise, this is just a flat list
    int x = trace_alloc(entry, n);
    tcell_t *tc = &entry[x];
    tc->op = OP_quote;
    tc->trace.type = T_LIST;
    tc->n = 0;
    COUNTDOWN(i, n) {
      q = list_next(&right, false);
      if(is_compose_arg(*q)) tc->op = OP_ap;
      assert_error(q);
      int y = trace_value(entry, *q);
      tc->expr.arg[i] = index_tr(y);
      entry[y].n++;
    }
    *left = index_tr(x);
  }

  int x = tr_index(res);
  entry[x].n--;
  apply_condition(l, &x);
  return x;
}

// store a return list
static
int trace_return(tcell_t *entry, cell_t *c_) {
  bool row = entry->entry.specialize && FLAG(entry->entry, specialize, ROW);
  if(row) FLAG_CLEAR(*c_, value, ROW); // ***
  cell_t *c = flat_copy(c_);
  assert_eq(list_size(c), entry->entry.out);
  cell_t **p;
  FORLIST(p, c, true) {
    trace_index_t x;
    assert_error(is_value(*p));
    if(is_var(*p)) {
      switch_entry(entry, *p);
      x = var_index(entry, (*p)->value.var);
    } else if(is_list(*p)) {
      x = trace_build_quote(entry, *p, true);
    } else {
      x = trace_store_value(entry, *p);
    }
    *p = index_tr(x);
    if(x >= 0) entry[x].n++;
  }
  int x = trace_copy_cell(entry, c);
  tcell_t *tc = &entry[x];
  LOG("trace_return: %s[%d] <- %C", entry->word_name, tc-entry, c_);
  closure_free(c);
  trace_set_type(tc, T_RETURN);
  tc->n = -1;
  tc->alt = NULL;
  return x;
}

// reduce for tracing & compilation
unsigned int trace_reduce(tcell_t *entry, cell_t **cp) {
  cell_t *tc = NULL, **prev = &tc;
  unsigned int alts = 0;

  CONTEXT("trace_reduce %s %C", entry->word_name, *cp);
  insert_root(cp);

  COUNTUP(priority, PRIORITY_MAX) {
    cell_t **p = cp;
    bool delay = false;
    CONTEXT("priority = %d", priority);
    context_t *ctx = &CTX_DEFAULT;
    ctx->priority = priority;

  loop_start:
    while(*p) {
      CONTEXT("branch %d: %C", alts, *p);
      ctx->src = p;
      response rsp = func_list(p, &CTX(return));
      if(rsp == DELAY) {
        delay = true;
        p = &(*p)->alt;
        continue;
      }
      // TODO handle rotating alts
      if(rsp != SUCCESS) continue;

      cell_t **a;
      bool has_lists = false;
      FORLIST(a, *p, true) { // ***
        collapse_row(a);
        response rsp = WITH(x, &CTX(any),
                            x->priority = PRIORITY_TOP,
                            reduce(a, x));
        if(rsp != SUCCESS) goto loop_start; // ***
        if(is_value(*a) &&
           !is_list(*a) &&
           !is_var(*a)) { // TODO deps?
          LOG(TODO " use return value of trace_store_value");
          trace_store_value(entry, *a); // TODO use return value
        }
        if(is_list(*a)) {
          has_lists = true;
          LOG("has list %C", *a);
        }
      }
      if(has_lists) {
        LOG("reducing outer lists in %C", *p);
        if(WITH(x, &CTX(return),
                x->priority = PRIORITY_TOP,
                x->flags |= CONTEXT_REDUCE_LISTS,
                func_list(p, x)) != SUCCESS) goto loop_start;
      }
      int x = trace_return(entry, *p);
      tcell_t *r = &entry[x];
      FLAG_CLEAR(*entry, entry, PARTIAL);

      if(!alts) {
        entry->trace.first_return = x;
      }

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

// reduce an expression with one output
unsigned int trace_reduce_one(tcell_t *entry, cell_t *c) {
  cell_t *l = quote(c);
  int alts = trace_reduce(entry, &l);
  drop(l);
  return alts;
}

bool valid_pos(uint8_t pos) {
  return INRANGE(pos, 1, prev_entry_pos);
}

tcell_t *pos_entry(uint8_t pos) {
  return valid_pos(pos) ?
    active_entries[pos - 1] : NULL;
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
  return trace_ptr ? trace_ptr - trace_cells : 0;
}

// delay a branch so that it is listed at the end
// this allows reducing base cases first
void delay_branch(context_t *ctx, priority_t priority) {
  FOLLOW(p, ctx, up) {
    if(p->src) {
      cell_t *c = *p->src;
      if(is_list(c)) {
        c->priority = priority;
      }
    }
  }
}

// link placeholder extensions
void trace_extend(tcell_t *entry, tcell_t *tc) {
  assert_error(entry && tc->op == OP_placeholder);
  if(FLAG(*entry, entry, PARTIAL) ||
     closure_in(tc) != 1) return;
  tcell_t *l = &entry[tr_index(tc->expr.arg[0])];
  l->trace.extension = tc - entry;
}

// return the var corresponding to a placeholder if already traced
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

val_t trace_opaque_symbol(const tcell_t *c) {
  return c->trace.range.min;
}

// get the address of the tcell_t in which e is embedded as a cell_t
tcell_t *tcell_entry(cell_t *e) {
  if(!e) return NULL;
  assert_error(e->op == OP_null && is_persistent(e));
  return (tcell_t *)((char *)e - offsetof(tcell_t, c));
}

// tcells required to store c
csize_t closure_tcells(tcell_t const *c) {
  return calculate_tcells(closure_args(c));
}

// get the next expression
tcell_t *closure_next(tcell_t *c) {
  return c + closure_tcells(c);
}

const tcell_t *closure_next_const(const tcell_t *c) {
  return c + closure_tcells(c);
}

// which output is tc?
// first is 1, where 0 is failure
csize_t dep_arg_index(const tcell_t *entry,
                      const tcell_t *tc) {
  const tcell_t *src = &entry[tr_index(tc->expr.arg[0])];
  int index = tc - entry;
  int i = 1;
  TRAVERSE(src, const, out) {
    if(tr_index(*p) == index) {
      return i;
    } else {
      i++;
    }
  }
  return 0;
}

// check if two expressions are equivalent
bool trace_cell_eq(const tcell_t *entry, const tcell_t *a, const tcell_t *b) {
  if(a->op != b->op) return false;
  op op = a->op;
  if(op == OP_value) {
    if(is_var(a)) {
      return is_var(b) && a->pos == b->pos;
    } else {
      switch(op) {
      case T_INT: return a->value.integer == b->value.integer;
      case T_SYMBOL: return a->value.symbol == b->value.symbol;
      case T_FLOAT: return a->value.flt == b->value.flt;
      default: // ***
        return a == b;
      }
    }
  } else if(op == OP_dep) {
    return dep_arg_index(entry, a) == dep_arg_index(entry, b) &&
      trace_cell_eq(entry,
                    get_arg_const(entry, a->expr.arg[0]),
                    get_arg_const(entry, b->expr.arg[0]));
  } else {
    if(closure_out(a) != closure_out(b)) return false;
    if(closure_in(a) != closure_in(b)) return false;
    COUNTUP(i, closure_in(a)) {
      if(!trace_cell_eq(entry,
                        get_arg_const(entry, a->expr.arg[i]),
                        get_arg_const(entry, b->expr.arg[i]))) {
        return false;
      }
    }
    return true;
  }
}

bool is_array(const tcell_t *tc) {
  return trace_type(tc) == T_OPAQUE &&
    tc->trace.range.min == SYM_Array;
}

// get the return type
type_t trace_type(const tcell_t *tc) {
  assert_error(tc);
  return tc->trace.type;
}
