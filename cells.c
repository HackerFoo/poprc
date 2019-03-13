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

#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "special.h"
#include "rt.h"
#include "trace.h"
#include "list.h"
#include "user_func.h"
#include "macros.h"

// to catch errors that result in large allocations
#define MAX_ALLOC_SIZE 1024

#ifdef __clang__
#pragma clang diagnostic ignored "-Wgnu-empty-initializer"
#endif

// Cell storage array
// NOTE: make sure &cells > 255
#if INTERFACE
#ifdef EMSCRIPTEN
#define CELLS_SIZE (1<<16)
#else
#define CELLS_SIZE (1<<16)
#endif
#endif

#define MAX_ALLOC (CELLS_SIZE - 32)
cell_t cells[CELLS_SIZE] __attribute__((aligned(64))) = {};
cell_t *cells_ptr;
static cell_t *uninitialized_cells;
static cell_t *uninitialized_cells_end;

// Predefined failure cell
CONSTANT cell_t fail_cell = {
  .op = OP_value,
  .size = 1,
  .n = PERSISTENT,
  .value = {
    .type = T_FAIL
  }
};

CONSTANT cell_t nil_cell = {
  .op = OP_value,
  .size = VALUE_OFFSET(ptr),
  .n = PERSISTENT,
  .value.type = T_LIST
};

// Structs for storing statistics
int current_alloc_cnt = 0;
stats_t stats, saved_stats;

// Is `p` a pointer?
bool is_data(void const *p) {
  return (uintptr_t)p > 255;
}

// Is `p` a pointer to a cell in the cell storage array?
bool is_cell(void const *p) {
  return p >= (void *)&cells && p < (void *)(&cells+1);
}

// Is `p` a pointer to a closure (i.e. allocated cell)?
bool is_closure(void const *p) {
  return is_data(p) && ((cell_t *)p)->op;
}

// Is the closure `c` ready to reduce?
bool closure_is_ready(cell_t const *c) {
  assert_error(is_closure(c));
  return is_value(c) || NOT_FLAG(*c, expr, NEEDS_ARG);
}

// Set the readiness of closure `c` to state `r`
void closure_set_ready(cell_t *c, bool r) {
  assert_error(is_closure(c));
  FLAG_SET_TO(*c, expr, NEEDS_ARG, !r);
}

cell_t *cells_next() {
  cell_t *p = cells_ptr;
  assert_error(is_cell(p) && !is_closure(p) && is_cell(cells_ptr->mem.next));
  cells_ptr = cells_ptr->mem.next;
  return p;
}

void cells_init() {
  // zero the cells
  memset(&cells, 0, sizeof(cell_t) * 2);

  // set up doubly-linked pointer ring
  cells[0].mem.prev = &cells[1];
  cells[0].mem.next = &cells[1];
  cells[1].mem.prev = &cells[0];
  cells[1].mem.next = &cells[0];

  cells_ptr = &cells[0];
  uninitialized_cells = &cells[2];
  uninitialized_cells_end = &cells[LENGTH(cells)];
  current_alloc_cnt = 0;
}

static
void cell_alloc(cell_t *c) {
  assert_error(is_cell(c) && !is_closure(c));
  cell_t *prev = c->mem.prev;
  assert_error(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->mem.next;
  assert_error(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  assert_throw(c != prev && c != next, "can't alloc the last cell");
  prev->mem.next = next;
  next->mem.prev = prev;
}

cell_t *alloc_value() {
  return closure_alloc(1 + VALUE_OFFSET(integer));
}

cell_t *alloc_list(csize_t n) {
  return closure_alloc(n + VALUE_OFFSET(ptr));
}

// allocate space for n chars + '\0'
cell_t *alloc_string(size_t n) {
  int args = DIV_UP(n + 1, sizeof_field(cell_t, expr.arg[0]));
  cell_t *c = closure_alloc(args + VALUE_OFFSET(str));
  c->op = OP_value;
  c->value.type = T_STRING;
  c->value.str[n] = '\0';
  size_t max_size = sizeof_field(cell_t, expr.arg[0]) * args;
  c->value.str[max_size-1] = max_size - n;
  return c;
}

size_t set_string_size(cell_t *c, size_t n) {
  int args = c->size - VALUE_OFFSET(str);
  if(args <= 0) return 0;
  size_t max_size = sizeof_field(cell_t, expr.arg[0]) * args;
  if(n > max_size - 1) n = max_size - 1;
  c->value.str[n] = '\0';
  c->value.str[max_size-1] = max_size - n;
  return n;
}

size_t string_size(const cell_t *c) {
  int args = c->size - VALUE_OFFSET(str);
  if(args <= 0) return 0;
  size_t max_size = sizeof_field(cell_t, expr.arg[0]) * args;
  return max_size - c->value.str[max_size-1];
}

size_t max_strlen(const cell_t *c) {
  if(c->size < VALUE_OFFSET(str)) return 0;
  return (c->size - VALUE_OFFSET(str)) * sizeof_field(cell_t, expr.arg[0]) - 1;
}

cell_t *closure_alloc(csize_t args) {
  cell_t *c = closure_alloc_cells(calculate_cells(args));
  c->size = args;
  return c;
}

cell_t *closure_alloc_cells(csize_t size) {
  assert_throw(size <= MAX_ALLOC_SIZE);
  assert_throw(current_alloc_cnt + size <= MAX_ALLOC, "%d bytes allocated", current_alloc_cnt);
  cell_t *c;

  if(uninitialized_cells &&
     uninitialized_cells + size <= uninitialized_cells_end) {
    // allocate from uninitialized cells first
    c = uninitialized_cells;
    uninitialized_cells += size;
  } else {
    if(size == 1) uninitialized_cells = NULL;

    // otherwise allocate from the chain
    cell_t *ptr = cells_next();
    cell_t *mark = ptr;
    csize_t cnt = 0;
    (void)mark;

    c = ptr;

    // search for contiguous chunk
    while(cnt < size) {
      if(is_cell(ptr) && !is_closure(ptr)) {
        cnt++;
        ptr++;
      } else {
        cnt = 0;
        c = ptr = cells_next();
        assert_throw(c != mark, "could not find cells to allocate");
      }
    }

    // remove the found chunk
    COUNTUP(i, size) {
      cell_alloc(&c[i]);
    }
  }

  // update stats
  stats.alloc_cnt += size;
  current_alloc_cnt += size;
  if(current_alloc_cnt > stats.max_alloc_cnt)
    stats.max_alloc_cnt = current_alloc_cnt;

  memset(c, 0, sizeof(cell_t)*size);
  WATCH(c, "cell_alloc");
  return c;
}

#define calc_size(f, n)     \
  ((offsetof(cell_t, f[n])  \
    + (sizeof(cell_t) - 1)) \
   / sizeof(cell_t))

csize_t calculate_cells(csize_t n) {
  return n <= 3 ? 1 : calc_size(expr.arg, n); // TODO calculate 3 at compile time
}

csize_t calculate_list_size(csize_t n) {
  return calc_size(value.ptr, n);
}

csize_t calculate_map_size(csize_t n) {
  return calc_size(value.map, n + 1);
}

csize_t closure_cells(cell_t const *c) {
  return calculate_cells(closure_args(c));
}

void cell_free(cell_t *c) {
  c->op = 0;
  c->mem.next = cells_ptr;
  c->mem.prev = cells_ptr->mem.prev;
  cells_ptr->mem.prev = c;
  c->mem.prev->mem.next = c;
}

void closure_shrink(cell_t *c, csize_t s) {
  if(!c) return;
  assert_error(is_cell(c));
  if(s == 0) WATCH(c, "closure_shrink");
  csize_t size = closure_cells(c);
  if(size > s) {
    assert_error(is_closure(c));
    cell_t *prev = cells_ptr->mem.prev;
    RANGEUP(i, s, size) {
      c[i].op = 0;
      c[i].mem.prev = prev;
      prev->mem.next = &c[i];
      prev = &c[i];
    }
    cells_ptr->mem.prev = prev;
    prev->mem.next = cells_ptr;
    current_alloc_cnt -= size - s;
  }
}

void closure_free(cell_t *c) {
  closure_shrink(c, 0);
}

// max offset is 255
bool is_offset(cell_t const *c) {
  return !((uintptr_t)c & ~0xff);
}

csize_t list_size(cell_t const *c) {
  return c->size > VALUE_OFFSET(ptr) ? c->size - VALUE_OFFSET(ptr) : 0;
}

csize_t closure_args(cell_t const *c) {
  assert_error(is_closure(c));
  return c->size;
}

csize_t closure_in(cell_t const *c) {
  assert_error(is_closure(c) && !is_value(c));
  csize_t in = c->size - c->expr.out;
  if(is_user_func(c)) in--;
  return in;
}

csize_t closure_out(cell_t const *c) {
  assert_error(is_closure(c) && !is_value(c));
  return c->expr.out;
}

csize_t closure_next_child(cell_t const *c) {
  assert_error(is_closure(c));
  return !closure_is_ready(c) && is_offset(c->expr.arg[0]) ? (intptr_t)c->expr.arg[0] : 0;
}

cell_t **closure_next_arg(cell_t *c) {
  return &c->expr.arg[closure_next_child(c)];
}

cell_t *const *closure_next_arg_const(const cell_t *c) {
  return &c->expr.arg[closure_next_child(c)];
}

cell_t *copy(cell_t const *c) {
  CONTEXT("copying %C", c);
  csize_t size = closure_cells(c);
  cell_t *new_c = closure_alloc_cells(size);
  memcpy(new_c, c, size * sizeof(cell_t));
  new_c->n = 0;
  return new_c;
}

cell_t *copy_ref(cell_t const *c) {
  cell_t *nc = copy(c);
  TRAVERSE_REF(nc, alt, in, ptrs);
  return nc;
}

cell_t *copy_expand(cell_t const *c, csize_t s) {
  csize_t n = closure_args(c);
  csize_t new_size = calculate_cells(n + s);
  cell_t *new_c = closure_alloc_cells(new_size);
  memcpy(new_c, c, offsetof(cell_t, expr.arg[n]));
  new_c->size = n + s;
  new_c->n = 0;
  return new_c;
}

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, refcount_t n) {
  if(c && c->n != PERSISTENT) {
    assert_error(is_closure(c));
    c->n += n;
  }
  return c;
}

bool has_type(cell_t const *c, int t) {
  return c &&
    c->op == OP_value &&
    c->value.type == t;
}

bool is_fail(cell_t const *c) {
  return has_type(c, T_FAIL);
}

bool is_any(cell_t const *c) {
  return is_value(c) && c->value.type == T_ANY;
}

void dropn(cell_t *c, refcount_t n) {
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert_error(is_closure(c));
  if(n > c->n) {
    cell_t *p;
    LOG_WHEN(c->alt &&
             !c->alt->n &&
             c->alt->op != OP_value,
             MARK("WARN") " unreduced alt %C -> %C", c, c->alt);
    TRAVERSE(c, alt, in, ptrs) {
      drop(*p);
    }
    if(is_dep(c) && !is_value(p = c->expr.arg[0]) && is_closure(p)) {
      /* mark dep arg as gone */
      // TODO improve this using stored pos
      csize_t n = closure_args(p);
      while(n--) {
        if(p->expr.arg[n] == c) {
          p->expr.arg[n] = 0;
          break;
        }
      }
    }
    if(is_var(c) && !is_list(c)) {
      trace_drop(c);
    }
    closure_free(c);
  } else {
    c->n -= n;
  }
}

void drop(cell_t *c) {
  dropn(c, 1);
}

void drop_multi(cell_t **a, csize_t n) {
  COUNTUP(i, n) {
    drop(*a++);
  }
}

void fake_drop(cell_t *c) {
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert_error(~c->n && is_closure(c));
  if(!c->n) {
    TRAVERSE(c, alt, in, ptrs) {
      fake_drop(*p);
    }
  }
  --c->n;
}

void fake_undrop(cell_t *c) {
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert_error(is_closure(c));
  if(!++c->n) {
    TRAVERSE(c, alt, in, ptrs) {
      fake_undrop(*p);
    }
  }
}

alt_set_t as_single(unsigned int k, unsigned int v) {
  assert_throw(k < AS_SIZE);
  return (alt_set_t)1 << ((k << 1) + (v & 1));
}

alt_set_t as_multi(unsigned int k, unsigned n, unsigned int v) {
  if(n == 0) return 0;
  alt_set_t alt_set = 0;
  while(n--) {
    alt_set |= as_single(k, v);
    k++;
    v >>= 1;
  }
  return alt_set;
}

alt_set_t as_intersect(alt_set_t a, alt_set_t b) {
  return a & b;
}

alt_set_t as_union(alt_set_t a, alt_set_t b) {
  return a | b;
}

alt_set_t as_conflict(alt_set_t a) {
  return (a & (a >> 1)) & AS_MASK;
}

alt_set_t as_mask(alt_set_t a) {
  return (a | (a >> 1)) & AS_MASK;
}

/*
alt_set_t as_more_general_than(alt_set_t a, alt_set_t b) {
  return (~a & b) & ~(((alt_set_t)1<<AS_SIZE)-1);
}
*/

TEST(alt_sets) {
  bool ok = true;
  alt_set_t
    a0 = as_single(0, 0),
    a1 = as_single(0, 1),
    b0 = as_single(1, 0),
    b1 = as_single(1, 1),
    c = a0 | b0,
    m0 = as_mask(a0 | b1),
    m1 = as_mask(a1 | b0),
    d = as_single(2, 1),
    e = as_multi(0, 3, 5);
  ok &= !as_conflict(a0 | b1);
  ok &= !!as_conflict(a1 | c);
  ok &= m0 == m1;
  ok &= (a1 | b0 | d) == e;
//  ok &= !!as_more_general_than(a0, c);
  return ok ? 0 : -1;
}

void set_bit(uint8_t *m, unsigned int x) {
  m[x >> 3] |= 1 << (x & 7);
}

void clear_bit(uint8_t *m, unsigned int x) {
  m[x >> 3] &= ~(1 << (x & 7));
}

bool check_bit(uint8_t *m, unsigned int x) {
  return m[x >> 3] & (1 << (x & 7));
}

cell_t *closure_next(cell_t *c) {
  return c + closure_cells(c);
}

const cell_t *closure_next_const(const cell_t *c) {
  return c + closure_cells(c);
}

// used to get consistent allocations
void alloc_to(size_t n) {
  if(n < LENGTH(cells) &&
     uninitialized_cells < &cells[n]) {
    size_t s = &cells[n] - uninitialized_cells;
    cell_t *c = closure_alloc_cells(s);
    c->op = OP_value;
    closure_free(c);
  }
}

bool check_cycle() {
  size_t i = 0;
  cell_t *start = cells_ptr, *ptr = start;
  while(ptr->mem.next != start) {
    if(i > LENGTH(cells)) return false;
    i++;
    assert_error(is_cell(ptr->mem.next->mem.next));
    ptr = ptr->mem.next;
  }
  return true;
}

TEST(alloc) {
  cell_t *a[30];
  LOOP(50) {
    FOREACH(i, a) {
      a[i] = func(OP_ap, 9, 1);
    }
    FOREACH(i, a) {
      closure_free(a[i]);
    }
  }
  return leak_test() && check_cycle() ? 0 : -1;
}

bool leak_test() {
  bool leak = false;
  FOREACH(i, cells) {
    cell_t *c = &cells[i];
    if(is_closure(c)) {
      if(c->n != PERSISTENT) {
        printf("LEAK: %" PRIuPTR " (%u)\n", i, (unsigned int)cells[i].n);
        leak = true;
      }
      i += closure_cells(c) - 1;
    }
  }
  return !leak;
}

static
cell_t **flatten(cell_t *c, cell_t **tail) {
  if(c && !c->tmp && tail != &c->tmp && c->n != PERSISTENT) {
    LIST_ADD(tmp, tail, c);
    TRAVERSE(c, alt, in, ptrs) {
      tail = flatten(*p, tail);
    }
  }
  return tail;
}

void print_list(cell_t *c) {
  if(c) {
    printf("{%d", (int)(c-cells));
    while((c = c->tmp)) {
      printf(", %d", (int)(c-cells));
    }
    printf("}\n");
  } else {
    printf("{}\n");
  }
}

static
void assert_ref_dec(cell_t *c) {
  while(c) {
    TRAVERSE(c, alt, in, ptrs) {
      cell_t *x = *p;
      if(x && x->n != PERSISTENT) --x->n;
    }
    c = c->tmp;
  }
}

static
void assert_ref_inc(cell_t *c) {
  while(c) {
    TRAVERSE(c, alt, in, ptrs) {
      cell_t *x = *p;
      if(x && x->n != PERSISTENT) ++x->n;
    }
    c = c->tmp;
  }
}

size_t count_root(const cell_t *c, cell_t ***roots, size_t n) {
  size_t cnt = 0;
  COUNTUP(i, n) {
    if(roots[i] && c == *roots[i]) cnt++;
  }
  return cnt;
}

static
void print_roots(cell_t ***roots, size_t n) {
  COUNTUP(i, n) {
    LOG_WHEN(roots[i] && *roots[i], "root: %d @ 0x%p", (int)((*roots[i])-cells), (void *)roots[i]);
  }
}

static
bool assert_ref_check(cell_t *c, cell_t ***roots, size_t roots_n) {
  bool res = true;
  while(c) {
    refcount_t n = c->n + 1;
    if(count_root(c, roots, roots_n)) n = 0;
    if(n) {
      LOG("assert_ref: cell[%C].n == %d", c, n);
      res = false;
    }
    c = c->tmp;
  }
  return res;
}

// check ref counts starting at root
bool assert_ref(cell_t ***roots, size_t n) {
  cell_t *list = 0, **tail = &list;
  COUNTUP(i, n) {
    if(!roots[i] || !is_closure(*roots[i])) continue; // ***
    tail = flatten(*roots[i], tail);
  }
  assert_ref_dec(list);
  bool check = assert_ref_check(list, roots, n);
  assert_ref_inc(list);
  clean_tmp(list);
  if(!check) {
    print_roots(roots, n);
  }
  return check;
}

cell_t *take(cell_t **cp) {
  cell_t *r = *cp;
  *cp = NULL;
  return r;
}

void unique(cell_t **cp) {
  cell_t *c = *cp;
  if(c->n) {
    if(c->n != PERSISTENT) --c->n;
    cell_t *n = copy(c);
    TRAVERSE_REF(n, alt, args, ptrs);
    *cp = n;
  }
}

bool out_used(const cell_t *c, int ix) {
  int n = closure_args(c);
  int out = closure_out(c);
  assert_error(ix <= out);
  cell_t *const *out_arg = &c->expr.arg[n - out];
  if(ix) {
    return out_arg[ix - 1] != NULL;
  } else {
    int ref = c->n;
    COUNTUP(i, out) {
      if(out_arg[i]) ref--;
    }
    return ref >= 0;
  }
}

int count_out_used(const cell_t *c) {
  int n = closure_args(c);
  int out = closure_out(c);
  cell_t *const *out_arg = &c->expr.arg[n - out];
  int ref = c->n;
  COUNTUP(i, out) {
    if(out_arg[i]) ref--;
  }
  return (ref >= 0) + c->n - ref;
}

void cleanup_cells() {
  FOREACH(i, cells) {
    cell_t *c = &cells[i];
    if(is_closure(c)) {
      int s = closure_cells(c) - 1;
      if(c->n != PERSISTENT) {
        closure_free(c);
      }
      i += s;
    }
  }
}
