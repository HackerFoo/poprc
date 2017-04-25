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

#include <string.h>
#include <assert.h>
#include "rt_types.h"
#include "gen/cells.h"
#include "gen/special.h"
#include "gen/rt.h"
#include "gen/support.h"
#include "gen/byte_compile.h"
#include "gen/list.h"

// to catch errors that result in large allocations
#define MAX_ALLOC_SIZE 32

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

// Predefined failure cell
cell_t fail_cell = {
  .func = func_value,
  .size = 1,
  .n = PERSISTENT,
  .value.type.flags = T_FAIL
};

cell_t nil_cell = {
  .func = func_value,
  .size = 1,
  .n = PERSISTENT,
  .value.type.exclusive = T_LIST
};

// Structs for storing statistics
measure_t measure, saved_measure;

// Is `p` a pointer?
bool is_data(void const *p) {
  return (uintptr_t)p > 255;
}

// Is `p` a pointer to a cell in the cell storage array?
bool is_cell(void const *p) {
  return p >= (void *)&cells && p < (void *)(&cells+1);
}

// Is `p` a prointer to a closure (i.e. allocated cell)?
bool is_closure(void const *p) {
  return is_data(p) && ((cell_t *)p)->func;
}

// Is the closure `c` ready to reduce?
bool closure_is_ready(cell_t const *c) {
  assert(is_closure(c));
  return is_value(c) || !(c->expr.flags & FLAGS_NEEDS_ARG);
}

// Set the readiness of closure `c` to state `r`
void closure_set_ready(cell_t *c, bool r) {
  assert(is_closure(c));
  FLAG_SET_TO(c->expr.flags, FLAGS_NEEDS_ARG, !r);
}

cell_t *cells_next() {
  cell_t *p = cells_ptr;
  assert(is_cell(p) && !is_closure(p) && is_cell(cells_ptr->mem.next));
  cells_ptr = cells_ptr->mem.next;
  return p;
}

#ifdef CHECK_CYCLE
bool check_cycle() {
  size_t i = 0;
  cell_t *start = cells_ptr, *ptr = start;
  while(ptr->next != start) {
    if(i > LENGTH(cells)) return false;
    i++;
    assert(is_cell(ptr->next->next));
    ptr = ptr->next;
  }
  return true;
}
#else
bool check_cycle() {
  return true;
}
#endif

void cells_init() {
  size_t const n = LENGTH(cells);

  // zero the cells
  memset(&cells, 0, sizeof(cells));

  // set up doubly-linked pointer ring
  cells[0].mem.prev = &cells[n-1];
  cells[0].mem.next = &cells[1];
  for(size_t i = 1; i < n-1; i++) {
    cells[i].mem.prev = &cells[i-1];
    cells[i].mem.next = &cells[i+1];
  }
  cells[n-1].mem.prev = &cells[n-2];
  cells[n-1].mem.next = &cells[0];

  cells_ptr = &cells[0];
}

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  assert_throw(measure.current_alloc_cnt < MAX_ALLOC);
  cell_t *prev = c->mem.prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->mem.next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  assert_throw(c != prev && c != next, "can't alloc the last cell");
  prev->mem.next = next;
  next->mem.prev = prev;
  measure.alloc_cnt++;
  if(++measure.current_alloc_cnt > measure.max_alloc_cnt)
    measure.max_alloc_cnt = measure.current_alloc_cnt;
}

cell_t *closure_alloc(csize_t args) {
  cell_t *c = closure_alloc_cells(calculate_cells(args));
  c->size = args;
  return c;
}

cell_t *closure_alloc_cells(csize_t size) {
  assert_throw(size < MAX_ALLOC_SIZE);
  cell_t *ptr = cells_next(), *c = ptr;
  cell_t *mark = ptr;
  csize_t cnt = 0;
  (void)mark;

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
  for(csize_t i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  memset(c, 0, sizeof(cell_t)*size);
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

csize_t calculate_val_size(csize_t n) {
  return calc_size(value.integer, n);
}

csize_t calculate_map_size(csize_t n) {
  return calc_size(value.map, n + 1);
}

csize_t closure_cells(cell_t const *c) {
  return calculate_cells(closure_args(c));
}

void cell_free(cell_t *c) {
  c->func = 0;
  c->mem.next = cells_ptr;
  c->mem.prev = cells_ptr->mem.prev;
  cells_ptr->mem.prev = c;
  c->mem.prev->mem.next = c;
}

void closure_shrink(cell_t *c, csize_t s) {
  if(!c) return;
  assert(is_cell(c));
  csize_t i, size = closure_cells(c);
  if(size > s) {
    assert(is_closure(c));
    for(i = s; i < size; i++) {
      c[i].func = 0;
      c[i].mem.prev = &c[i-1];
      c[i].mem.next = &c[i+1];
    }
    c[s].mem.prev = cells_ptr->mem.prev;
    cells_ptr->mem.prev->mem.next = &c[s];
    c[size-1].mem.next = cells_ptr;
    cells_ptr->mem.prev = &c[size-1];
    measure.current_alloc_cnt -= size - s;
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
  return c->size ? c->size - 1 : 0;
}

csize_t val_size(cell_t const *c) {
  return c->size ? c->size - 1 : 0;
}

csize_t closure_args(cell_t const *c) {
  assert(is_closure(c));
  return c->size;
}

csize_t closure_in(cell_t const *c) {
  assert(is_closure(c) && !is_value(c));
  csize_t in = c->size - c->expr.out;
  if(c->expr.flags & FLAGS_USER_FUNC) in--;
  return in;
}

csize_t closure_out(cell_t const *c) {
  assert(is_closure(c) && !is_value(c));
  return c->expr.out;
}

csize_t closure_next_child(cell_t const *c) {
  assert(is_closure(c));
  return is_offset(c->expr.arg[0]) ? (intptr_t)c->expr.arg[0] : 0;
}

cell_t *copy(cell_t const *c) {
  csize_t size = closure_cells(c);
  cell_t *new_c = closure_alloc_cells(size);
  memcpy(new_c, c, size * sizeof(cell_t));
  new_c->n = 0;
  return new_c;
}

cell_t *copy_expand(cell_t const *c, csize_t s) {
  csize_t n = closure_args(c);
  csize_t new_size = calculate_cells(n + s);
  cell_t *new_c = closure_alloc_cells(new_size);
  memcpy(new_c, c, (uintptr_t)&((cell_t *)0)->expr.arg[n]);
  new_c->size = n + s;
  new_c->n = 0;
  return new_c;
}

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, refcount_t n) {
  c = clear_ptr(c);
  if(c && c->n != PERSISTENT) {
    assert(is_closure(c));
    c->n += n;
  }
  return c;
}

bool is_fail(cell_t const *c) {
  return is_value(c) && (c->value.type.flags & T_FAIL) != 0;
}

bool is_any(cell_t const *c) {
  return is_value(c) && c->value.type.exclusive == T_ANY;
}

void drop(cell_t *c) {
  c = clear_ptr(c);
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert(is_closure(c));
  if(!c->n) {
    cell_t *p;
    TRAVERSE(c, alt, in, ptrs) {
      drop(*p);
    }
    if(is_dep(c) && !is_value(p = c->expr.arg[0]) && is_closure(p)) {
      /* mark dep arg as gone */
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
    --c->n;
  }
}

void drop_multi(cell_t **a, csize_t n) {
  for(csize_t i = 0; i < n; i++) drop(*a++);
}

void fake_drop(cell_t *c) {
  c = clear_ptr(c);
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert(~c->n && is_closure(c));
  if(!c->n) {
    TRAVERSE(c, alt, in, ptrs) {
      fake_drop(*p);
    }
  }
  --c->n;
}

void fake_undrop(cell_t *c) {
  c = clear_ptr(c);
  if(!is_cell(c) || c->n == PERSISTENT) return;
  assert(is_closure(c));
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

int test_alt_sets() {
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
