/* Copyright 2012-2015 Dustin DeWeese
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
#include "gen/rt.h"
#include "gen/primitive.h"

#ifdef __clang__
#pragma clang diagnostic ignored "-Wgnu-empty-initializer"
#endif

// Cell storage array
// NOTE: make sure &cells > 255
#if INTERFACE
#ifdef EMSCRIPTEN
#define CELLS_SIZE (1<<10)
#else
#define CELLS_SIZE (1<<16)
#endif
#endif
#define MAX_ALLOC (CELLS_SIZE - 32)
cell_t cells[CELLS_SIZE] = {};
cell_t *cells_ptr;

// Counter of used alt ids
uint8_t alt_cnt = 0;

// Predefined failure cell
cell_t fail_cell = {
  .func = func_value,
  .value.type = T_FAIL
};

// to catch errors that result in large allocations
#define MAX_ALLOC_SIZE 32

// OBSOLETE Array for tracking 'live' alt ids
uintptr_t alt_live[AS_SIZE];

// Structs for storing statistics
measure_t measure, saved_measure;

// Default tracing function that does nothing
void trace_noop(UNUSED cell_t *c, UNUSED cell_t *r, UNUSED trace_type_t tt, UNUSED csize_t n) {}

// Pointer to tracing function
void (*trace)(cell_t *, cell_t *, trace_type_t, csize_t) = trace_noop;

// Set the tracing function
void set_trace(void (*t)(cell_t *, cell_t *, trace_type_t, csize_t)) {
  trace = t ? t : trace_noop;
}

// #define CHECK_CYCLE

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
  return !is_marked(c->func);
}

// Set the readiness of closure `c` to state `r`
void closure_set_ready(cell_t *c, bool r) {
  assert(is_closure(c));
  c->func = (reduce_t *)(r ? clear_ptr(c->func) : mark_ptr(c->func));
}

// Duplicate c to c->alt and return it
cell_t *dup_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t i = 0, in = closure_in(c), out = 0;
  assert(n < in);
  cell_t *a = copy(c);

  // ref args
  for(; i < in; ++i) {
    if(i != n) ref(a->expr.arg[i]);
  }

  // update deps
  for(; i < c->size; ++i) {
    if(a->expr.arg[i]) a->expr.arg[i] = dep(a);
    c->expr.arg[i]->alt = conc_alt(a->expr.arg[i], c->expr.arg[i]->alt);
    ++out;
  }

  a->expr.arg[n] = b;
  a->n = out;
  c->alt = a;
  return a;
}

// Lift alternates from c->arg[n] to c
void split_arg(cell_t *c, csize_t n) {
  cell_t
    *a = c->expr.arg[n],
    *p = c,
    **pa;
  if(!a || !a->alt || is_marked(a)) return;
  do {
    pa = &p->expr.arg[n];
    if(*pa == a) {
      // insert a copy with the alt arg
      p = dup_alt(p, n, ref((*pa)->alt))->alt;
      // mark the arg
      *pa = mark_ptr(*pa);
    } else p = p->alt;
  } while(p);
}

// Reduce then split c->arg[n]
bool reduce_arg(cell_t *c,
                csize_t n,
                alt_set_t *as,
                type_t t) {
  bool r = reduce(&c->expr.arg[n], t);
  split_arg(c, n);
  return r && entangle(as, clear_ptr(c->expr.arg[n]));
}

// Lift alternates from all args
cell_t *closure_split(cell_t *c, csize_t s) {
  csize_t i;
  for(i = 0; i < s; ++i) {
    split_arg(c, i);
  }
  for(i = 0; i < s; ++i) {
    c->expr.arg[i] = clear_ptr(c->expr.arg[i]);
  }
  return c->alt;
}

// Clear the flags bits in args
void clear_flags(cell_t *c) {
  int i = 0;
  for(; i < c->size; ++i) {
    c->expr.arg[i] = clear_ptr(c->expr.arg[i]);
  }
}

cell_t *closure_split1(cell_t *c, int n) {
  if(!c->expr.arg[n]->alt) return c->alt;
  return dup_alt(c, n, ref(c->expr.arg[n]->alt));
}

// Reduce *cp with type t
bool reduce(cell_t **cp, type_t t) {
  cell_t *c;
  while((c = clear_ptr(*cp))) {
    if(!closure_is_ready(c)) close_placeholders(c);
    if(is_placeholder(c)) break;
    assert(is_closure(c));
    if(!closure_is_ready(c)) {
      fail(cp);
      continue;
    }
    measure.reduce_cnt++;
    if(c->func(cp, t)) return true;
  }
  *cp = &fail_cell;
  return false;
}

// Perform one reduction step on *cp
void reduce_dep(cell_t **cp) {
  cell_t *c = clear_ptr(*cp);
  if(!closure_is_ready(c)) close_placeholders(c);
  if(!c || !closure_is_ready(c)) {
    fail(cp);
  } else {
    assert(is_closure(c) &&
           closure_is_ready(c));
    measure.reduce_cnt++;
    c->func(cp, T_ANY);
  }
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
  size_t const n = LENGTH(cells)-1;

  // zero the cells
  memset(&cells, 0, sizeof(cells));
  memset(&alt_live, 0, sizeof(alt_live));

  // set up doubly-linked pointer ring
  for(size_t i = 0; i < n; i++) {
    cells[i].mem.prev = &cells[i-1];
    cells[i].mem.next = &cells[i+1];
  }
  cells[0].mem.prev = &cells[n-1];
  cells[n-1].mem.next = &cells[0];

  cells_ptr = &cells[0];
  alt_cnt = 0;
}

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  assert(measure.current_alloc_cnt < MAX_ALLOC);
  cell_t *prev = c->mem.prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->mem.next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
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
  assert(size < MAX_ALLOC_SIZE);
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
      assert(c != mark);
    }
  }

  // remove the found chunk
  for(csize_t i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  memset(c, 0, sizeof(cell_t)*size);
  return c;
}

#define calc_size(f, n)                         \
  ((sizeof(cell_t)                              \
    + ((uintptr_t)&(((cell_t *)0)->f[n]) - 1))  \
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

bool type_match(type_t t, cell_t const *c) {
  type_t ta = t & T_EXCLUSIVE;
  type_t tb = c->value.type & T_EXCLUSIVE;
  return ta == T_ANY || tb == T_ANY || ta == tb;
}

bool func_value(cell_t **cp, type_t t) {
  cell_t *c = clear_ptr(*cp); // TODO remove clear_ptr
  assert(is_closure(c));
  measure.reduce_cnt--;
  if(c->value.type != T_FAIL &&
     type_match(t, c)) {
    if(is_any(c)) {
      /* create placeholder */
      if((t & T_EXCLUSIVE) == T_LIST) {
        c->value.ptr[0] = func(func_placeholder, 0, 1);
        c->size = 2;
        c->value.type &= ~T_TRACED;
      }
      c->value.type |= t;
      trace(c, 0, tt_touched, 0);
    }
    return true;
  }
  else {
    fail(cp);
    return false;
  }
}

cell_t *val(intptr_t x) {
  cell_t *c = closure_alloc(2);
  c->func = func_value;
  c->value.type = T_INT;
  c->value.integer[0] = x;
  return c;
}

cell_t *var(type_t t) {
  cell_t *c = closure_alloc(1);
  c->func = func_value;
  c->value.type = T_VAR | t;
  return c;
}

cell_t *vector(csize_t n) {
  cell_t *c = closure_alloc(n+1);
  c->func = func_value;
  c->value.type = T_ANY;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(2);
  c->func = func_value;
  c->value.type = T_LIST;
  c->value.ptr[0] = x;
  return c;
}

cell_t *empty_list() {
  cell_t *c = closure_alloc(1);
  c->func = func_value;
  c->value.type = T_LIST;
  return c;
}

cell_t *make_list(csize_t n) {
  cell_t *c = closure_alloc(n + 1);
  c->func = func_value;
  c->value.type = T_LIST;
  return c;
}

cell_t *append(cell_t *a, cell_t *b) {
  csize_t n = list_size(b);
  csize_t n_a = list_size(a);
  cell_t *e = expand(a, n);
  while(n--) e->value.ptr[n + n_a] = b->value.ptr[n];
  return e;
}

cell_t *expand(cell_t *c, csize_t s) {
  if(!c) return 0;
  csize_t n = closure_args(c);
  csize_t cn_p = calculate_cells(n);
  csize_t cn = calculate_cells(n + s);
  if(!c->n && cn == cn_p) {
    c->size += s;
    return c;
  } else {
    /* copy */
    cell_t *new = closure_alloc(n + s);
    memcpy(new, c, cn_p * sizeof(cell_t));
    if(is_placeholder(c)) trace(new, c, tt_copy, 0);
    new->n = 0;
    traverse_ref(new, ARGS_IN | PTRS | ALT);
    new->size = n + s;
    if(is_value(c)) alt_set_ref(c->value.alt_set);
    drop(c);
    return new;
  }
}

cell_t *expand_inplace(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->expr.arg[s], &c->expr.arg[0], (c->size - 1) * sizeof(cell_t *));
  csize_t i;
  for(i = c->size - c->expr.out; i < c->size; ++i) {
    if(c->expr.arg[i]) c->expr.arg[i]->expr.arg[0] = c;
  }
  return c;
}

cell_t *expand_inplace_dep(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  csize_t in = closure_in(c);
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->expr.arg[in+s], &c->expr.arg[in], c->expr.out * sizeof(cell_t *));
  c->expr.out += s;
  csize_t i;
  for(i = c->size - c->expr.out + s; i < c->size; ++i) {
    if(c->expr.arg[i]) c->expr.arg[i]->expr.arg[0] = c;
  }
  return c;
}

cell_t *compose_placeholders(cell_t *a, cell_t *b) {
  csize_t i;
  csize_t b_in = closure_in(b);
  csize_t b_out = closure_out(b);
  cell_t *c = expand(b, a->size);
  memmove(c->expr.arg + a->size + b_in,
          c->expr.arg + b_in,
          b_out * sizeof(cell_t *));
  for(i = 0; i < closure_in(a); ++i)
    c->expr.arg[b_in + i] = ref(a->expr.arg[i]);
  for(; i < a->size; ++i)
    c->expr.arg[b_in + i] = a->expr.arg[i];
  //drop(a); // dropping here will cause func_compose to break in store_reduced
  cell_t *ab[] = {a, b};
  trace(c, (cell_t *)ab, tt_compose_placeholders, 0);
  return c;
}

cell_t *compose_nd(cell_t *a, cell_t *b) {
  csize_t n = list_size(b);
  csize_t n_a = list_size(a);
  csize_t i = 0;
  if(n && n_a) {
    cell_t *l;
    while(!closure_is_ready(l = b->value.ptr[n-1]) && i < n_a) {
      cell_t *x = a->value.ptr[i];
      if(is_placeholder(x)) {
        if(is_placeholder(l)) {
          b->value.ptr[n-1] = compose_placeholders(x, l);
          ++i;
          break;
        }
        a->value.ptr[i] = x = expand_inplace_dep(x, 1);
        b = arg_nd(l, x->expr.arg[closure_in(x)] = dep(ref(x)), b);
      } else {
        b = arg_nd(l, ref(x), b);
        ++i;
      }
    }
  }
  cell_t *e = expand(b, n_a - i);
  for(unsigned j = n; i < n_a; ++i, ++j) {
    e->value.ptr[j] = ref(a->value.ptr[i]);
  }
  drop(a);
  return e;
}

bool is_value(cell_t const *c) {
  return c && c->func == func_value;
}

// max offset is 255
bool is_offset(cell_t const *c) {
  return !((uintptr_t)c & ~0xff);
}

cell_t *func(reduce_t *f, csize_t in, csize_t out) {
  assert(out > 0);
  csize_t args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->expr.out = out - 1;
  c->func = f;
  if(args) c->expr.arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, !args && f != func_placeholder);
  return c;
}

#define cell_offset(f) ((cell_t **)&(((cell_t *)0)->f))

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
  return c->size - c->expr.out;
}

csize_t closure_out(cell_t const *c) {
  assert(is_closure(c) && !is_value(c));
  return c->expr.out;
}

csize_t closure_next_child(cell_t const *c) {
  assert(is_closure(c));
  return is_offset(c->expr.arg[0]) ? (intptr_t)c->expr.arg[0] : 0;
}

/* arg is destructive to *cp */
void arg(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  csize_t i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->expr.arg[0]))) {
    c = expand_inplace(c, 1);
    c->expr.arg[0] = a;
  } else if(!is_data(c->expr.arg[i])) {
    c->expr.arg[0] = (cell_t *)(intptr_t)
      (i - (closure_is_ready(a) ? 1 : 0));
    c->expr.arg[i] = a;
    if(i == 0 && !is_placeholder(c))
      closure_set_ready(c, closure_is_ready(a));
  } else {
    arg(&c->expr.arg[i], a);
    if(!is_placeholder(c) &&
       closure_is_ready(c->expr.arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->expr.arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void arg_noexpand(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  csize_t i = closure_next_child(c);
  if(!is_data(c->expr.arg[i])) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->expr.arg[i] = a;
    if(i == 0) closure_set_ready(c, closure_is_ready(a));
  } else {
    arg_noexpand(&c->expr.arg[i], a);
    if(closure_is_ready(c->expr.arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->expr.arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void close_placeholders(cell_t *c) {
  if(!is_closure(c) ||
     closure_is_ready(c)) return;
  if(is_placeholder(c)) {
    closure_set_ready(c, closure_in(c) == 0 ? true : closure_is_ready(c->expr.arg[0]));
  } else if(is_data(c->expr.arg[0])) {
    close_placeholders(c->expr.arg[0]);
  }
}

bool is_fail(cell_t const *c) {
  return (is_value(c) && c->value.type & T_FAIL) != 0;
}

bool is_any(cell_t const *c) {
  return (is_value(c) && c->value.type & T_EXCLUSIVE) == T_ANY;
}

#if INTERFACE
#define traverse(r, action, flags)                              \
  do {                                                          \
    csize_t i = 0, n = 0;                                       \
    cell_t **p;                                                 \
    if(is_value(r)) {                                           \
      if(((flags) & PTRS) &&                                    \
         (r->value.type & T_EXCLUSIVE) == T_LIST) {             \
        n = list_size(r);                                       \
        for(i = 0; i < n; ++i) {                                \
          p = (r)->value.ptr + i;                               \
          action                                                \
        }                                                       \
      }                                                         \
    } else if((flags) & ARGS) {                                 \
      csize_t  __in = closure_in(r);                            \
      i = (~(flags) & ARGS_IN) ? __in : closure_next_child(r);  \
      n = (~(flags) & ARGS_OUT) ? __in : closure_args(r);       \
      for(; i < n; ++i) {                                       \
        p = (r)->expr.arg + i;                                  \
        if(*p) {action}                                         \
      }                                                         \
    }                                                           \
    if((flags) & ALT) {                                         \
      p = &(r)->alt;                                            \
      action                                                    \
    }                                                           \
  } while(0)
#endif

cell_t *arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *l = _arg_nd(c, a, r);
  r = r->tmp ? r->tmp : r;
  clean_tmp(l);
  //check_tmps();
  return r;
}

cell_t *_arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *l = 0;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  csize_t i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->expr.arg[0]))) {
    l = mutate(c, r, 1);
    c = c->tmp ? c->tmp : c;
    r = r->tmp ? r->tmp : r;
    c->expr.arg[0] = a;
    r->value.ptr[list_size(r)-1] = c; // ***
  } else if(!is_data(c->expr.arg[i])) {
    l = mutate(c, r, 0);
    c = c->tmp ? c->tmp : c;
    c->expr.arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->expr.arg[i] = a;
    if(i == 0 && !is_placeholder(c)) closure_set_ready(c, closure_is_ready(a));
  } else {
    l = _arg_nd(c->expr.arg[i], a, r);
    c = c->tmp ? c->tmp : c;
    if(!is_placeholder(c) &&
       closure_is_ready(c->expr.arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->expr.arg[0]; // decrement offset
    }
  }
  return l;
}

cell_t *copy(cell_t const *c) {
  csize_t size = closure_cells(c);
  cell_t *new = closure_alloc_cells(size);
  memcpy(new, c, size * sizeof(cell_t));
  return new;
}

cell_t *traverse_ref(cell_t *c, uint8_t flags) {
  traverse(c, {
      if(is_closure(*p)) *p = ref(*p);
    }, flags);
  return c;
}

void store_fail(cell_t *c, cell_t *alt) {
  closure_shrink(c, 1);
  memset(&c->value, 0, sizeof(c->value));
  c->func = func_value;
  c->value.type = T_FAIL;
  c->alt = alt;
}

void store_var(cell_t *c, type_t t) {
  closure_shrink(c, 1);
  c->func = func_value;
  c->value.type = T_VAR | t;
  c->size = 0;
}

void fail(cell_t **cp) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  cell_t *alt = ref(c->alt);
  drop(c);
  if(c->func) {
    traverse(c, {
        cell_t *x = clear_ptr(*p);
        drop(x);
      }, ARGS_IN);
    closure_shrink(c, 1);
    memset(&c->value, 0, sizeof(c->value));
    c->func = func_value;
    c->value.type = T_FAIL;
  }
  *cp = alt;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  refcount_t n = c->n;
  r->func = func_value;
  trace(c, r, tt_reduction, 0);
  drop_multi(c->expr.arg, closure_in(c));
  alt_set_ref(r->value.alt_set);
  csize_t size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    assert(is_cell(r));
    traverse_ref(r, ALT | PTRS);
    drop(r);
    c->n = n;
   } else { /* TODO: must copy if not cell */
    /*
    if(!is_cell(r)) {
      cell_t *t = closure_alloc_cells(size);
      memcpy(t, r, sizeof(cell_t) * size);
      r = t;
    }
    */
    store_lazy(cp, c, r, 0);
  }
}

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, refcount_t n) {
  c = clear_ptr(c);
  if(c) {
    assert(is_closure(c));
    c->n += n;
  }
  return c;
}

bool is_nil(cell_t const *c) {
  return !c;
}

bool is_list(cell_t const *c) {
  return c && is_value(c) && (c->value.type & T_EXCLUSIVE) == T_LIST;
}

bool is_var(cell_t const *c) {
  return c && is_value(c) && (c->value.type & T_VAR) != 0;
}

bool is_weak(cell_t const *p, cell_t const *c) {
  return c && is_dep(c) && (!c->expr.arg[0] || c->expr.arg[0] == p);
}

void drop(cell_t *c) {
  if(!is_cell(c)) return;
  assert(is_closure(c));
  if(!c->n) {
    cell_t *p;
    traverse(c, {
        cell_t *x = clear_ptr(*p);
        drop(x);
      }, ALT | ARGS_IN | PTRS);
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
    if(is_value(c)) alt_set_drop(c->value.alt_set);
    if(c->func == func_id) alt_set_drop((alt_set_t)c->expr.arg[1]);
    closure_free(c);
  } else {
    --c->n;
  }
}

cell_t *conc_alt(cell_t *a, cell_t *b) {
  if(!a) return b;
  if(!b) return a;
  cell_t *p = a, *r;
  while((r = p->alt)) p = r;
  p->alt = b;
  return a;
}

cell_t *dep(cell_t *c) {
  cell_t *n = closure_alloc(1);
  n->func = func_dep;
  n->expr.arg[0] = c;
  return n;
}

/* todo: propagate types here */
bool func_dep(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  /* rely on another cell for reduction */
  /* don't need to drop arg, handled by other function */
  /* must make weak reference strong during reduction */
  cell_t *p = ref(c->expr.arg[0]);
  if(p) {
    c->expr.arg[0] = 0;
    reduce_dep(&p);
    drop(p);
  } else {
    // shouldn't happen
    // can be caused by circular reference
    fail(cp);
  }
  return false;
}

bool is_dep(cell_t const *c) {
  return c->func == func_dep;
}

// compose a with n outputs into the list b (destructive) returning the result
// *** fix arg()'s
cell_t *compose_expand(cell_t *a, csize_t n, cell_t *b) {
  assert(is_closure(a) &&
         is_list(b));
  assert(n);
  bool ph_a = is_placeholder(a);
  csize_t i, bs = list_size(b);
  if(bs) {
    cell_t **l = &b->value.ptr[bs-1];
    cell_t *d = 0;
    if(ph_a && is_placeholder(*l)) {
      *l = compose_placeholders(a, *l);
      return b;
    }
    while((ph_a || n > 1) &&
          !closure_is_ready(*l)) {
      d = dep(NULL);
      arg(l, d);
      arg(&a, d);
      d->expr.arg[0] = ref(a);
      if(ph_a) ++a->expr.out;
      else --n;
    }
    if(!closure_is_ready(*l)) {
      arg(l, a);
      // *** messy
      for(i = closure_in(*l); i < closure_args(*l); ++i) {
        drop((*l)->expr.arg[i]->expr.arg[0]);
        (*l)->expr.arg[i]->expr.arg[0] = ref(*l);
      }
      return b;
    }
  }

  b = expand(b, n);

  for(i = 0; i < n-1; ++i) {
    cell_t *d = dep(ref(a));
    b->value.ptr[bs+i] = d;
    arg(&a, d);
  }
  b->value.ptr[bs+n-1] = a;
  return b;
}

cell_t *pushl_val(intptr_t x, cell_t *c) {
  csize_t n = val_size(c);
  c = expand(c, 1);
  c->value.integer[n] = x;
  return c;
}

cell_t *pushl_nd(cell_t *a, cell_t *b) {
  assert(is_closure(a) &&
         is_list(b));

  csize_t n = list_size(b);
  if(n) {
    cell_t *l = b->value.ptr[n-1];
    if(!closure_is_ready(l)) {
      cell_t *_b = arg_nd(l, a, b);
      if(is_placeholder(l)) trace(_b->value.ptr[n-1], l, tt_copy, 0);
      return _b;
    }
  }

  cell_t *e = expand(b, 1);
  e->value.ptr[n] = a;
  return e;
}

alt_set_t as(unsigned int k, unsigned int v) {
  assert(k < AS_SIZE);
  return ((alt_set_t)1 << (k + AS_SIZE)) |
    (((alt_set_t)v & 1) << k);
}

alt_set_t as_intersect(alt_set_t a, alt_set_t b) {
  return a & b;
}

alt_set_t as_union(alt_set_t a, alt_set_t b) {
  return a | b;
}

alt_set_t as_conflict(alt_set_t a, alt_set_t b) {
  return ((a & b) >> AS_SIZE) &
    ((a ^ b) & (((alt_set_t)1<<AS_SIZE)-1));
}

alt_set_t as_overlap(alt_set_t a, alt_set_t b) {
  return a | (b & (a >> AS_SIZE));
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

//cell_t *collect;

/* reassemble a fragmented cell */
/*
bool func_collect(cell_t *c) {
  collect->alt = c->alt;
  collect->n = c->n;
  collect->func = (reduce_t *)c->arg[0];
  cell_t **dest = collect->arg;
  cell_t **src = c->arg+1;
  cell_t *prev = 0;
  int n = sizeof(c->arg)-1;
  do {
    while(--n > 0) {
      *dest++ = *src++;
    }
    src = (cell_t **)*src;
    cell_free(prev);
    prev = (cell_t *)src;
    n = sizeof(cell_t)-1;
    src++;
  } while(src);
  bool b = reduce(collect);
  return store_reduced(c, collect, b);
}
*/

void *lookup(void *table, size_t width, size_t rows, seg_t key_seg) {
  size_t low = 0, high = rows, pivot;
  char const *key = key_seg.s;
  size_t key_length = key_seg.n;
  void *entry, *ret = 0;
  if(key_length > width) key_length = width;
  while(high > low) {
    pivot = low + ((high - low) >> 1);
    entry = (uint8_t *)table + width * pivot;
    int c = strncmp(key, entry, key_length);
    if(c == 0) {
      /* keep looking for a lower key */
      ret = entry;
      high = pivot;
    } else if(c < 0) high = pivot;
    else low = pivot + 1;
  }
  return ret;
}

void *lookup_linear(void *table, size_t width, size_t rows, seg_t key_seg) {
  char const *key = key_seg.s;
  size_t key_length = key_seg.n;
  uint8_t *entry = table;
  unsigned int rows_left = rows;
  if(key_length > width) key_length = width;
  while(rows_left-- && *entry) {
    if(!strncmp(key, (void *)entry, key_length)) return entry;
    entry += width;
  }
  return NULL;
}

void check_tmps() {
  size_t i = 0;
  cell_t *p;
  while(i < LENGTH(cells)) {
    p = &cells[i];
    if(is_closure(p)) {
      /*
      if(p->tmp) {
        printf("<<%d %d>>\n", i, (int)p->tmp);
        drop(clear_ptr(p->tmp));
        p->tmp = 0;
      }
      */
      assert(!p->tmp);
      i += closure_cells(p);
    } else ++i;
  }
}

/* ref count not from deps */
refcount_t nondep_n(cell_t *c) {
  if(!is_closure(c)) return 0;
  refcount_t nd = c->n;
  if(is_dep(c)) return nd;
  else if(!is_value(c)) {
    csize_t n = closure_args(c);
    while(n-- &&
          is_closure(c->expr.arg[n]) &&
          is_dep(c->expr.arg[n]) &&
          c->expr.arg[n]->expr.arg[0] == c) --nd;
  }
  return nd;
}

void mutate_update(cell_t *r, bool m) {
  traverse(r, {
      cell_t *c = clear_ptr(*p);
      if(is_closure(c)) {
        if(c->tmp) {
          *p = ref(c->tmp);
          if (m) --c->n;
        } else if (!m) ref(c);
      }
    }, ARGS_IN | PTRS | ALT);

  traverse(r, {
      cell_t *c = clear_ptr(*p);
      if(c && c->tmp) {
        *p = c->tmp;
      }
    }, ARGS_OUT);
}

bool mutate_sweep(cell_t *c, cell_t *r, cell_t **l, bool u, int exp) {
  r = clear_ptr(r);
  if(!is_closure(r)) return false;
  if(r->tmp) return true;
  u &= !nondep_n(r);

  bool dirty = false;
  if(r == c) {
    dirty = true;
  } else {
    // prevent looping
    r->tmp = r;
    traverse(r, {
        dirty |= mutate_sweep(c, *p, l, u, exp);
      }, ARGS | PTRS | ALT);
    r->tmp = 0;
  }

  if(dirty) {
    if(u) {
      // if unique, rewrite pointers inplace
      mutate_update(r, true);
    } else {
      // otherwise, add to the list and defer
      cell_t *n = copy(r);
      n->n = -1;
      if(exp && r == c) n = expand_inplace(n, exp); // *** TODO optimize
      n->tmp = *l;
      r->tmp = n;
      *l = r;
    }
  }
  return dirty && !u;
}

cell_t *mutate(cell_t *c, cell_t *r, int exp) {
  cell_t *l = 0;

  // make sure c is copied if it is to be expanded
  if(exp) ref(c);

  mutate_sweep(c, r, &l, true, exp);
  if(r->tmp) {
    ++r->tmp->n;
    --r->n;
  }

  if(exp) {
    --c->n;
  }

  // traverse list and rewrite pointers
  cell_t *li = l;
  while(li) {
    cell_t *t = li->tmp;
    mutate_update(t, false);
    li = t->tmp;
  }
  return l;
}

void clean_tmp(cell_t *l) {
  while(l) {
    cell_t *next = l->tmp;
    l->tmp = 0;
    if(l->n + 1 == 0) {
      l->n = 0;
      drop(l);
    }
    l = next;
  }
}

cell_t *mod_alt(cell_t *c, cell_t *alt, alt_set_t alt_set) {
  cell_t *n;
  if(c->alt == alt &&
     c->value.alt_set == alt_set) return c;
  alt_set_ref(alt_set);
  if(!c->n) {
    n = c;
    drop(c->alt);
    alt_set_drop(c->value.alt_set);
  } else {
    --c->n;
    n = copy(c);
    traverse_ref(n, ARGS | PTRS);
    n->n = 0;
  }
  n->alt = alt;
  n->value.alt_set = alt_set;
  return n;
}

alt_set_t alt_set_ref(alt_set_t alt_set) {
  /*
  uintptr_t bit = (uintptr_t)1 << (sizeof(uintptr_t) * 4);
  uintptr_t *live = alt_live;
  while(bit) {
    if(alt_set & bit) ++*live;
    ++live;
    bit <<= 1;
  }
  */
  return alt_set;
}

alt_set_t alt_set_drop(alt_set_t alt_set) {
  /*
  uintptr_t bit = (uintptr_t)1 << (sizeof(uintptr_t) * 4);
  uintptr_t *live = alt_live;
  while(bit) {
    if(alt_set & bit) {
      assert(*live);
      --*live;
    }
    ++live;
    bit <<= 1;
  }
  */
  return alt_set;
}

uint8_t new_alt_id(UNUSED uintptr_t n) {
  /*
  uint8_t r = 0;
  while(r < ALT_SET_IDS) {
    if(alt_live[r]) ++r;
    else {
      alt_live[r] = n;
      return r;
    }
  }
  assert(false);
  return -1;
  */
  return alt_cnt++;
}

void drop_multi(cell_t **a, csize_t n) {
  for(csize_t i = 0; i < n; i++) drop(*a++);
}

void store_lazy(cell_t **cp, cell_t *c, cell_t *r, alt_set_t alt_set) {
  if(c->n || alt_set) {
    closure_shrink(c, 1);
    c->func = func_id;
    c->size = 1;
    c->expr.out = 0;
    c->expr.arg[0] = r;
    if(c->n) ref(r);
    c->expr.arg[1] = (cell_t *)alt_set;
  }

  if(!alt_set) {
    if(c->n == 0) closure_free(c);
    else --c->n;
    *cp = r;
  }
}

void store_lazy_dep(cell_t *c, cell_t *d, cell_t *r, alt_set_t alt_set) {
  if(d) {
    drop(c);
    d->func = func_id;
    d->size = 1;
    d->expr.out = 0;
    d->expr.arg[0] = r;
    d->expr.arg[1] = (cell_t *)alt_set;
  } else drop(r);
}

// this shouldn't reduced directly, but is called through reduce_partial from func_dep
bool func_placeholder(cell_t **cp, UNUSED type_t t) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  csize_t in = closure_in(c), n = closure_args(c);
  for(csize_t i = 0; i < in; ++i) {
    if(!reduce(&c->expr.arg[i], T_ANY)) goto fail;
    trace(c->expr.arg[i], c, tt_force, i);
  }
  for(csize_t i = in; i < n; ++i) {
    cell_t *d = c->expr.arg[i];
    if(d && is_dep(d)) {
      drop(c);
      store_var(d, 0);
      trace(d, c, tt_placeholder_dep, i);
    }
  }
  return false;

 fail:
  fail(cp);
  return false;
}

bool func_self(cell_t **cp, UNUSED type_t t) {
  func_placeholder(cp, t);
  store_reduced(cp, var(t));
  return true;
}

bool is_placeholder(cell_t const *c) {
  return c && clear_ptr(c->func) == (void *)func_placeholder;
}

bool entangle(alt_set_t *as, cell_t *c) {
  alt_set_t cas = c->value.alt_set;
  return !cas ||
    (!as_conflict(*as, cas) &&
     (*as |= cas, true));
}
