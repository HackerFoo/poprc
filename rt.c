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


// Cell storage array
// NOTE: make sure &cells > 255
#ifdef EMSCRIPTEN
cell_t cells[1<<10] = {};
#else
cell_t cells[1<<16];
#endif
cell_t *cells_ptr;

// Counter of used alt ids
uint8_t alt_cnt = 0;

// Predefined failure cell
cell_t fail_cell = {
  .func = func_reduced,
  .type = T_FAIL
};

// Maximum number of alts
#define AS_SIZE (sizeof(intptr_t) * 4)
#define ALT_SET_IDS AS_SIZE

// to catch errors that result in large allocations
#define MAX_ALLOC_SIZE 32

// OBSOLETE Array for tracking 'live' alt ids
uintptr_t alt_live[sizeof(intptr_t) * 4];

// Structs for storing statistics
measure_t measure, saved_measure;

// Default tracing function that does nothing
void trace_noop(UNUSED cell_t *c, UNUSED cell_t *r, UNUSED trace_type_t tt, unsigned int n) {}

// Pointer to tracing function
void (*trace)(cell_t *, cell_t *, trace_type_t, unsigned int) = trace_noop;

// Set the tracing function
void set_trace(void (*t)(cell_t *, cell_t *, trace_type_t, unsigned int)) {
  trace = t ? t : trace_noop;
}

// #define CHECK_CYCLE

// Is `p` a pointer?
bool is_data(void const *p) {
  return (intptr_t)p > 255;
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
  return !is_marked(c->func, 1);
}

// Set the readiness of closure `c` to state `r`
void closure_set_ready(cell_t *c, bool r) {
  assert(is_closure(c));
  c->func = (reduce_t *)mark_ptr(clear_ptr(c->func, 1), r ? 0 : 1);
}

// Duplicate c to c->alt and return it
cell_t *dup_alt(cell_t *c, unsigned int n, cell_t *b) {
  unsigned int i = 0, in = closure_in(c), out = 0;
  assert(n < in);
  cell_t *a = copy(c);

  // ref args
  for(; i < in; ++i) {
    if(i != n) ref(a->arg[i]);
  }

  // update deps
  for(; i < c->size; ++i) {
    if(a->arg[i]) a->arg[i] = dep(a);
    c->arg[i]->alt = conc_alt(a->arg[i], c->arg[i]->alt);
    ++out;
  }

  a->arg[n] = b;
  a->n = out;
  c->alt = a;
  return a;
}

// Lift alternates from c->arg[n] to c
void split_arg(cell_t *c, unsigned int n) {
  cell_t
    *a = c->arg[n],
    *p = c,
    **pa;
  if(!a || !a->alt || is_marked(a, 1)) return;
  do {
    pa = &p->arg[n];
    if(*pa == a) {
      // insert a copy with the alt arg
      p = dup_alt(p, n, ref((*pa)->alt))->alt;
      // mark the arg
      *pa = mark_ptr(*pa, 1);
    } else p = p->alt;
  } while(p);
}

// Reduce then split c->arg[n]
bool reduce_arg(cell_t *c,
                unsigned int n,
                alt_set_t *as,
                type_t t) {
  bool r = reduce(&c->arg[n], t);
  split_arg(c, n);
  return r && entangle(as, clear_ptr(c->arg[n], 1));
}

// Lift alternates from all args
cell_t *closure_split(cell_t *c, unsigned int s) {
  unsigned int i;
  for(i = 0; i < s; ++i) {
    split_arg(c, i);
  }
  for(i = 0; i < s; ++i) {
    c->arg[i] = clear_ptr(c->arg[i], 1);
  }
  return c->alt;
}

// Clear the flags bits in args
void clear_flags(cell_t *c) {
  int i = 0;
  for(; i < c->size; ++i) {
    c->arg[i] = clear_ptr(c->arg[i], 3);
  }
}

cell_t *closure_split1(cell_t *c, int n) {
  if(!c->arg[n]->alt) return c->alt;
  return dup_alt(c, n, ref(c->arg[n]->alt));
}

// Reduce *cp with type t
bool reduce(cell_t **cp, type_rep_t t) {
  cell_t *c;
  while((c = clear_ptr(*cp, 1))) {
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
  cell_t *c = clear_ptr(*cp, 1);
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
  assert(is_cell(p) && !is_closure(p) && is_cell(cells_ptr->next));
  cells_ptr = cells_ptr->next;
  return p;
}

#ifdef CHECK_CYCLE
bool check_cycle() {
  unsigned int i = 0;
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
  unsigned int const n = LENGTH(cells)-1;

  // zero the cells
  memset(&cells, 0, sizeof(cells));
  memset(&alt_live, 0, sizeof(alt_live));

  // set up doubly-linked pointer ring
  for(unsigned int i = 0; i < n; i++) {
    cells[i].prev = &cells[i-1];
    cells[i].next = &cells[i+1];
  }
  cells[0].prev = &cells[n-1];
  cells[n-1].next = &cells[0];

  cells_ptr = &cells[0];
  alt_cnt = 0;
}

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  cell_t *prev = c->prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  prev->next = next;
  next->prev = prev;
  measure.alloc_cnt++;
  if(++measure.current_alloc_cnt > measure.max_alloc_cnt)
    measure.max_alloc_cnt = measure.current_alloc_cnt;
}

cell_t *closure_alloc(unsigned int args) {
  cell_t *c = closure_alloc_cells(calculate_cells(args));
  c->size = args;
  return c;
}

cell_t *closure_alloc_cells(unsigned int size) {
  assert(size < MAX_ALLOC_SIZE);
  cell_t *ptr = cells_next(), *c = ptr;
  cell_t *mark = ptr;
  unsigned int cnt = 0;
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
  for(unsigned int i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  memset(c, 0, sizeof(cell_t)*size);
  return c;
}

#define calc_size(f, n)                         \
  ((sizeof(cell_t)                              \
    + ((uintptr_t)&(((cell_t *)0)->f[n]) - 1))  \
   / sizeof(cell_t))

unsigned int calculate_cells(unsigned int n) {
  return calc_size(arg, n);
}

unsigned int calculate_list_size(unsigned int n) {
  return calc_size(ptr, n);
}

unsigned int calculate_val_size(unsigned int n) {
  return calc_size(val, n);
}

unsigned int closure_cells(cell_t const *c) {
  return calculate_cells(closure_args(c));
}

void cell_free(cell_t *c) {
  c->func = 0;
  c->next = cells_ptr;
  c->prev = cells_ptr->prev;
  cells_ptr->prev = c;
  c->prev->next = c;
}

void closure_shrink(cell_t *c, unsigned int s) {
  if(!is_cell(c)) return;
  unsigned int i, size = closure_cells(c);
  if(size > s) {
    assert(is_closure(c));
    for(i = s; i < size; i++) {
      c[i].func = 0;
      c[i].prev = &c[i-1];
      c[i].next = &c[i+1];
    }
    c[s].prev = cells_ptr->prev;
    cells_ptr->prev->next = &c[s];
    c[size-1].next = cells_ptr;
    cells_ptr->prev = &c[size-1];
    measure.current_alloc_cnt -= size - s;
  }
}

void closure_free(cell_t *c) {
  closure_shrink(c, 0);
}

bool type_match(type_t t, cell_t const *c) {
  type_t ta = t & T_EXCLUSIVE;
  type_t tb = c->type & T_EXCLUSIVE;
  return ta == T_ANY || tb == T_ANY || ta == tb;
}

bool func_reduced(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  assert(is_closure(c));
  measure.reduce_cnt--;
  if(c->type != T_FAIL &&
     type_match(t, c)) {
    if(is_any(c)) {
      /* create placeholder */
      if((t & T_EXCLUSIVE) == T_LIST) {
        c->ptr[0] = func(func_placeholder, 0, 1);
        c->size = 2;
        c->type &= ~T_TRACED;
      }
      c->type |= t;
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
  c->func = func_reduced;
  c->type = T_ANY;
  c->val[0] = x;
  return c;
}

cell_t *var(type_t t) {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_VAR | t;
  return c;
}

cell_t *vector(uint32_t n) {
  cell_t *c = closure_alloc(n+1);
  c->func = func_reduced;
  c->type = T_ANY;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(2);
  c->func = func_reduced;
  c->type = T_LIST;
  c->ptr[0] = x;
  return c;
}

cell_t *empty_list() {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_LIST;
  return c;
}

cell_t *make_list(unsigned int n) {
  cell_t *c = closure_alloc(n + 1);
  c->func = func_reduced;
  c->type = T_LIST;
  return c;
}

cell_t *append(cell_t *a, cell_t *b) {
  unsigned int n = list_size(b);
  unsigned int n_a = list_size(a);
  cell_t *e = expand(a, n);
  while(n--) e->ptr[n + n_a] = b->ptr[n];
  return e;
}

cell_t *expand(cell_t *c, unsigned int s) {
  if(!c) return 0;
  unsigned int n = closure_args(c);
  unsigned int cn_p = calculate_cells(n);
  unsigned int cn = calculate_cells(n + s);
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
    if(is_reduced(c)) alt_set_ref(c->alt_set);
    drop(c);
    return new;
  }
}

cell_t *expand_inplace(cell_t *c, unsigned int s) {
  uintptr_t n = c->n;
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->arg[s], &c->arg[0], (c->size - 1) * sizeof(cell_t *));
  unsigned int i;
  for(i = c->size - c->out; i < c->size; ++i) {
    if(c->arg[i]) c->arg[i]->arg[0] = c;
  }
  return c;
}

cell_t *expand_inplace_dep(cell_t *c, unsigned int s) {
  uintptr_t n = c->n;
  unsigned int in = closure_in(c);
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->arg[in+s], &c->arg[in], c->out * sizeof(cell_t *));
  c->out += s;
  unsigned int i;
  for(i = c->size - c->out + s; i < c->size; ++i) {
    if(c->arg[i]) c->arg[i]->arg[0] = c;
  }
  return c;
}

cell_t *compose_placeholders(cell_t *a, cell_t *b) {
  unsigned int i;
  unsigned int b_in = closure_in(b);
  unsigned int b_out = closure_out(b);
  cell_t *c = expand(b, a->size);
  memmove(c->arg + a->size + b_in,
          c->arg + b_in,
          b_out * sizeof(cell_t *));
  for(i = 0; i < closure_in(a); ++i)
    c->arg[b_in + i] = ref(a->arg[i]);
  for(; i < a->size; ++i)
    c->arg[b_in + i] = a->arg[i];
  drop(a);
  cell_t *ab[] = {a, b};
  trace(c, (cell_t *)ab, tt_compose_placeholders, 0);
  return c;
}

cell_t *compose_nd(cell_t *a, cell_t *b) {
  unsigned int n = list_size(b);
  unsigned int n_a = list_size(a);
  unsigned int i = 0;
  if(n && n_a) {
    cell_t *l;
    while(!closure_is_ready(l = b->ptr[n-1]) && i < n_a) {
      cell_t *x = a->ptr[i];
      if(is_placeholder(x)) {
        if(is_placeholder(l)) {
          b->ptr[n-1] = compose_placeholders(x, l);
          ++i;
          break;
        }
        a->ptr[i] = x = expand_inplace_dep(x, 1);
        b = arg_nd(l, x->arg[closure_in(x)] = dep(ref(x)), b);
      } else {
        b = arg_nd(l, ref(x), b);
        ++i;
      }
    }
  }
  cell_t *e = expand(b, n_a - i);
  for(unsigned j = n; i < n_a; ++i, ++j) {
    e->ptr[j] = ref(a->ptr[i]);
  }
  drop(a);
  return e;
}

bool is_reduced(cell_t const *c) {
  return c && c->func == func_reduced;
}

// max offset is 255
bool is_offset(cell_t const *c) {
  return !((uintptr_t)c & ~0xff);
}

cell_t *func(reduce_t *f, unsigned int in, unsigned int out) {
  assert(out > 0);
  unsigned int args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->out = out - 1;
  c->func = f;
  if(args) c->arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, !args && f != func_placeholder);
  return c;
}

#define cell_offset(f) ((cell_t **)&(((cell_t *)0)->f))

unsigned int list_size(cell_t const *c) {
  return c->size ? c->size - 1 : 0;
}

unsigned int val_size(cell_t const *c) {
  return c->size ? c->size - 1 : 0;
}

unsigned int closure_args(cell_t const *c) {
  assert(is_closure(c));
  return c->size;
}

unsigned int closure_in(cell_t const *c) {
  assert(is_closure(c) && !is_reduced(c));
  return c->size - c->out;
}

unsigned int closure_out(cell_t const *c) {
  assert(is_closure(c) && !is_reduced(c));
  return c->out;
}

unsigned int closure_next_child(cell_t const *c) {
  assert(is_closure(c));
  return is_offset(c->arg[0]) ? (intptr_t)c->arg[0] : 0;
}

/* arg is destructive to *cp */
void arg(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  unsigned int i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->arg[0]))) {
    c = expand_inplace(c, 1);
    c->arg[0] = a;
  } else if(!is_data(c->arg[i])) {
    c->arg[0] = (cell_t *)(intptr_t)
      (i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0 && !is_placeholder(c))
      closure_set_ready(c, closure_is_ready(a));
  } else {
    arg(&c->arg[i], a);
    if(!is_placeholder(c) &&
       closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void arg_noexpand(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  unsigned int i = closure_next_child(c);
  if(!is_data(c->arg[i])) {
    c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0) closure_set_ready(c, closure_is_ready(a));
  } else {
    arg_noexpand(&c->arg[i], a);
    if(closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void close_placeholders(cell_t *c) {
  if(!is_closure(c) ||
     closure_is_ready(c)) return;
  if(is_placeholder(c)) {
    closure_set_ready(c, closure_in(c) == 0 ? true : closure_is_ready(c->arg[0]));
  } else if(is_data(c->arg[0])) {
    close_placeholders(c->arg[0]);
  }
}

bool is_fail(cell_t const *c) {
  return (c->type & T_FAIL) != 0;
}

bool is_any(cell_t const *c) {
  return (c->type & T_EXCLUSIVE) == T_ANY;
}

#if INTERFACE
#define traverse(r, action, flags)                              \
  do {                                                          \
    unsigned int i = 0, n = 0;                                  \
    cell_t **p;                                                 \
    if(is_reduced(r)) {                                         \
      if(((flags) & PTRS) &&                                    \
                is_list(r)) {                                   \
        n = list_size(r);                                       \
        for(i = 0; i < n; ++i) {                                \
          p = (r)->ptr + i;                                     \
          action                                                \
        }                                                       \
      }                                                         \
    } else if((flags) & ARGS) {                                 \
      unsigned int  __in = closure_in(r);                       \
      i = (~(flags) & ARGS_IN) ? __in : closure_next_child(r);  \
      n = (~(flags) & ARGS_OUT) ? __in : closure_args(r);       \
      for(; i < n; ++i) {                                       \
        p = (r)->arg + i;                                       \
        if(*p) {action}                                         \
      }                                                         \
    }                                                           \
    if((flags) & ALT) {                                         \
      p = &(r)->alt;                                            \
      action                                                    \
    }                                                           \
  } while(0)
#endif

void traverse_mark_alt(cell_t *c) {
  traverse(c, {
      if(*p && !is_marked((*p)->alt, 1)) {
        (*p)->alt = mark_ptr((*p)->alt, 1);
        traverse_mark_alt(*p);
      }
    }, ARGS | PTRS);
}

void traverse_clear_alt(cell_t *c) {
  traverse(c, {
      if(*p && is_marked((*p)->alt, 1)) {
        (*p)->alt = clear_ptr((*p)->alt, 1);
        traverse_clear_alt(*p);
      }
    }, ARGS | PTRS);
}

cell_t *arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *l = _arg_nd(c, a, r);
  r = r->tmp ? clear_ptr(r->tmp, 3) : r;
  clean_tmp(l);
  //check_tmps();
  return r;
}

cell_t *_arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *l = 0;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  unsigned int i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->arg[0]))) {
    l = mutate(c, r, 1);
    c = c->tmp ? clear_ptr(c->tmp, 3) : c;
    r = r->tmp ? clear_ptr(r->tmp, 3) : r;
    c->arg[0] = a;
    r->ptr[list_size(r)-1] = c; // ***
  } else if(!is_data(c->arg[i])) {
    l = mutate(c, r, 0);
    c = c->tmp ? clear_ptr(c->tmp, 3) : c;
    c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0 && !is_placeholder(c)) closure_set_ready(c, closure_is_ready(a));
  } else {
    l = _arg_nd(c->arg[i], a, r);
    c = c->tmp ? clear_ptr(c->tmp, 3) : c;
    if(!is_placeholder(c) &&
       closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->arg[0]; // decrement offset
    }
  }
  return l;
}

cell_t *copy(cell_t const *c) {
  unsigned int size = closure_cells(c);
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
  memset(c->arg, 0, sizeof(c->arg));
  c->func = func_reduced;
  c->type = T_FAIL;
  c->alt = alt;
}

void store_var(cell_t *c, type_rep_t t) {
  closure_shrink(c, 1);
  c->func = func_reduced;
  c->type = T_VAR | t;
  c->size = 0;
}

void fail(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *alt = ref(c->alt);
  drop(c);
  if(c->func) {
    traverse(c, {
        cell_t *x = clear_ptr(*p, 3);
        drop(x);
      }, ARGS_IN);
    closure_shrink(c, 1);
    memset(c->arg, 0, sizeof(c->arg));
    c->func = func_reduced;
    c->type = T_FAIL;
  }
  *cp = alt;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = clear_ptr(*cp, 3);
  unsigned int n = c->n;
  r->func = func_reduced;
  trace(c, r, tt_reduction, 0);
  drop_multi(c->arg, closure_in(c));
  alt_set_ref(r->alt_set);
  unsigned int size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    if(is_cell(r)) {
      traverse_ref(r, ALT | PTRS);
      drop(r);
    }
    c->n = n;
   } else { /* TODO: must copy if not cell */
    if(!is_cell(r)) {
      cell_t *t = closure_alloc_cells(size);
      memcpy(t, r, sizeof(cell_t) * size);
      r = t;
    }
    store_lazy(cp, c, r);
  }
}

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, unsigned int n) {
  c = clear_ptr(c, 3);
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
  return c && is_reduced(c) && (c->type & T_EXCLUSIVE) == T_LIST;
}

bool is_var(cell_t const *c) {
  return c && is_reduced(c) && (c->type & T_VAR) != 0;
}

bool is_weak(cell_t const *p, cell_t const *c) {
  return c && is_dep(c) && (!c->arg[0] || c->arg[0] == p);
}

void drop(cell_t *c) {
  if(!is_cell(c) || !is_closure(c)) return;
  if(!c->n) {
    cell_t *p;
    traverse(c, {
        cell_t *x = clear_ptr(*p, 3);
        /* !is_marked condition needed */
        /* during _modify_copy2 */
        if(!is_marked(*p, 2)) {
          drop(x);
        }
      }, ALT | ARGS_IN | PTRS);
    if(is_dep(c) && !is_reduced(p = c->arg[0]) && is_closure(p)) {
      /* mark dep arg as gone */
      unsigned int n = closure_args(p);
      while(n--) {
        if(p->arg[n] == c) {
          p->arg[n] = 0;
          break;
        }
      }
    }
    if(is_reduced(c)) alt_set_drop(c->alt_set);
    if(c->func == func_id) alt_set_drop((alt_set_t)c->arg[1]);
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
  n->arg[0] = c;
  return n;
}

/* todo: propagate types here */
bool func_dep(cell_t **cp, UNUSED type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  /* rely on another cell for reduction */
  /* don't need to drop arg, handled by other function */
  /* must make weak reference strong during reduction */
  cell_t *p = ref(c->arg[0]);
  reduce_dep(&p);
  drop(p);
  return false;
}

bool is_dep(cell_t const *c) {
  return c->func == func_dep;
}

// compose a with n outputs into the list b (destructive) returning the result
// *** fix arg()'s
cell_t *compose_expand(cell_t *a, unsigned int n, cell_t *b) {
  assert(is_closure(a) &&
         is_closure(b) && is_list(b));
  assert(n);
  bool ph_a = is_placeholder(a);
  unsigned int i, bs = list_size(b);
  if(bs) {
    cell_t **l = &b->ptr[bs-1];
    cell_t *d = 0;
    unsigned int nd = 0;
    if(ph_a && is_placeholder(*l)) {
      *l = compose_placeholders(a, *l);
      return b;
    }
    while((ph_a || n > 1) &&
          !closure_is_ready(*l)) {
      d = dep(NULL);
      arg(l, d);
      arg(&a, d);
      d->arg[0] = ref(a);
      ++nd;
      if(ph_a) ++a->out;
      else --n;
    }
    if(!closure_is_ready(*l)) {
      arg(l, a);
      // *** messy
      for(i = closure_in(*l); i < closure_args(*l); ++i) {
        drop((*l)->arg[i]->arg[0]);
        (*l)->arg[i]->arg[0] = ref(*l);
      }
      return b;
    }
  }

  b = expand(b, n);

  for(i = 0; i < n-1; ++i) {
    cell_t *d = dep(ref(a));
    b->ptr[bs+i] = d;
    arg(&a, d);
  }
  b->ptr[bs+n-1] = a;
  return b;
}

cell_t *pushl_val(intptr_t x, cell_t *c) {
  unsigned int n = val_size(c);
  c = expand(c, 1);
  c->val[n] = x;
  return c;
}

cell_t *pushl_nd(cell_t *a, cell_t *b) {
  assert(is_closure(a) &&
         is_closure(b) && is_list(b));

  unsigned int n = list_size(b);
  if(n) {
    cell_t *l = b->ptr[n-1];
    if(!closure_is_ready(l)) {
      cell_t *_b = arg_nd(l, a, b);
      if(is_placeholder(l)) trace(_b->ptr[n-1], l, tt_copy, 0);
      return _b;
    }
  }

  cell_t *e = expand(b, 1);
  e->ptr[n] = a;
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

void *lookup(void *table, unsigned int width, unsigned int rows, char const *key) {
  unsigned int low = 0, high = rows, pivot;
  void *entry, *ret = 0;
  int key_length = strnlen(key, width);
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

void *lookup_linear(void *table, unsigned int width, unsigned int rows, char const *key) {
  uint8_t *entry = table;
  unsigned int rows_left = rows;
  unsigned int key_length = strnlen(key, width);
  while(rows_left-- && *entry) {
    if(!strncmp(key, (void *)entry, key_length)) return entry;
    entry += width;
  }
  return NULL;
}

// return a copy of c rooted at r that can be modified without
// affecting else while sharing as much as possible
cell_t *modify_copy(cell_t *c, cell_t *r) {
  cell_t *new = _modify_copy1(c, r, true);
  if(new && new != r) {
    ref(new);
    drop(r);
  }
  if(new) {
    _modify_copy2(new);
    return new;
  } else return r;
}

void check_tmps() {
  unsigned int i = 0;
  cell_t *p;
  while(i < LENGTH(cells)) {
    p = &cells[i];
    if(is_closure(p)) {
      /*
      if(p->tmp) {
        printf("<<%d %d>>\n", i, (int)p->tmp);
        drop(clear_ptr(p->tmp, 3));
        p->tmp = 0;
      }
      */
      assert(!p->tmp);
      i += closure_cells(p);
    } else ++i;
  }
}

void zero_tmps(cell_t *r) {
  if(!r || !r->tmp) return;

  cell_t *t = clear_ptr(r->tmp, 3);
  r->tmp = 0;
  zero_tmps(t);
  drop(t);

  traverse(r, {
      zero_tmps(clear_ptr(*p, 3));
    }, ARGS | PTRS | ALT);
}

/* ref count not from deps */
unsigned int nondep_n(cell_t *c) {
  if(!is_closure(c)) return 0;
  unsigned int nd = c->n;
  if(is_dep(c)) return nd;
  else if(!is_reduced(c)) {
    unsigned int n = closure_args(c);
    while(n-- &&
          is_closure(c->arg[n]) &&
          is_dep(c->arg[n]) &&
          c->arg[n]->arg[0] == c) --nd;
  }
  return nd;
}

void mutate_update(cell_t *r, bool m) {
  traverse(r, {
      cell_t *c = clear_ptr(*p, 3);
      if(is_closure(c)) {
        if(c->tmp) {
          *p = ref(c->tmp);
          if (m) --c->n;
        } else if (!m) ref(c);
      }
    }, ARGS_IN | PTRS | ALT);

  traverse(r, {
      cell_t *c = clear_ptr(*p, 3);
      if(c && c->tmp) {
        *p = c->tmp;
      }
    }, ARGS_OUT);
}

bool mutate_sweep(cell_t *c, cell_t *r, cell_t **l, bool u, int exp) {
  r = clear_ptr(r, 3);
  if(!is_closure(r)) return false;
  if(r->tmp) return true;
  u &= !nondep_n(r);

  bool dirty = false;
  if(r == c) {
    dirty = true;
  } else {
    traverse(r, {
        dirty |= mutate_sweep(c, *p, l, u, exp);
      }, ARGS_IN | PTRS | ALT);
  }

  if(dirty) {
    if(u) {
      // if unique, rewrite pointers inplace
      mutate_update(r, true);
    } else {
      // otherwise, add to the list and defer
      cell_t *n = copy(r);
      n->n = -1;
      if(exp) n = expand_inplace(n, exp); // *** TODO optimize
      n->tmp = *l;
      r->tmp = n;
      *l = r;
    }
  }
  return dirty && !u;
}

cell_t *mutate(cell_t *c, cell_t *r, int exp) {
  cell_t *l = 0;
  make_graph_all(0);
  mutate_sweep(c, r, &l, true, exp);
  if(r->tmp) {
    ++r->tmp->n;
    --r->n;
  }
  make_graph_all(0);

  // traverse list and rewrite pointers
  cell_t *li = l;
  while(li) {
    cell_t *t = li->tmp;
    mutate_update(t, false);
    li = t->tmp;
  }
  make_graph_all(0);
  return l;
}

void clean_tmp(cell_t *l) {
  while(l) {
    cell_t *next = l->tmp;
    l->tmp = 0;
    if(l->n == -1) {
      l->n = 0;
      drop(l);
    }
    l = next;
  }
}

void _modify_new(cell_t *r, bool u) {
  cell_t *n;
  if(clear_ptr(r->tmp, 3)) return;
  if(u) {
    n = ref(r);
  } else {
    n = copy(r);
    n->tmp = (cell_t *)3;
    n->n = 0;
  }
  r->tmp = mark_ptr(n, 3);
}

/* first sweep of modify_copy */
cell_t *_modify_copy1(cell_t *c, cell_t *r, bool up) {
  if(!is_closure(r)) return 0;

  r = clear_ptr(r, 3);
  unsigned int nd = nondep_n(r);

  /* is r unique (okay to replace)? */
  bool u = up && !nd;

  if(r->tmp) {
    assert(is_marked(r->tmp, 3));
    /* already been replaced */
    return clear_ptr(r->tmp, 3);
  } else r->tmp = (cell_t *)3;
  if(c == r) _modify_new(r, u);
  traverse(r, {
      if(_modify_copy1(c, *p, u))
        _modify_new(r, u);
    }, ARGS | PTRS | ALT);
  return clear_ptr(r->tmp, 3);
}

cell_t *get_mod(cell_t *r) {
  if(!r) return 0;
  cell_t *a = r->tmp;
  if(is_marked(a, 2)) return clear_ptr(a, 3);
  else return 0;
}

/* second sweep of modify copy */
void _modify_copy2(cell_t *r) {

  /* r is modified in place */
  bool s = r == clear_ptr(r->tmp, 3);

  if(!is_closure(r)) return;
  /* alread been here */
  if(!is_marked(r->tmp, 1)) return;
  r->tmp = clear_ptr(r->tmp, 1);
  traverse(r, {
      cell_t *u = clear_ptr(*p, 3);
      cell_t *t = get_mod(u);
      if(t) {
        if(!(s && t == u)) {
          *p = ref(t);
          if(s) drop(u);
        }
        _modify_copy2(t);
      } else if(!s) ref(u);
      if((!s || t != u) && is_weak(r, *p)) {
        --(*p)->n;
      }
    }, ARGS | PTRS | ALT);
}

cell_t *mod_alt(cell_t *c, cell_t *alt, alt_set_t alt_set) {
  cell_t *n;
  if(c->alt == alt &&
     c->alt_set == alt_set) return c;
  alt_set_ref(alt_set);
  if(!c->n) {
    n = c;
    drop(c->alt);
    alt_set_drop(c->alt_set);
  } else {
    --c->n;
    n = copy(c);
    traverse_ref(n, ARGS | PTRS);
    n->n = 0;
  }
  n->alt = alt;
  n->alt_set = alt_set;
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

void drop_multi(cell_t **a, unsigned int n) {
  for(unsigned int i = 0; i < n; i++) drop(*a++);
}

void store_lazy(cell_t **cp, cell_t *c, cell_t *r) {
  if(c->n) {
    --c->n;
    closure_shrink(c, 1);
    c->func = func_id;
    c->size = 1;
    c->out = 0;
    c->arg[0] = ref(r);
    c->arg[1] = 0;
  } else closure_free(c);
  *cp = r;
}

void store_lazy_dep(cell_t *c, cell_t *d, cell_t *r, alt_set_t alt_set) {
  if(d) {
    drop(c);
    d->func = func_id;
    d->size = 1;
    d->out = 0;
    d->arg[0] = r;
    d->arg[1] = (cell_t *)alt_set;
  } else drop(r);
}

/*
type_rep_t tr_next(type_rep_t t) {
  type_rep_t p = t;
  int level = 0;
  do {
    if(!*p) return NULL;
    level += *p++ == '.' ? -1 : 1;
  } while(level > 0);
  return p;
}

type_rep_t tr_arg(type_rep_t t, unsigned int n) {
  if(!t[0] || t[0] == '.') return NULL;
  type_rep_t p = t + 1;
  while(n--) p = tr_next(p);
  return p;
}
*/

// this shouldn't reduced directly, but is called through reduce_partial from func_dep
bool func_placeholder(cell_t **cp, UNUSED type_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  unsigned int in = closure_in(c), n = closure_args(c);
  for(unsigned int i = 0; i < in; ++i) {
    if(!reduce(&c->arg[i], T_ANY)) goto fail;
    trace(c->arg[i], c, tt_force, i);
  }
  for(unsigned int i = in; i < n; ++i) {
    cell_t *d = c->arg[i];
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
  return func_placeholder(cp, t);
}

bool is_placeholder(cell_t const *c) {
  return c && clear_ptr(c->func, 3) == (void *)func_placeholder;
}

bool entangle(alt_set_t *as, cell_t *c) {
  return !as_conflict(*as, c->alt_set) &&
    (*as |= c->alt_set, true);
}
