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
#include <stdio.h>
#include "rt_types.h"
#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/eval.h"
#include "gen/print.h"
#include "gen/test.h"
#include "gen/support.h"

// Counter of used alt ids
uint8_t alt_cnt = 0;

#define RT_ROOTS_N 31
static const size_t rt_roots_n = RT_ROOTS_N;
static cell_t **rt_roots[RT_ROOTS_N];
#define ASSERT_REF() assert_ref(rt_roots, rt_roots_n)

bool insert_root(cell_t **r) {
  return set_insert((uintptr_t)r, (uintptr_t *)rt_roots, rt_roots_n);
}

bool is_root(const cell_t *c) {
  return count_root(c, rt_roots, rt_roots_n) > 0;
}

bool remove_root(cell_t **r) {
  return set_remove((uintptr_t)r, (uintptr_t *)rt_roots, rt_roots_n);
}

// Default tracing function that does nothing
void trace_noop(UNUSED cell_t *c, UNUSED cell_t *r, UNUSED trace_type_t tt, UNUSED csize_t n) {}

// Pointer to tracing function
void (*trace)(cell_t *, cell_t *, trace_type_t, csize_t) = trace_noop;

// Set the tracing function
void set_trace(void (*t)(cell_t *, cell_t *, trace_type_t, csize_t)) {
  trace = t ? t : trace_noop;
}

// Initialize run time
void rt_init() {
  alt_cnt = 0;
  memset(rt_roots, 0, sizeof(rt_roots));
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
    if(c->expr.arg[i]) {
      a->expr.arg[i] = dep(a);
      c->expr.arg[i]->alt = conc_alt(a->expr.arg[i], c->expr.arg[i]->alt);
      ++out;
    }
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
  if(!a || is_marked(a) || !a->alt) return;
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
                alt_set_t *ctx,
                type_t t) {
  cell_t **ap = &c->expr.arg[n];
  bool r = reduce(ap, t);
  *ctx |= ((cell_t *)clear_ptr(*ap))->value.alt_set;
  split_arg(c, n);
  return r;
}

// Clear the flags bits in args
void clear_flags(cell_t *c) {
  int i = 0;
  for(; i < c->size; ++i) {
    c->expr.arg[i] = clear_ptr(c->expr.arg[i]);
  }
}

// Reduce *cp with type t
bool reduce(cell_t **cp, type_t t) {
  cell_t *c;
  ASSERT_REF();
  while((c = clear_ptr(*cp))) {
    if(!closure_is_ready(c)) close_placeholders(c);
    assert(is_closure(c));
    if(!closure_is_ready(c)) {
      fail(cp, t);
      continue;
    }
    unsigned int m = measure.reduce_cnt++;
    bool success = c->func(cp, t);
    ASSERT_REF();
    if(success) {
      cell_t *n = clear_ptr(*cp);
      if(write_graph && measure.reduce_cnt > m) {
        mark_cell(n);
        make_graph_all(0);
      }
      return true;
    }
  }
  if(write_graph) {
    mark_cell(c);
    make_graph_all(0);
  }
  *cp = &fail_cell;
  return false;
}

// Perform one reduction step on *cp
void reduce_dep(cell_t **cp) {
  cell_t *c = clear_ptr(*cp);
  const type_t t = T_ANY;
  if(!closure_is_ready(c)) close_placeholders(c);
  if(!c || !closure_is_ready(c)) {
    fail(cp, t);
  } else {
    assert(is_closure(c) &&
           closure_is_ready(c));
    measure.reduce_cnt++;
    c->func(cp, t);
  }
}

bool type_match(type_t t, cell_t const *c) {
  type_t ta = t & T_EXCLUSIVE;
  type_t tb = c->value.type & T_EXCLUSIVE;
  return ta == T_ANY || tb == T_ANY || ta == tb;
}

cell_t *append(cell_t *a, cell_t *b) {
  csize_t n = list_size(b);
  csize_t n_a = list_size(a);
  cell_t *e = expand(a, n);
  while(n--) e->value.ptr[n + n_a] = b->value.ptr[n];
  return e;
}

// TODO clean up these expand functions

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
    drop(c);
    return new;
  }
}

void update_deps(cell_t *c) {
  for(csize_t i = c->size - c->expr.out; i < c->size; ++i) {
    cell_t *d = c->expr.arg[i];
    if(d) {
      assert(is_dep(d));
      d->expr.arg[0] = c;
    }
  }
}

void new_deps(cell_t *c) {
  for(csize_t i = c->size - c->expr.out; i < c->size; ++i) {
    cell_t **d = &c->expr.arg[i];
    if(*d) {
      assert(is_dep(*d));
      *d = dep(ref(c));
    }
  }
}

// add more inputs
cell_t *expand_args(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  csize_t args = closure_args(c);
  c = expand(c, s);

  // shift and update deps
  memmove(&c->expr.arg[s], &c->expr.arg[0], args * sizeof(cell_t *));
  memset(c->expr.arg, 0, s * sizeof(cell_t *));
  if(n) {
    new_deps(c);
  } else {
    update_deps(c);
  }

  return c;
}

// destructive version
cell_t *expand_args_inplace(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  c->n = 0;
  c = expand_args(c, s);
  c->n = n;
  return c;
}

// add more outputs
cell_t *expand_deps(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  csize_t in = closure_in(c);
  c = expand(c, s);

  // shift and update deps
  memmove(&c->expr.arg[in+s], &c->expr.arg[in], c->expr.out * sizeof(cell_t *));
  memset(&c->expr.arg[in], 0, s * sizeof(cell_t *));
  c->expr.out += s;
  if(n) {
    new_deps(c);
  } else {
    update_deps(c);
  }

  return c;
}

// destructive version
cell_t *expand_deps_inplace(cell_t *c, csize_t s) {
  refcount_t n = c->n;
  c->n = 0;
  c = expand_deps(c, s);
  c->n = n;
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

// non-destructive (_nd) compose
cell_t *compose_nd(cell_t *a, cell_t *b) {
  csize_t n = list_size(b);
  csize_t n_a = list_size(a);
  csize_t i = 0, new = 0;
  if(n && n_a) {
    cell_t *l;
    while(!closure_is_ready(l = b->value.ptr[n-1]) && i < n_a) {
      cell_t *x = a->value.ptr[i];
      if(is_placeholder(x)) {
        if(is_placeholder(l)) {
          assert_throw(false, "composing placeholders doesn't work right now.");
          b->value.ptr[n-1] = compose_placeholders(x, l);
          ++i;
          break;
        }

        // only expand inplace after copying if necessary the first time
        if(i >= new) {
          new = i + 1;
          drop(x);
          x = func(func_placeholder, 0, 2);
        } else {
          x = expand_deps_inplace(x, 1);
        }
        a->value.ptr[i] = x;
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
    c = expand_args_inplace(c, 1);
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

void update_ready(cell_t *c, cell_t *a) {
  cell_t *p = c, *q = 0;
  cell_t *l = 0;

  // build stack
  while(p != a) {
    csize_t i = closure_next_child(p);
    if(i == 0) {
      // last arg, push to stack
      CONS(tmp, &l, p);
    } else {
      // not last arg, clear the stack
      clean_tmp(l);
      l = 0;
      q = p;
    }
    p = p->expr.arg[i];
  }

  // set ready
  p = l;
  while(p) {
    cell_t *n = p->tmp;
    closure_set_ready(p, true);
    p->tmp = 0;
    p = n;
  }

  if(q) --*(intptr_t *)&q->expr.arg[0]; // decrement offset
}

// non-destructive (_nd) version of arg
cell_t *arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  assert(is_closure(a));
  cell_t *p = c;
loop:
  assert(!closure_is_ready(p));
  csize_t i = closure_next_child(p);
  // *** shift args if placeholder
  if(is_placeholder(p) &&
     (closure_in(p) == 0 ||
      closure_is_ready(p->expr.arg[0]))) {
    cell_t *l = mutate(&p, &r, 1);
    if(c->tmp) c = c->tmp;
    clean_tmp(l);
    assert(!p->expr.arg[0]);
    p->expr.arg[0] = a;
    r->value.ptr[list_size(r)-1] = p; // *** why is this done here?
  } else if(!is_data(p->expr.arg[i])) {
    cell_t *l = mutate(&p, &r, 0);
    if(c->tmp) c = c->tmp;
    clean_tmp(l);
    p->expr.arg[i] = a;
    if(!is_placeholder(p) &&
       closure_is_ready(a)) {
        update_ready(c, a);
    }
  } else {
    p = p->expr.arg[i];
    goto loop;
  }
  return r;
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
  c->value.alt_set = 0;
  c->value.type = T_VAR | t;
  c->size = 1;
}

void fail(cell_t **cp, type_t t) {
  cell_t *c = clear_ptr(*cp);
  if(!is_cell(c)) {
    *cp = NULL;
    return;
  }
  assert(!is_marked(c));
  trace(c, c->alt, tt_fail, 0);
  cell_t *alt = ref(c->alt);
  if(c->n && t == T_ANY) { // HACK this should be more sophisticated
    traverse(c, {
        cell_t *x = clear_ptr(*p);
        drop(x);
      }, ARGS_IN);
    traverse(c, {
        cell_t *d = clear_ptr(*p);
        if(d) {
          drop(c);
          store_fail(d, d->alt);
        }
      }, ARGS_OUT);
    closure_shrink(c, 1);
    memset(&c->value, 0, sizeof(c->value));
    c->func = func_value;
    c->value.type = T_FAIL;
  }
  drop(c);
  *cp = alt;
  measure.fail_cnt++;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  r->func = func_value;
  trace(c, r, tt_reduction, 0);
  drop_multi(c->expr.arg, closure_in(c));
  csize_t size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    refcount_t n = c->n;
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    if(is_cell(r)) {
      traverse_ref(r, ALT | PTRS);
      drop(r);
    }
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

bool is_weak(cell_t const *p, cell_t const *c) {
  return c && is_dep(c) && (!c->expr.arg[0] || c->expr.arg[0] == p);
}

cell_t *conc_alt(cell_t *a, cell_t *b) {
  if(!a) return b;
  if(!b) return a;
  cell_t *p = a, *r;
  while((r = p->alt)) p = r;
  p->alt = b;
  return a;
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

// non-destructive (_nd) pushl
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

/* replace references in r with corresponding tmp references */
/* m => in-place, update ref counts for replaced references */
static
void mutate_update(cell_t *r, bool m) {
  traverse(r, {
      cell_t *c = clear_ptr(*p);
      if(is_closure(c) && c->n != PERSISTENT) {
        if(c->tmp) {
          *p = ref(c->tmp);
          if (m) --c->n;
        } else if (!m) ref(c);
      }
    }, ARGS_IN | PTRS | ALT);

  traverse(r, {
      cell_t *c = clear_ptr(*p);
      if(c && c->n != PERSISTENT && c->tmp) {
        // if(m) fix deps?
        *p = c->tmp;
      }
    }, ARGS_OUT);
}

static
cell_t *add_to_list(cell_t *c, cell_t *nc, cell_t **l) {
  nc->n = -1;
  CONS(tmp, l, nc);
  CONS(tmp, l, c);
  assert(check_tmp_loop(*l));
  return nc;
}

static
cell_t *add_copy_to_list(cell_t *c, cell_t **l) {
  return add_to_list(c, copy(c), l);
}

/* traverse r and make copies to tmp */
/* u => subtree is unique, exp => to be expanded */
static
bool mutate_sweep(cell_t *r, cell_t **l) {
  r = clear_ptr(r);
  if(!is_closure(r)) return false;
  if(r->tmp) return true;

  bool unique = !~r->n; // only referenced by the root
  bool dirty = false; // references c

  // prevent looping
  r->tmp = r;
  traverse(r, {
      dirty |= mutate_sweep(*p, l);
    }, ARGS | PTRS | ALT);
  r->tmp = 0;

  if(!dirty) return false;

  if(unique) {
    // rewrite pointers inplace
    mutate_update(r, true);
    return false;
  } else {
    add_copy_to_list(r, l);
    return true;
  }
}

/* make a path copy from the root (r) to the cell to modify (c) and store in tmps */
/* r references c. Optimization over: */
/* r' = deep_copy(r) */
/* drop(r) */
/* modify c' in r' without affecting c */
cell_t *mutate(cell_t **cp, cell_t **rp, int exp) {
  cell_t *c = *cp, *r = *rp;
  cell_t *l = NULL;

  fake_drop(r);
  if(!~c->n) {
    fake_undrop(r);
    if(exp) {
      *cp = expand_args_inplace(c, exp);
    }
    return NULL;
  }

  // expand c if needed
  if(exp) {
    cell_t *nc = copy_expand(c, exp);
    add_to_list(c, nc, &l);
  } else {
    add_copy_to_list(c, &l);
  }

  mutate_sweep(r, &l);
  fake_undrop(r);

  if(r->tmp) {
    ++r->tmp->n;
    --r->n;
  }

  // traverse list and rewrite pointers
  cell_t *li = l;
  assert(check_tmp_loop(li));
  while(li) {
    cell_t *t = li->tmp;
    mutate_update(t, false);
    li = t->tmp;
  }
  assert(check_deps(r));
  if(c->tmp) *cp = c->tmp;
  if(r->tmp) *rp = r->tmp;
  return l;
}

bool check_deps(cell_t *c) {
  bool ret = true;
  c = clear_ptr(c);
  if(c && is_cell(c)) {
    traverse(c, {
        cell_t *x = clear_ptr(*p);
        if(x && x->expr.arg[0] != c) {
          printf("bad dep %d -> %d, should be %d\n",
                 (int)(x - cells),
                 (int)(x->expr.arg[0] - cells),
                 (int)(c - cells));
          ret = false;
        }
      }, ARGS_OUT);
    traverse(c, {
        if(!check_deps(*p)) ret = false;
      }, ARGS_IN | ALT | PTRS);
  }
  return ret;
}

bool check_tmp_loop(cell_t *c) {
  if(!c) return true;
  cell_t *tortoise = c, *hare = c;
  size_t cnt = 0;
  do {
    cnt += 2;
    tortoise = tortoise->tmp;
    hare = hare->tmp ? hare->tmp->tmp : NULL;
    if(!(tortoise && hare)) return true;
  } while(tortoise != hare);

  while(cnt--) {
    printf("%d -> ", (int)(c - cells));
    c = c->tmp;
  }
  printf("...\n");
  return false;
}

// execute deferred drops and zero tmps
void clean_tmp(cell_t *l) {
  while(l) {
    cell_t *next = l->tmp;
    l->tmp = 0;
    assert(~l->n);
    l = next;
  }
}

cell_t *mod_alt(cell_t *c, cell_t *alt, alt_set_t alt_set) {
  assert(is_value(c));
  cell_t *n;
  if(c->alt == alt &&
     c->value.alt_set == alt_set) return c;
  if(!c->n) {
    n = c;
    drop(c->alt);
  } else {
    if(c->n != PERSISTENT) --c->n;
    n = copy(c);
    traverse_ref(n, ARGS | PTRS);
  }
  n->alt = alt;
  n->value.alt_set = alt_set;
  return n;
}

void store_lazy(cell_t **cp, cell_t *c, cell_t *r, alt_set_t alt_set) {
  if(c->n || alt_set) {
    closure_shrink(c, 1);
    c->func = func_id;
    c->size = 1;
    c->expr.out = 0;
    c->expr.arg[0] = r;
    c->alt = 0;
    if(c->n) ref(r);
    c->expr.alt_set = alt_set;
  }

  if(!alt_set) {
    if(c->n == 0) closure_free(c);
    else --c->n;
    *cp = r;
  }
}

void store_lazy_dep(cell_t *d, cell_t *r, alt_set_t alt_set) {
  if(d) {
    drop(d->expr.arg[0]);
    d->func = func_id;
    d->size = 1;
    d->expr.out = 0;
    d->expr.arg[0] = r;
    d->expr.alt_set = alt_set;
  } else drop(r);
}

uint8_t new_alt_id(unsigned int n) {
  uint8_t r = alt_cnt;
  alt_cnt += n;
  return r;
}
