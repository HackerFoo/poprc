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
#include "gen/byte_compile.h"
#include "gen/list.h"

// Counter of used alt ids
uint8_t alt_cnt = 0;

cell_t **rt_roots[31];
const size_t rt_roots_n = LENGTH(rt_roots);

#if INTERFACE
#define ASSERT_REF() assert(assert_ref(rt_roots, rt_roots_n))
#endif

bool insert_root(cell_t **r) {
  return set_insert((uintptr_t)r, (uintptr_t *)rt_roots, rt_roots_n);
}

bool is_root(const cell_t *c) {
  return count_root(c, rt_roots, rt_roots_n) > 0;
}

bool remove_root(cell_t **r) {
  return set_remove((uintptr_t)r, (uintptr_t *)rt_roots, rt_roots_n);
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
                type_request_t treq) {
  cell_t **ap = &c->expr.arg[n];
  bool marked = is_marked(*ap);
  *ap = clear_ptr(*ap);
  bool r = reduce(ap, treq);
  *ctx |= (*ap)->value.alt_set;
  if(marked) *ap = mark_ptr(*ap);
  split_arg(c, n);
  return r;
}

// Duplicate c to c->alt and return it
cell_t *dup_list_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t i = 0, in = list_size(c);
  assert(n < in);
  cell_t *a = copy(c);

  // ref args
  for(; i < in; ++i) {
    if(i != n) ref(a->value.ptr[i]);
  }

  a->value.ptr[n] = b;
  c->alt = a;
  return a;
}

// Lift alternates from c->value.ptr[n] to c
void split_ptr(cell_t *c, csize_t n) {
  cell_t
    *a = c->value.ptr[n],
    *p = c,
    **pa;
  if(!a || is_marked(a) || !a->alt) return;
  do {
    pa = &p->value.ptr[n];
    if(*pa == a) {
      // insert a copy with the alt arg
      p = dup_list_alt(p, n, ref((*pa)->alt))->alt;
      // mark the arg
      *pa = mark_ptr(*pa);
    } else p = p->alt;
  } while(p);
}

// Reduce then split c->arg[n]
// NOTE: c should never have a row
bool reduce_ptr(cell_t *c,
                csize_t n,
                alt_set_t *ctx,
                type_request_t treq) {
  assert(is_list(c));
  cell_t **ap = &c->value.ptr[n];
  bool marked = is_marked(*ap);
  *ap = clear_ptr(*ap);
  bool r = reduce(ap, treq);
  *ctx |= (*ap)->value.alt_set;
  if(marked) *ap = mark_ptr(*ap);
  split_ptr(c, n);
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
bool reduce(cell_t **cp, type_request_t treq) {
  cell_t *c;
  while((c = *cp)) {
    assert(is_closure(c));
    if(!closure_is_ready(c)) {
      fail(cp, treq);
      continue;
    }
    unsigned int m = measure.reduce_cnt++;
    bool success = c->func(cp, treq);
    if(success) {
      cell_t *n = *cp;
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
  cell_t *c = *cp;
  if(!c || !closure_is_ready(c)) {
    fail(cp, req_any);
  } else {
    assert(is_closure(c) &&
           closure_is_ready(c));
    measure.reduce_cnt++;
    c->func(cp, req_any);
  }
}

bool type_match(int t, cell_t const *c) {
  int tc = c->value.type.exclusive;
  return t == T_ANY || tc == T_ANY || t == tc;
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
      *d = 0; //dep(ref(c)); // these would be dangling
    }
  }
}

csize_t count_deps(cell_t *c) {
  csize_t deps = 0;
  for(csize_t i = c->size - c->expr.out; i < c->size; ++i) {
    if(c->expr.arg[i]) deps++;
  }
  return deps;
}

// add more outputs
cell_t *expand_deps(cell_t *c, csize_t s) {
  csize_t deps = count_deps(c);
  c->n -= deps;
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

  c->n += deps;

  return c;
}

cell_t *compose(list_iterator_t it, cell_t *b) {
  if(!list_has_more(it)) goto done;

  cell_t **x;
  csize_t b_in = function_in(b);
  if(b_in && (x = list_next(&it, true))) {
    b_in--;
    cell_t **left = leftmost(&b);
    b = arg_nd(*left, ref(*x), b);
    left = leftmost(&b);
    // left is now safe for arg() because arg_nd() made necessary copies
    while(b_in) {
      b_in--;
      if((x = list_next(&it, true))) break;
      arg(*left, ref(*x));
    }
  }

  // prepend b with the remainder of a ***
  int remaining_a = list_remaining_size(it, false);
  if(remaining_a) {
    cell_t **ll = left_list(&b);
    csize_t offset = list_size(*ll);
    *ll = expand(*ll, remaining_a);
    cell_t **bp = &(*ll)->value.ptr[offset];
    WHILELIST(x, it) {
      *bp++ = ref(*x);
    }
    if(it.row) (*ll)->value.type.flags |= T_ROW;
  }

done:
  return b;
}

cell_t *func(reduce_t *f, csize_t in, csize_t out) {
  assert(out > 0);
  csize_t args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->expr.out = out - 1;
  c->func = f;
  if(args) c->expr.arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, !args);
  return c;
}

/* arg is destructive to c */
void arg(cell_t *c, cell_t *a) {
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  csize_t i = closure_next_child(c);
  if(!is_data(c->expr.arg[i])) {
    c->expr.arg[0] = (cell_t *)(intptr_t)
      (i - (closure_is_ready(a) ? 1 : 0));
    c->expr.arg[i] = a;
    if(i == 0)
      closure_set_ready(c, closure_is_ready(a));
  } else {
    arg(c->expr.arg[i], a);
    if(closure_is_ready(c->expr.arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->expr.arg[0]; // decrement offset
    }
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
  if(!is_data(p->expr.arg[i])) {
    cell_t *l = mutate(&p, &r);
    if(c->tmp) c = c->tmp;
    clean_tmp(l);
    p->expr.arg[i] = a;
    if(closure_is_ready(a)) {
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
  c->value.type.flags |= T_FAIL;
  c->alt = alt;
}

void store_var(cell_t *c, int t) {
  cell_t v = {
    .func = func_value,
    .n = c->n,
    .size = 2,
    .value = {
      .alt_set = 0,
      .type = {
        .flags = T_VAR,
        .exclusive = t
      },
      .ptr = { trace_alloc(c->size) }
    }
  };
  trace_update(c, &v);
  closure_shrink(c, 1);
  *c = v;
}

void fail(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  if(!is_cell(c)) {
    *cp = NULL;
    return;
  }
  assert(!is_marked(c));
  cell_t *alt = ref(c->alt);
  if(c->n && treq.t == T_ANY) { // HACK this should be more sophisticated
    traverse(c, {
        drop(*p);
      }, ARGS_IN);
    traverse(c, {
        cell_t *d = *p;
        if(d && is_dep(d)) {
          drop(c);
          store_fail(d, d->alt);
        }
      }, ARGS_OUT);
    closure_shrink(c, 1);
    memset(&c->value, 0, sizeof(c->value));
    c->func = func_value;
    c->value.type.flags = T_FAIL;
  }
  drop(c);
  *cp = alt;
  measure.fail_cnt++;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = *cp;
  assert(!is_marked(c));
  r->func = func_value;
  trace_reduction(c, r);
  drop_multi(c->expr.arg, closure_in(c));
  csize_t size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    refcount_t n = c->n;
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    c->n = n;
    if(is_cell(r)) {
      traverse_ref(r, ALT | PTRS);
      drop(r);
    }
   } else {
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

cell_t *pushl_val(intptr_t x, cell_t *c) {
  csize_t n = val_size(c);
  c = expand(c, 1);
  c->value.integer[n] = x;
  return c;
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

/* replace references in r with corresponding tmp references */
/* m => in-place, update ref counts for replaced references */
static
void mutate_update(cell_t *r, bool m) {
  traverse(r, {
      cell_t *c = *p;
      if(is_closure(c) && c->n != PERSISTENT) {
        if(c->tmp) {
          *p = ref(c->tmp);
          if (m) --c->n;
        } else if (!m) ref(c);
      }
    }, ARGS_IN | PTRS | ALT);

  traverse(r, {
      cell_t *c = *p;
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
  if(!is_closure(r) || r->n == PERSISTENT) return false;
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

bool deps_are_unique(cell_t *c) {
  traverse(c, {
      if(*p && ~(*p)->n) return false;
    }, ARGS_OUT);
  return true;
}

/* make a path copy from the root (r) to the cell to modify (c) and store in tmps */
/* r references c. Optimization over: */
/* r' = deep_copy(r) */
/* drop(r) */
/* modify c' in r' without affecting c */
cell_t *mutate(cell_t **cp, cell_t **rp) {
  cell_t *c = *cp, *r = *rp;
  cell_t *l = NULL;

  fake_drop(r);
  if(!~c->n) {
    fake_undrop(r);
    return NULL;
  }

  add_copy_to_list(c, &l);

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
        cell_t *x = *p;
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

type_request_t req_list(type_request_t *p, int in, int out) {
  if(p) {
    in += p->in;
    out = max(0, out + p->out);
  }
  type_request_t req = {
    .t = T_LIST,
    .in = in,
    .out = out,
  };
  return req;
}

const type_request_t req_any = { .t = T_ANY };
const type_request_t req_int = { .t = T_INT };
const type_request_t req_symbol = { .t = T_SYMBOL };

type_request_t req_simple(int t) {
  type_request_t req = {
    .t = t
  };
  return req;
}
