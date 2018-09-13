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
#include "rt_types.h"

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "eval.h"
#include "print.h"
#include "trace.h"
#include "list.h"
#include "user_func.h"
#include "tags.h"

// Counter of used alt ids
uint8_t alt_cnt = 0;

cell_t **rt_roots[31];
const size_t rt_roots_n = LENGTH(rt_roots);

static cell_t *watched_cells[4] = {0};
static bool watch_enabled = false;

#if INTERFACE
#define ASSERT_REF() assert_error(assert_ref(rt_roots, rt_roots_n))
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

int set_watch(cell_t *c) {
  FOREACH(i, watched_cells) {
    if(!watched_cells[i]) {
      watch_enabled = true;
      watched_cells[i] = c;
      return i + 1;
    }
  }
  return 0;
}

int get_watch(cell_t *c) {
  if(!watch_enabled) return 0;
  FOREACH(i, watched_cells) {
    if(watched_cells[i] == c) {
      return i + 1;
    }
  }
  return 0;
}

// Initialize run time
void rt_init() {
  alt_cnt = 0;
  memset(rt_roots, 0, sizeof(rt_roots));
  clear_ptr_tags();
}

// Duplicate c to c->alt and return it
cell_t *dup_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t
    in = closure_in(c),
    out = closure_out(c),
    args = closure_args(c);
  assert_error(n < in);
  cell_t *a = copy(c);

  // ref args
  COUNTUP(i, in) {
    if(i != n) ref(a->expr.arg[i]);
  }

  // update deps
  RANGEUP(i, args - out, args) {
    if(c->expr.arg[i]) {
      a->expr.arg[i] = dep(ref(a));
      c->expr.arg[i]->alt = conc_alt(a->expr.arg[i], c->expr.arg[i]->alt);
    }
  }

  a->expr.arg[n] = b;
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
      p = dup_alt(p, n, ref(a->alt))->alt;
      // mark the arg
      *pa = mark_ptr(a);
    } else p = p->alt;
  } while(p);
}

void split_expr(cell_t *c) {
  csize_t in = closure_in(c);
  COUNTUP(i, in) {
    split_arg(c, i);
  }
  TRAVERSE(c, in) {
    *p = clear_ptr(*p);
  }
}

// Reduce then split c->arg[n]
response reduce_arg(cell_t *c,
                csize_t n,
                alt_set_t *ctx,
                type_request_t treq) {
  cell_t **ap = &c->expr.arg[n];
  response r = reduce(ap, treq);
  cell_t *a = clear_ptr(*ap);
  if(r == SUCCESS) *ctx |= a->value.alt_set;
  if(r < DELAY) split_arg(c, n);
  return r;
}

// Duplicate c to c->alt and return it
cell_t *dup_list_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t in = list_size(c);
  assert_error(n < in);
  cell_t *a = copy(c);

  // ref args
  COUNTUP(i, in) {
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
response reduce_ptr(cell_t *c,
                    csize_t n,
                    alt_set_t *ctx,
                    type_request_t treq) {
  assert_error(is_list(c));
  cell_t **ap = &c->value.ptr[n];
  response r = reduce(ap, treq);
  cell_t *a = clear_ptr(*ap);
  if(r == SUCCESS) *ctx |= a->value.alt_set;
  if(r < DELAY) split_ptr(c, n);
  return r;
}

// Clear the flags bits in args
void clear_flags(cell_t *c) {
  TRAVERSE(c, in) {
    *p = clear_ptr(*p);
  }
}

static reduce_t *_func[] = {
#define OP__ITEM(name) func_##name,
#include "op_list.h"
#undef OP__ITEM
};

reduce_t *op_func(op op) {
  assert_error(op > 0 && op < OP_COUNT);
  return _func[op - 1];
}

cell_t *fill_incomplete(cell_t *c) {
  if(!closure_is_ready(c)) {
    LOG("fill_incomplete: closure not ready %C", c);
    do {
      c = arg_nd(c, &fail_cell, c);
    } while(!closure_is_ready(c));
  }
  return c;
}

// Reduce *cp with type t
response reduce(cell_t **cp, type_request_t treq) {
  bool marked = is_marked(*cp);
  *cp = clear_ptr(*cp);
  cell_t *c = *cp;
  while(c) {
    assert_error(is_closure(c));
    c = *cp = fill_incomplete(c);
    stats.reduce_cnt++;
    op op = c->op;
    response r = op_func(op)(cp, treq);

    // prevent infinite loops when debugging
    assert_counter(LENGTH(cells));

    LOG_WHEN(!*cp, MARK("FAIL") ": %O %C @abort", op, c);
    c = *cp;
    if(r <= DELAY) {
      if(marked) *cp = mark_ptr(c); // *** is the right pointer being marked?
      return r;
    }
  }
  *cp = &fail_cell;
  return FAIL;
}

// Perform one reduction step on *cp
response reduce_one(cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  if(!c) {
    LOG("reduce_one: null closure %C", c);
    return abort_op(FAIL, cp, treq);
  } else {
    c = *cp = fill_incomplete(c);
    assert_error(is_closure(c) &&
           closure_is_ready(c));
    stats.reduce_cnt++;
    return op_func(c->op)(cp, treq);
  }
}

bool type_match(int t, cell_t const *c) {
  type_t tc = c->value.type;
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
    TRAVERSE_REF(new, alt, in, ptrs);
    new->size = n + s;
    drop(c);
    return new;
  }
}

void update_deps(cell_t *c) {
  TRAVERSE(c, out) {
    cell_t *d = *p;
    if(d) {
      assert_error(is_dep(d));
      d->expr.arg[0] = c;
    }
  }
}

csize_t count_deps(cell_t *c) {
  csize_t deps = 0;
  TRAVERSE(c, out) {
    if(*p) deps++;
  }
  return deps;
}

// add more outputs
cell_t *expand_deps(cell_t *c, csize_t s) {
  csize_t deps = count_deps(c);
  c->n -= deps;
  assert_error(!c->n);
  csize_t in = closure_in(c);
  c = expand(c, s);

  // shift and update deps
  memmove(&c->expr.arg[in+s], &c->expr.arg[in], c->expr.out * sizeof(cell_t *));
  memset(&c->expr.arg[in], 0, s * sizeof(cell_t *));
  c->expr.out += s;
  update_deps(c);

  c->n += deps;

  return c;
}

cell_t *compose(list_iterator_t it, cell_t *b) {
  cell_t **x;
  csize_t b_in = function_in(b);
  if(b_in && (x = list_next(&it, true))) {
    b_in--;
    cell_t **left = needs_arg(b);
    b = arg_nd(*left, ref(*x), b);
    // left is now safe for arg() because arg_nd() made necessary copies ***
    while(b_in) {
      b_in--;
      if(!(x = list_next(&it, true))) break;
      left = needs_arg(b); // ***
      arg(*left, ref(*x));
    }
  }

  // prepend b with the remainder of a
  int remaining_a = list_remaining_size(it, true);
  if(remaining_a) {
    cell_t **ll = left_list(&b);
    csize_t offset = list_size(*ll);
    *ll = expand(*ll, remaining_a);
    cell_t **bp = &(*ll)->value.ptr[offset];
    WHILELIST(x, it, true) {
      *bp++ = ref(*x);
    }
    if(it.row) FLAG_SET((*ll)->value, VALUE_ROW);
  }

  return b;
}

cell_t *func(op op, csize_t in, csize_t out) {
  assert_error(out > 0);
  csize_t args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->expr.out = out - 1;
  c->expr.flags = 0;
  c->op = op;
  if(args) c->expr.arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, !args);
  return c;
}

/* arg is destructive to c */
void arg(cell_t *c, cell_t *a) {
  assert_error(is_closure(c) && is_closure(a));
  assert_error(!closure_is_ready(c));
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
      else --c->expr.idx[0]; // decrement offset
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

  if(q) --q->expr.idx[0]; // decrement offset
}

// non-destructive (_nd) version of arg
cell_t *arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  assert_error(is_closure(a));
  cell_t *p = c;
loop:
  assert_error(!closure_is_ready(p));
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

void store_fail(cell_t *c, cell_t *alt) {
  closure_shrink(c, 1);
  memset(&c->value, 0, sizeof(c->value));
  c->op = OP_value;
  FLAG_SET(c->value, VALUE_FAIL);
  c->alt = alt;
}

void store_dep(cell_t *c, cell_t *tc, csize_t pos, type_t t, alt_set_t alt_set) {
  cell_t v = {
    .op = OP_value,
    .n = c->n,
    .size = 2,
    .pos = pos,
    .alt = c->alt,
    .value = {
      .alt_set = alt_set,
      .type = t,
      .flags = VALUE_VAR | VALUE_DEP,
      .var = tc
    }
  };
  if(c->op) closure_shrink(c, 1);
  *c = v;
}

response abort_op(response rsp, cell_t **cp, type_request_t treq) {
  cell_t *c = *cp;
  if(rsp == FAIL) {
    if(!is_cell(c)) {
      *cp = NULL;
      return rsp;
    }
    assert_error(!is_marked(c));
    cell_t *alt = ref(c->alt);
    if(c->n && treq.t == T_ANY && !treq.expected) { // HACK this should be more sophisticated
      TRAVERSE(c, in) {
        drop(*p);
      }
      TRAVERSE(c, out) {
        cell_t *d = *p;
        if(d && is_dep(d)) {
          drop(c);
          store_fail(d, d->alt);
        }
      }
      closure_shrink(c, 1);
      memset(&c->value, 0, sizeof(c->value));
      c->op = OP_value;
      c->value.flags = VALUE_FAIL;
    }
    drop(c);
    *cp = alt;
    stats.fail_cnt++;
  }
  return rsp;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = *cp;
  assert_error(!is_marked(c));
  r->op = OP_value;
  trace_reduction(c, r);
  drop_multi(c->expr.arg, closure_in(c));
  csize_t size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    refcount_t n = c->n;
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    c->n = n;
    if(is_cell(r)) {
      TRAVERSE_REF(r, alt, ptrs);
      drop(r);
    }
   } else {
    store_lazy(cp, r, 0);
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
      assert_error(!p->tmp);
      i += closure_cells(p);
    } else ++i;
  }
}

/* replace references in r with corresponding tmp references */
/* m => in-place, update ref counts for replaced references */
static
void mutate_update(cell_t *r, bool m) {
  TRAVERSE(r, alt, in, ptrs) {
    cell_t *c = *p;
    if(is_closure(c) && c->n != PERSISTENT) {
      if(c->tmp) {
        *p = ref(c->tmp);
        if (m) --c->n;
      } else if (!m) ref(c);
    }
  }

  TRAVERSE(r, out) {
    cell_t *c = *p;
    if(c && c->n != PERSISTENT && c->tmp) {
      // if(m) fix deps?
      *p = c->tmp;
    }
  }
}

static
cell_t *add_to_list(cell_t *c, cell_t *nc, cell_t **l) {
  nc->n = -1;
  CONS(tmp, l, nc);
  CONS(tmp, l, c);
  assert_error(check_tmp_loop(*l));
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
  TRAVERSE(r, alt, in, ptrs) {
    dirty |= mutate_sweep(*p, l);
  }
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
  TRAVERSE(c, out) {
    if(*p && ~(*p)->n) return false;
  }
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
  assert_error(check_tmp_loop(li));
  while(li) {
    cell_t *t = li->tmp;
    mutate_update(t, false);
    li = t->tmp;
  }
  assert_error(check_deps(r));
  if(c->tmp) *cp = c->tmp;
  if(r->tmp) *rp = r->tmp;
  return l;
}

bool check_deps(cell_t *c) {
  bool ret = true;
  c = clear_ptr(c);
  if(c && is_cell(c)) {
    TRAVERSE(c, out) {
      cell_t *x = *p;
      if(x && x->expr.arg[0] != c) {
        printf("bad dep %d -> %d, should be %d\n",
               (int)(x - cells),
               (int)(x->expr.arg[0] - cells),
               (int)(c - cells));
        ret = false;
      }
    }
    TRAVERSE(c, alt, in, ptrs) {
      if(!check_deps(*p)) ret = false;
    }
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
    assert_error(~l->n);
    l = next;
  }
}

cell_t *mod_alt(cell_t *c, cell_t *alt, alt_set_t alt_set) {
  assert_error(is_value(c));
  if(c->alt != alt ||
     c->value.alt_set != alt_set) {
    unique(&c);
    drop(c->alt);
  }
  c->alt = alt;
  c->value.alt_set = alt_set;
  return c;
}

void store_lazy(cell_t **cp, cell_t *r, alt_set_t alt_set) {
  cell_t *c = *cp;
  if(c->n || alt_set || c->pos) {
    closure_shrink(c, 1); // *** does not drop arguments first
    c->op = OP_id;
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

void store_lazy_and_update_deps(cell_t **cp, cell_t *r, alt_set_t alt_set) {
  cell_t *c = *cp;
  refcount_t n = 0;
  csize_t out_n = closure_out(c);
  assert_error(closure_out(r) == out_n);
  if(out_n) {
    cell_t
      **c_out = &c->expr.arg[closure_args(c) - out_n],
      **r_out = &r->expr.arg[closure_args(r) - out_n];
    COUNTUP(i, out_n) {
      cell_t *d = c_out[i];
      if(d) {
        d->expr.arg[0] = r;
        r_out[i] = d;
        n++;
      }
    }

    // update ref counts
    refn(r, n);
    assert_error(c->n >= n);
    c->n -= n;
  }
  store_lazy(cp, r, alt_set);
}

void store_lazy_dep(cell_t *d, cell_t *r, alt_set_t alt_set) {
  if(d) {
    drop(d->expr.arg[0]);
    d->op = OP_id;
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

#if INTERFACE
#define REQ_INHERIT .priority = treq.priority, .delay_assert = treq.delay_assert
#define REQ(type, ...) CONCAT(REQ_, type)(__VA_ARGS__)
#define REQ_list(_in, _out) \
  ((type_request_t) { .t = T_LIST, .in = _in, .out = _out, REQ_INHERIT})
#define REQ_t_1(_t)                                                     \
  ((type_request_t) { .t = _t, REQ_INHERIT, .expected = false })
#define REQ_t_2(_t, _expected_val)                                          \
  ((type_request_t) { .t = _t, REQ_INHERIT, .expected = true, .expected_value = _expected_val })
#define REQ_t_3(_t, _expected, _expected_val)                            \
  ((type_request_t) { .t = _t, REQ_INHERIT, .expected = _expected, .expected_value = _expected_val })
#define REQ_t(...) DISPATCH(REQ_t, __VA_ARGS__)
#define REQ_any(...) REQ_t(T_ANY, ##__VA_ARGS__)
#define REQ_int(...) REQ_t(T_INT, ##__VA_ARGS__)
#define REQ_float(...) REQ_t(T_FLOAT, ##__VA_ARGS__)
#define REQ_symbol(...) REQ_t(T_SYMBOL, ##__VA_ARGS__)
#define REQ_return() \
  ((type_request_t) { .t = T_RETURN })
#define REQ_INV(invert) treq.expected, (treq.expected ? invert(treq.expected_value) : 0)
#endif

// default 'treq' for REQ(...) to inherit
const type_request_t treq = { .t = T_ANY };

type_request_t req_pos(type_request_t treq, uint8_t pos) {
  treq.pos = pos;
  return treq;
}

bool check_type(uint8_t requested, uint8_t expected) {
  if(requested != T_ANY &&
     requested != expected) {
    LOG("check_type: requested %d, but expected %d", requested, expected);
    return false;
  }
  return true;
}

// assert on overlapping alts
// c->alt->...->alt = a
void assert_alt(cell_t *c, cell_t *a) {
#ifdef NDEBUG
  (void)c;
  (void)a;
#else
  alt_set_t alt_set = a->value.alt_set;
  FOLLOW(p, c, alt) {
    if(p == a) break;
    assert_error(as_conflict(p->value.alt_set | alt_set),
                 "overlapping alts %C %C @exec_split", p, a);
  }
#endif
}

response fail_if(bool x) {
  return x ? FAIL : SUCCESS;
}

cell_t *build01(op op) {
  cell_t *c = closure_alloc(0);
  c->op = op;
  return c;
}

cell_t *build11(op op, cell_t *i0) {
  cell_t *c = closure_alloc(1);
  c->op = op;
  c->expr.arg[0] = i0;
  return c;
}

cell_t *build21(op op, cell_t *i0, cell_t *i1) {
  cell_t *c = closure_alloc(2);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = i1;
  return c;
}

cell_t *build12(op op, cell_t *i0, cell_t **o1) {
  cell_t *c = closure_alloc(2);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = *o1 = dep(ref(c));
  return c;
}

cell_t *build22(op op, cell_t *i0, cell_t *i1, cell_t **o1) {
  cell_t *c = closure_alloc(3);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = i1;
  c->expr.arg[2] = *o1 = dep(ref(c));
  return c;
}
