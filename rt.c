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
#include <unistd.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "io.h"
#include "primitive_io.h"
#include "special.h"
#include "eval.h"
#include "print.h"
#include "trace.h"
#include "list.h"
#include "user_func.h"
#include "tags.h"

#if INTERFACE
#define MAX_ALTS 256
#endif

// Counter of used alt ids
uint8_t alt_cnt = 0;

cell_t **rt_roots[257];
const size_t rt_roots_n = LENGTH(rt_roots);

static cell_t *watched_cells[4] = {0};
static op watched_op = OP_null;
bool watch_enabled = false;

#if INTERFACE
#define ASSERT_REF() if(ctx->priority > PRIORITY_SIMPLIFY) assert_error(assert_ref(rt_roots, rt_roots_n))
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

void set_watched_op(op op) {
  watch_enabled = op != OP_null;
  watched_op = op;
}

int get_watch(cell_t *c) {
  if(!watch_enabled) return 0;
  FOREACH(i, watched_cells) {
    if(watched_cells[i] == c) {
      return i + 1;
    }
  }
  if(watched_op && c->op == watched_op) {
    return -1;
  }
  return 0;
}

// Initialize run time
void rt_init() {
  alt_cnt = 0;
  memset(rt_roots, 0, sizeof(rt_roots));
  clear_ptr_tags();
  reset_counters();
}

// Duplicate c to c->alt and return it
void dup_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t
    in = closure_in(c),
    out = closure_out(c),
    args = closure_args(c);
  assert_error(n < in);
  cell_t *a = copy(c);
  c->alt = a;

  // ref args
  COUNTUP(i, in) {
    if(i != n) ref(a->expr.arg[i]);
  }

  // update deps
  RANGEUP(i, args - out, args) {
    cell_t
      **dp = &c->expr.arg[i],
      *d = *dp;
    if(d) {
      // convert d to id
      *dp = copy(d);
      d->op = OP_id;
      d->expr.arg[0] = *dp;
      d->expr.alt_set = 0;

      // create a dep for `a`
      cell_t *da = dep(ref(a));
      a->expr.arg[i] = da;

      // make it the alt for d
      d->alt = da;
    }
  }

  a->expr.arg[n] = b;
}

cell_t *idify(cell_t *c) {
  cell_t *n = copy(c);
  n->alt = NULL;
  closure_shrink(c, 1);
  c->op = OP_id;
  c->size = 1;
  c->expr.flags = 0;
  c->expr.out = 0;
  c->expr.alt_set = 0;
  c->expr.arg[0] = ref(n);
  drop(c);
  return n;
}

// make *cp refer to an individual without breaking anything
cell_t *drop_alt(cell_t *c) {
  assert_error(!is_dep(c));
  if(!c->n) {
    drop(c->alt);
    c->alt = NULL;
    return c;
  } else if(is_value(c)) {
    cell_t *n = copy(c);
    n->alt = NULL;
    if(is_list(n)) {
      TRAVERSE_REF(n, ptrs);
    }
    drop(c);
    return n;
  } else {
    cell_t *n = idify(c);
    TRAVERSE(n, out) {
      cell_t *d = *p;
      if(d) {
        d->expr.arg[0] = ref(n);
        drop(c);
      }
    }
    return n;
  }
}

void drop_failed(cell_t **p) {
  cell_t **s = p;
  while(*p && is_fail(*p)) p = &(*p)->alt;
  if(*p != *s) {
    ref(*p);
    drop(*s);
    *s = *p;
  }
}

// Lift alternates from c->arg[n] to c
void split_arg(cell_t *c, csize_t n) {
  cell_t *a = c->expr.arg[n];
  drop_failed(&a->alt);
  if(!a || !a->alt) return;
  refcount_t an = 0;
  FOLLOW(p, c, alt) an += (p->expr.arg[n] == a);
  cell_t *alt = refn(a->alt, an);
  dropn(a, an-1);
  cell_t *a1 = refn(drop_alt(a), an-1);
  FOLLOW(p, c, alt) {
    if(p->expr.arg[n] == a) {
      // insert a copy with the alt arg
      dup_alt(p, n, alt);
      p->expr.arg[n] = a1;
      p = p->alt;
    }
  }
}

// Reduce then split c->arg[n]
response reduce_arg(cell_t *c,
                csize_t n,
                context_t *ctx) {
  cell_t **ap = &c->expr.arg[n];
  response r = reduce(ap, ctx);
  cell_t *a = *ap;
  if(r == SUCCESS) ctx->up->alt_set |= a->value.alt_set;
  if(r <= DELAY) split_arg(c, n);
  return r;
}

// Duplicate c to c->alt and return it
void dup_list_alt(cell_t *c, csize_t n, cell_t *b) {
  csize_t in = list_size(c);
  assert_error(n < in);
  cell_t *a = copy(c);
  a->priority = 0;
  c->alt = a;

  // ref args
  COUNTUP(i, in) {
    if(i != n) ref(a->value.ptr[i]);
  }

  a->value.ptr[n] = b;
}

// Lift alternates from c->value.ptr[n] to c
void split_ptr(cell_t *c, csize_t n) {
  cell_t *a = c->value.ptr[n];
  if(!a || !a->alt) return;
  refcount_t an = 0;
  FOLLOW(p, c, alt) an += (p->value.ptr[n] == a);
  cell_t *alt = refn(a->alt, an);
  dropn(a, an-1);
  cell_t *a1 = refn(drop_alt(a), an-1);
  FOLLOW(p, c, alt) {
    if(p->value.ptr[n] == a) {
      // insert a copy with the alt arg
      dup_list_alt(p, n, alt);
      p->value.ptr[n] = a1;
      p = p->alt;
    }
  }
}

// Reduce then split c->arg[n]
// NOTE: c should never have a row
response reduce_ptr(cell_t *c,
                    csize_t n,
                    context_t *ctx) {
  assert_error(is_list(c));
  cell_t **ap = &c->value.ptr[n];
  response r = reduce(ap, ctx);
  cell_t *a = *ap;
  if(r == SUCCESS) ctx->up->alt_set |= a->value.alt_set;
  if(r <= DELAY) split_ptr(c, n);
  return r;
}

static
response op_call(op op, cell_t **cp, context_t *ctx) {
  switch(op) {
#define OP__ITEM(file, line, name)                      \
    case OP_##name: return func_##name(cp, ctx);
#include "op_list.h"
#undef OP__ITEM
    default:
      assert_error(false, "unknown op %d", op);
      return FAIL;
  }
}

cell_t *fill_incomplete(cell_t *c) {
  if(!closure_is_ready(c)) {
    const char *module_name, *word_name;
    get_name(c, &module_name, &word_name);
    LOG("fill_incomplete: closure not ready %C (%s.%s)", c, module_name, word_name);
    do {
      c = arg_nd(c, &fail_cell, c);
    } while(!closure_is_ready(c));
  }
  return c;
}

bool is_delayed(const cell_t *c) {
  return !is_value(c) && FLAG(*c, expr, DELAYED);
}

// Reduce *cp with type t
response reduce(cell_t **cp, context_t *ctx) {
  cell_t *c = *cp;
  const char *module_name, *word_name;
  get_name(c, &module_name, &word_name); // debug

  while(c) {
    assert_error(is_closure(c));
    c = *cp = fill_incomplete(c);
    stats.reduce_cnt++;
    op op = c->op;
    response r = op_call(op, cp, ctx);

    // prevent infinite loops when debugging
    assert_counter(LENGTH(cells));

    LOG_WHEN(!*cp, MARK("FAIL") ": %O %C (%s.%s) @abort", op, c, module_name, word_name);
    c = *cp;
    if(r <= DELAY || (r == RETRY && ctx->retry)) {
      ctx->retry = false;
      return r;
    }
  }

  *cp = &fail_cell;
  return FAIL;
}

#if INTERFACE
#define force(cp) reduce((cp), &CTX(any))
#endif

response simplify(cell_t **cp) {
  CONTEXT("simplify %C", *cp);
  return reduce(cp, WITH(&CTX(any), priority, PRIORITY_SIMPLIFY));
}

// Perform one reduction step on *cp
response reduce_one(cell_t **cp, context_t *ctx) {
  cell_t *c = *cp;
  if(!c) {
    LOG("reduce_one: null closure %C", c);
    return abort_op(FAIL, cp, ctx);
  } else {
    c = *cp = fill_incomplete(c);
    assert_error(is_closure(c) &&
           closure_is_ready(c));
    stats.reduce_cnt++;
    return op_call(c->op, cp, ctx);
  }
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
  csize_t in = closure_args(c) - closure_out(c);
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
      remaining_a--;
    }
    assert_eq(remaining_a, 0);
    if(it.row) FLAG_SET(**ll, value, ROW);
  }

  return b;
}

cell_t *ready_func(op op, csize_t in, csize_t out) {
  assert_error(out > 0);
  csize_t args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->expr.out = out - 1;
  c->expr.flags = 0;
  c->op = op;
  return c;
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
  c->value.type = T_FAIL;
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

void store_dep_var(cell_t *c, cell_t *res, csize_t pos, type_t t, alt_set_t alt_set) {
  cell_t *d = c->expr.arg[pos];
  if(d && is_dep(d)) {
    drop(c);
    d->expr.arg[0] = res;
    store_dep(d, res->value.var, pos, t, alt_set);
  }
}

response abort_op(response rsp, cell_t **cp, context_t *ctx) {
  cell_t *c = *cp;
  if(rsp == FAIL) {
    if(!is_cell(c)) {
      *cp = NULL;
      return rsp;
    }
    WATCH(c, "abort_op");
    cell_t *alt = ref(c->alt);
    if(c->n && ctx->t == T_ANY && !ctx->expected) { // HACK this should be more sophisticated
      TRAVERSE(c, in) {
        drop(*p);
      }
      TRAVERSE(c, out) {
        cell_t *d = *p;
        if(d && is_dep(d)) {
          assert_error(c == d->expr.arg[0]);
          drop(c);
          store_fail(d, d->alt);
        }
      }
      closure_shrink(c, 1);
      memset(&c->value, 0, sizeof(c->value));
      c->op = OP_value;
      c->value.type = T_FAIL;
    }
    drop(c);
    *cp = alt;
    stats.fail_cnt++;
  }
  return rsp;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = *cp;
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
  cell_t **p = &a->alt;
  while(*p) p = &(*p)->alt;
  *p = b;
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
        drop(p->tmp);
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

void store_lazy_dep(cell_t *d, cell_t *r, alt_set_t alt_set) {
  if(d) {
    assert_error(!d->alt);
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
  LOG_WHEN(n > 0, "allocated alt ids %d...%d", r, alt_cnt-1);
  return r;
}

#if INTERFACE
#define CTX_INHERIT                             \
  .priority = ctx->priority,                    \
  .up = ctx,                                    \
  .inv = ctx->inv
#define CTX(type, ...) CONCAT(CTX_, type)(__VA_ARGS__)
#define CTX_list(_in, _out) \
  ((context_t) { .t = T_LIST, .s = { .in = _in, .out = _out }, CTX_INHERIT})
#define CTX_t_1(_t)                                                     \
  ((context_t) { .t = _t, CTX_INHERIT, .expected = false })
#define CTX_t_2(_t, _expected_val)                                          \
  ((context_t) { .t = _t, CTX_INHERIT, .expected = true, .expected_value = _expected_val })
#define CTX_t_3(_t, _expected, _expected_val)                            \
  ((context_t) { .t = _t, CTX_INHERIT, .expected = _expected, .expected_value = _expected_val })
#define CTX_t(...) DISPATCH(CTX_t, __VA_ARGS__)
#define CTX_any() CTX_t(T_ANY)
#define CTX_int(...) CTX_t(T_INT, ##__VA_ARGS__)
#define CTX_float(...) CTX_t(T_FLOAT, ##__VA_ARGS__)
#define CTX_symbol(...) CTX_t(T_SYMBOL, ##__VA_ARGS__)
#define CTX_string(...) CTX_t(T_STRING, ##__VA_ARGS__)
#define CTX_opaque() CTX_t(T_OPAQUE)
#define CTX_return() ((context_t) { .t = T_RETURN })
#define CTX_INV(invert) ctx->expected, (ctx->expected ? invert(ctx->expected_value) : 0)
#define CTX_UP ((context_t) { .t = ctx->t, .s = { .in = ctx->s.in, .out = ctx->s.out }, CTX_INHERIT})
#endif

// default 'ctx' for CTX(...) to inherit
context_t * const ctx = &(context_t) { .t = T_ANY, .priority = PRIORITY_TOP };

context_t *ctx_pos(context_t *ctx, uint8_t pos) {
  ctx->pos = pos;
  return ctx;
}

bool check_type(type_t requested, type_t expected) {
  if(expected == T_FAIL ||
     (expected != T_ANY &&
      !ONEOF(requested, T_ANY,
                        T_BOTTOM,
            expected))) {
    LOG("check_type: requested %t, but expected %t", requested, expected);
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
  if(!is_value(a)) return;
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

cell_t *build31(op op, cell_t *i0, cell_t *i1, cell_t *i2) {
  cell_t *c = closure_alloc(3);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = i1;
  c->expr.arg[2] = i2;
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

cell_t *build32(op op, cell_t *i0, cell_t *i1, cell_t *i2, cell_t **o1) {
  cell_t *c = closure_alloc(4);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = i1;
  c->expr.arg[2] = i2;
  c->expr.arg[3] = *o1 = dep(ref(c));
  return c;
}

cell_t *build23(op op, cell_t *i0, cell_t *i1, cell_t **o1, cell_t **o2) {
  cell_t *c = closure_alloc(4);
  c->op = op;
  c->expr.arg[0] = i0;
  c->expr.arg[1] = i1;
  c->expr.arg[2] = *o1 = dep(ref(c));
  c->expr.arg[3] = *o2 = dep(ref(c));
  return c;
}

// cache value in context on the way up?
bool is_linear(context_t *ctx) {
  while(ctx) {
    if(ctx->src && ctx->src->n) return false;
    ctx = ctx->up;
  }
  return true;
}

#if INTERFACE
enum priority {
  PRIORITY_SIMPLIFY = 0,
  PRIORITY_VAR = 1,
  PRIORITY_ASSERT = 2,
  PRIORITY_EXEC_SELF = 3,
  PRIORITY_DELAY = 4,
  PRIORITY_OTHERWISE = 5,
  PRIORITY_MAX
};
#define PRIORITY_TOP (PRIORITY_MAX - 1)
#endif

bool should_delay(context_t *ctx, int priority) {
  return ctx->priority == PRIORITY_SIMPLIFY ?
    ONEOF(priority, PRIORITY_VAR, PRIORITY_EXEC_SELF):
    ctx->priority < priority;
}

// b.in > a.out
//   c.in = a.in + b.in - a.out
//   c.out = b.out
// b.in < a.out
//   c.in = a.in
//   c.out = b.out + a.out - b.in

qsize_t compose_size_c(csize_t n, qsize_t a, qsize_t b) {
  a.out += n;
  return (qsize_t) {
    .in  = a.in  + csub(b.in, a.out),
    .out = b.out + csub(a.out, b.in)
  };
}

// solve for the size of a in: a b . ==> c
qsize_t compose_size_a(csize_t n, qsize_t b, qsize_t c) {
  return (qsize_t) {
    .in = c.in,
    .out = csub(b.in + csub(c.out, b.out), n)
  };
}

// solve for the size of b in: a b . ==> c
qsize_t compose_size_b(csize_t n, qsize_t a, qsize_t c) {
  return (qsize_t) {
    .in = a.out + n + csub(c.in, a.in),
    .out = c.out
  };
}

TEST(compose_size) {
  COUNTUP(c_in, 4) {
    COUNTUP(c_out, 4) {
      COUNTUP(n, 4) {
        qsize_t
          c = { .in = c_in, .out = c_out },
          a = { .in = c_in & ~1, .out = c_out & ~1 },
          b = compose_size_b(n, a, c);
        a = compose_size_a(n, b, c);
        c = compose_size_c(n, a, b);
        LOG("%d -> %d (%d) %d -> %d ===> %d -> %d", a.in, a.out, n, b.in, b.out, c.in, c.out);
        if(c.in != c_in || c.out != c_out) return -1;
      }
    }
  }
  return 0;
}

qsize_t csub_size(qsize_t a, qsize_t b) {
  return (qsize_t) {
    .in  = csub(a.in,  b.in),
    .out = csub(a.out, b.out)
  };
}
