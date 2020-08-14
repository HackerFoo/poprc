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

#include <string.h>
#include <stdio.h>

#include "rt_types.h"
#include "rt.h"
#include "cells.h"
#include "startle/log.h"
#include "startle/error.h"
#include "list.h"
#include "special.h"
#include "mutate.h"

/* replace references in r with corresponding tmp references */
/* m => in-place, update ref counts for replaced references */
static
void mutate_update(cell_t *r, bool m) {
  WATCH(r, "mutate_update");
  TRAVERSE(r, alt, in, ptrs) {
    cell_t *c = *p;
    if(is_closure(c) && !is_persistent(c)) {
      if(c->tmp) {
        *p = ref(c->tmp);
        if (m) --c->n;
      } else if (!m) ref(c);
    }
  }

  TRAVERSE(r, out) {
    cell_t *c = *p;
    if(c && !is_persistent(c) && c->tmp) {
      // if(m) fix deps?
      *p = c->tmp;
    }
  }
}

static
cell_t *add_to_list(cell_t *c, cell_t *nc, cell_t **l) {
  WATCH(c, "add_to_list");
  nc->n = -1;
  CONS(tmp, l, nc);
  CONS(tmp, l, c);
  assert_error(check_tmp_loop(*l));
  return nc;
}

cell_t *add_to_mutate_list(cell_t *c, cell_t **l) {
  return c->tmp ? c->tmp : add_to_list(c, copy(c), l);
}

/* traverse r and make copies to tmp */
/* u => subtree is unique, exp => to be expanded */
static
bool mutate_sweep(cell_t *r, cell_t **l) {
  if(!is_closure(r) || is_persistent(r)) return false;
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
    add_to_mutate_list(r, l);
    return true;
  }
}

bool deps_are_unique(cell_t *c) {
  TRAVERSE(c, out) {
    if(*p && ~(*p)->n) return false;
  }
  return true;
}

cell_t *get_mutable(cell_t *c) {
  return c->tmp ? c->tmp : c;
}

/* make a path copy from the root (r) to the cell to modify (c) and store in tmps */
/* r references c. Optimization over: */
/* r' = deep_copy(r) */
/* drop(r) */
/* modify c' in r' without affecting c */
/* returns a chain of substitutions, use mutate_finish to finish */
cell_t *mutate(cell_t **cp, cell_t **rp) {
  cell_t *c = *cp;
  cell_t *l = NULL;
  USE_TMP();
  add_to_mutate_list(c, &l);
  l = mutate_list(l, rp);
  *cp = get_mutable(c);
  return l;
}

/* mutate a list of cells built with add_to_mutate_list */
cell_t *mutate_list(cell_t *l, cell_t **rp) {
  cell_t *r = *rp;

  // check if necessary
  fake_drop(r);
  FOLLOW(c, nc, l, tmp) {
    if(!~c->n) {
      refcount_t n = c->n;
      memcpy(c, nc, sizeof(cell_t) * closure_cells(c));
      closure_free(nc);
      c->n = n;
      *_prev = nc->tmp; // delete
    }
  }
  if(!l) {
    fake_undrop(r);
    return NULL;
  }

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
  if(r->tmp) *rp = r->tmp;
  return l;
}

bool check_deps(cell_t *c) {
  return _check_deps(c, 20);
}

bool _check_deps(cell_t *c, int depth) {
  assert_error(depth >= 0);
  if(c && is_cell(c)) {
    TRAVERSE(c, out) {
      cell_t *x = *p;
      if(x && x->expr.arg[0] != c) {
        printf("bad dep %d -> %d, should be %d\n",
               (int)(x - cells),
               (int)(x->expr.arg[0] - cells),
               (int)(c - cells));
        return false;
      }
    }
    TRAVERSE(c, alt, in, ptrs) {
      if(!_check_deps(*p, depth - 1)) return false;
    }
  }
  return true;
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

// clean up after mutate
void mutate_finish(cell_t *l) {
  while(l) {
    cell_t *next = l->tmp;

    // remove escaped deps
    TRAVERSE(l, out) {
      if(*p && (*p)->expr.arg[0] != l) {
        *p = NULL;
      }
    }

    l->tmp = 0;
    assert_error(~l->n);
    l = next;
  }
  done_with_tmp();
}
