/* Copyright 2012-2017 Dustin DeWeese
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

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "trace.h"
#include "list.h"

OP(value) {
  cell_t *c = *cp;
  response rsp;
  cell_t *res = NULL;
  PRE(c, value);
  stats.reduce_cnt--;

  // promote integer constants to float constants
  if(treq.t == T_FLOAT &&
     NOT_FLAG(c->value, VALUE_VAR) &&
     c->value.type == T_INT) {
    val_t x = c->value.integer[0];
    LOG("convert integer constant %d", x);
    *cp = float_val(x);
    drop(c);
    return RETRY;
  }

  if(FLAG(c->value, VALUE_FAIL) ||
     !type_match(treq.t, c)) {
    if(treq.t == T_FUNCTION && c == &nil_cell) return SUCCESS; // HACK
    rsp = FAIL;
    goto abort;
  }

  // NOTE: may create multiple placeholder
  // TODO use rows to work around this
  if(is_var(c)) {
    trace_dep(c);
    if(is_any(c)) {
      if(treq.t == T_LIST) {
        res = var_create(T_LIST, c->value.tc, treq.in, treq.out);
        res->value.alt_set = c->value.alt_set;
        res->alt = c->alt;
        if(c->n) {
          c->n--;
          LOG(TODO " share list var %C -> %C", c, res);
#if 0 // TODO get this working
          closure_shrink(c, 2);
          c->value.type = T_LIST;
          FLAG_SET(c->value, T_ROW);
          FLAG_CLEAR(c->value, T_VAR);
          c->value.ptr[0] = ref(res);
#endif
        } else {
          closure_free(c);
        }
        *cp = res;
      } else if(treq.t != T_ANY) {
        c->value.type = treq.t;
        trace_update(c, c);
      }
    }
  } else if(is_row_list(c)) {
    placeholder_extend(cp, treq.in, treq.out);
  } else if(c->pos && !is_list(c)) {
    // *** probably shouldn't be calling trace functions directly here
    cell_t *entry = trace_expr_entry(c->pos);
    cell_t *parent = entry->entry.parent;
    if(parent) {
      int v = trace_store_value(entry->entry.parent, c);
      int t = trace_alloc_var(entry);
      LOG("move value %C %e[%d] -> %e[%d]", c, entry, t, parent, v);
      c->value.tc = (trace_cell_t) {
        .entry = entry,
        .index = t
      };
      c->value.flags = VALUE_VAR;
      cell_t *tc = trace_cell_ptr(c->value.tc);
      tc->value.tc = (trace_cell_t) {
        .entry = parent,
        .index = v
      };
    }
  }

  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

cell_t *int_val(val_t x) {
  cell_t *c = closure_alloc(2);
  c->op = OP_value;
  c->value.type = T_INT;
  c->value.integer[0] = x;
  return c;
}

cell_t *float_val(double x) {
  cell_t *c = closure_alloc(2);
  c->op = OP_value;
  c->value.type = T_FLOAT;
  c->value.flt[0] = x;
  return c;
}

cell_t *symbol(val_t sym) {
  cell_t *c = closure_alloc(2);
  c->op = OP_value;
  c->value.type = T_SYMBOL;
  c->value.integer[0] = sym;
  return c;
}

bool is_value(cell_t const *c) {
  return c && c->op == OP_value;
}

void placeholder_extend(cell_t **lp, int in, int out) {
  cell_t *l = *lp;
  if(!is_row_list(l)) return;
  csize_t
    f_in = function_in(l),
    f_out = function_out(l, false),
    d_in = in - min(in, f_in),
    d_out = out - min(out, f_out);
  if(d_in == 0 && d_out == 0) return;
  cell_t **left = leftmost_row(&l);
  if(!left) return;
  // HACK need to map_assert after extending
  if((*left)->op == OP_assert) reduce(left, REQ(any));
  cell_t *f = *left;
  if(!(is_function(f) || is_placeholder(f))) return;
  cell_t *ph = func(OP_placeholder, d_in + 1, d_out + 1);

  if(l->n) {
    l->n--;
    l = copy(l);
    TRAVERSE_REF(l, alt, ptrs);
    left = leftmost_row(&l);
    f = *left;
  }
  LOG("placeholder_extend: (%d, %d) %C -> %C", in, out, *lp, l);

  if(d_out) {
    cell_t *l_exp = make_list(d_out + 1);
    l_exp->value.flags = VALUE_ROW;
    COUNTUP(i, d_out) {
      cell_t *d = dep(ph);
      l_exp->value.ptr[i] = d;
      arg(ph, d);
    }
    *left = l_exp;
    left = &l_exp->value.ptr[d_out];
  }

  arg(ph, f);
  refn(ph, d_out);
  *left = ph;
  *lp = l;
}

cell_t *var_create(type_t t, trace_cell_t tc, int in, int out) {
  return t == T_LIST ?
    var_create_list(var_create_nonlist(T_FUNCTION, tc), in, out, 0) :
    var_create_nonlist(t, tc);
}

cell_t *var_create_nonlist(type_t t, trace_cell_t tc) {
  cell_t *c = closure_alloc(1);
  c->op = OP_value;
  c->size = 2;
  c->value.tc = tc;
  c->value.flags = VALUE_VAR;
  c->value.type = t;
  trace_update_type(c);
  return c;
}

cell_t *var_create_list(cell_t *f, int in, int out, int shift) {
  cell_t *c = make_list(out + shift + 1);
  cell_t *ph = func(OP_placeholder, in + 1, out + 1);
  cell_t **a = &c->value.ptr[shift];
  COUNTUP(i, out) {
    cell_t *d = dep(ph);
    a[i] = d;
    arg(ph, d);
  }
  arg(ph, f);
  refn(ph, out);
  a[out] = ph;
  c->value.flags = VALUE_ROW;
  return c;
}

cell_t *var_create_with_entry(type_t t, cell_t *entry, csize_t size) {
  assert_error(entry);
  int ix = trace_alloc(entry, size);
  return var_create(t, (trace_cell_t) {entry, ix}, 0, 0);
}

cell_t *var_(type_t t, cell_t *c, uint8_t pos) {
  assert_error(c);
  cell_t *entry = trace_expr_entry(pos);
  TRAVERSE(c, in) {
    cell_t *a = clear_ptr(*p);
    if(a && is_var(a)) {
      // inherit entry with highest pos
      cell_t *e = a->value.tc.entry;
      if(e && e->pos > pos) {
        pos = e->pos;
        entry = e;
      }
    }
  }

  return var_create_with_entry(t, entry, c->size);
}

#if INTERFACE
#define var(...) DISPATCH(var, __VA_ARGS__)
#define var_3(t, c, pos) var_(t, c, pos)
#define var_2(t, c) var_(t, c, 0)
#endif

bool is_var(cell_t const *c) {
  return c && is_value(c) && FLAG(c->value, VALUE_VAR);
}

cell_t *vector(csize_t n) {
  cell_t *c = closure_alloc(n+1);
  c->op = OP_value;
  c->value.type = T_ANY;
  return c;
}

cell_t *make_map(csize_t s) {
  csize_t cs = calculate_map_size(s);
  cell_t *c = closure_alloc_cells(cs);
  uintptr_t size = (sizeof(cell_t) * cs - offsetof(cell_t, value.map)) / sizeof(pair_t) - 1;
  c->op = OP_value;
  c->size = 2 * (size + 1) + 1;
  c->value.type = T_MAP;
  c->value.map[0].first = size;
  c->value.map[0].second = 0;
  return c;
}

bool is_map(cell_t const *c) {
  return c && is_value(c) && c->value.type == T_MAP;
}

cell_t *make_string(seg_t s) {
  cell_t *c = closure_alloc(1);
  c->op = OP_value;
  c->value.type = T_STRING;
  c->value.str = s;
  return c;
}

bool is_string(cell_t const *c) {
  return c && is_value(c) && c->value.type == T_STRING;
}

bool is_dep_of(cell_t *d, cell_t *c) {
  bool ret = false;
  TRAVERSE(c, out) {
    if(*p == d) ret = true;
  }
  return ret;
}

void move_delay_flag(cell_t *src, cell_t *dst) {
  if(FLAG(src->expr, EXPR_DELAYED) &&
     NOT_FLAG(dst->expr, EXPR_DELAYED)) {
    FLAG_CLEAR(src->expr, EXPR_DELAYED);
    FLAG_SET(dst->expr, EXPR_DELAYED);
  }
}

/* todo: propagate types here */
OP(dep) {
  cell_t *c = *cp;
  response rsp;
  PRE(c, dep);
  /* rely on another cell for reduction */
  /* don't need to drop arg, handled by other function */
  /* must temporarily reference to avoid replacement of p which is referenced elsewhere */
  cell_t *p = ref(c->expr.arg[0]);
  assert_error(is_dep_of(c, p));
  // move delay flag out to dep
  move_delay_flag(p, c);
  insert_root(&p);
  CHECK(reduce_dep(&p, REQ(any)));
  trace_dep(c);
  remove_root(&p);
  drop(p);
  return RETRY;

 abort:
  // move delay flag out to dep
  if(rsp == DELAY) {
    move_delay_flag(p, c);
  }
  remove_root(&p);
  drop(p);
  return abort_op(rsp, cp, treq);
}

cell_t *dep(cell_t *c) {
  cell_t *n = closure_alloc(1);
  n->op = OP_dep;
  n->expr.arg[0] = c;
  return n;
}

bool is_dep(cell_t const *c) {
  return c->op == OP_dep;
}

// this shouldn't reduced directly, but is called through reduce_partial from func_dep
WORD("??", placeholder, 0, 1)
OP(placeholder) {
  cell_t *c = *cp;
  response rsp;
  PRE(c, placeholder);
  CHECK(!check_type(treq.t, T_FUNCTION), FAIL);
  csize_t in = closure_in(c), n = closure_args(c);

  if(n == 1) {
    *cp = CUT(c, expr.arg[0]);
    return RETRY;
  }

  alt_set_t alt_set = 0;
  assert_error(in >= 1);
  CHECK(reduce_arg(c, in - 1, &alt_set, REQ(function)));
  COUNTUP(i, in - 1) {
    CHECK(AND0(reduce_arg(c, i, &alt_set, REQ(any)),
               fail_if(as_conflict(alt_set))));
  }
  clear_flags(c);

  // compose X [] --> X
  if(in == 2 &&
     is_list(c->expr.arg[1]) &&
     list_size(c->expr.arg[1]) == 0 &&
     is_function(c->expr.arg[0])) {
    store_reduced(cp, mod_alt(ref(c->expr.arg[0]), c->alt, alt_set));
    return SUCCESS;
  }

  cell_t *res = var(T_FUNCTION, c, treq.pos);
  res->alt = c->alt;
  res->value.alt_set = alt_set;
  RANGEUP(i, in, n) {
    cell_t *d = c->expr.arg[i];
    if(d && is_dep(d)) {
      drop(c);
      d->expr.arg[0] = res;
      store_dep(d, res->value.tc, i, T_ANY, alt_set);
    } else {
      LOG("dropped placeholder[%C] output", c);
    }
  }
  store_reduced(cp, res);
  ASSERT_REF();
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, treq);
}

bool is_placeholder(cell_t const *c) {
  return c && c->op == OP_placeholder;
}

OP(fail) {
  cell_t *c = *cp;
  PRE(c, fail);
  stats.reduce_cnt--;
  return abort_op(FAIL, cp, treq);
}
