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

#include "rt_types.h"
#include <string.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "trace.h"
#include "list.h"

OP(value) {
  PRE(value);
  stats.reduce_cnt--;

  CHECK_IF(is_var(c) && ctx->delay_var, DELAY);

  // promote integer constants to float constants
  if(ctx->t == T_FLOAT &&
     NOT_FLAG(c->value, VALUE_VAR) &&
     c->value.type == T_INT) {
    val_t x = c->value.integer;
    LOG("convert integer constant %d", x);
    *cp = float_val(x);
    drop(c);
    return RETRY;
  }

  // TODO move this check out - ctx shouldn't cause a FAIL
  CHECK_IF(FLAG(c->value, VALUE_FAIL) ||
           !type_match(ctx->t, c),
           FAIL);

  if(ctx->expected && !is_var(c) &&
     ctx->expected_value != c->value.integer) {
    LOG("expected %C to be %d, but got %d",
        c, ctx->expected_value, c->value.integer);
  }

  // NOTE: may create multiple placeholder
  // TODO use rows to work around this
  if(is_var(c)) {
    trace_dep(c);
    if(is_any(c) && ctx->t != T_ANY) {
      c->value.type = ctx->t;
      trace_update(c, c);
    }
    if(ctx->t == T_LIST &&
       is_function(c)) {
      placeholder_extend(cp, ctx->in, ctx->out);
    }
  } else if(is_row_list(c)) {
    placeholder_extend(cp, ctx->in, ctx->out);
  } else if(c->pos) {
    if(is_list(c) && !is_empty_list(c)) {
      TRAVERSE(c, ptrs) {
        mark_pos(*p, c->pos);
      }
    } else {
      // *** probably shouldn't be calling trace functions directly here
      cell_t *entry = trace_expr_entry(c->pos);
      cell_t *parent = entry->entry.parent;
      if(parent) {
        LOG_WHEN(is_list(c), "nil %C", c);
        int v = trace_store_value(entry->entry.parent, c);
        cell_t *tc = trace_alloc_var(entry);
        LOG("move value %C %T -> %T", c, tc, &parent[v]);
        c->value.var = tc;
        c->value.flags = VALUE_VAR;
        tc->value.var = &parent[v];
        c->pos = 0;
      }
    }
  }

  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

cell_t *make_val(uint8_t t) {
  cell_t *c = closure_alloc(2);
  c->op = OP_value;
  c->value.type = t;
  return c;
}

cell_t *val(uint8_t t, val_t x) {
  cell_t *c = make_val(t);
  c->value.integer = x;
  return c;
}

cell_t *int_val(val_t x) {
  return val(T_INT, x);
}

cell_t *float_val(double x) {
  cell_t *c = make_val(T_FLOAT);
  c->value.flt = x;
  return c;
}

cell_t *symbol(val_t sym) {
  return val(T_SYMBOL, sym);
}

bool is_value(cell_t const *c) {
  return c && c->op == OP_value;
}

void placeholder_extend(cell_t **lp, int in, int out) {
  cell_t *l = *lp;
  assert_error(l->value.type == T_LIST);
  if(in == 0 && out == 0) return;
  if(is_var(l)) {
    *lp = var_create_list(l, in, out, 0);
    return;
  }
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
  if((*left)->op == OP_assert) reduce(left, &CTX(any));
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

cell_t *var_create(type_t t, cell_t *tc, int in, int out) {
  cell_t *v = var_create_nonlist(t, tc);
  return t == T_LIST && (in || out) ?
    var_create_list(v, in, out, 0) :
    v;
}

cell_t *var_create_nonlist(type_t t, cell_t *tc) {
  cell_t *c = closure_alloc(1);
  c->op = OP_value;
  c->size = 2;
  c->value.var = tc;
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
  return var_create(t, tc_get(entry, ix), 0, 0);
}

cell_t *var_(type_t t, cell_t *c, uint8_t pos) {
  assert_error(c);
  cell_t *entry = trace_expr_entry(pos);
  TRAVERSE(c, in) {
    cell_t *a = clear_ptr(*p);
    if(a && is_var(a)) {
      // inherit entry with highest pos
      cell_t *e = var_entry(a->value.var);
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
  int len = (sizeof(cell_t *) + s.n) / sizeof(cell_t *);
  cell_t *c = closure_alloc(len + 1);
  c->op = OP_value;
  c->value.type = T_STRING;
  memcpy(c->value.str, s.s, s.n);
  c->value.str[s.n] = '\0';
  return c;
}

cell_t *make_strcat(seg_t s1, seg_t s2) {
  int n = s1.n + s2.n;
  int len = (sizeof(cell_t *) + n) / sizeof(cell_t *);
  cell_t *c = closure_alloc(len + 1);
  c->op = OP_value;
  c->value.type = T_STRING;
  memcpy(c->value.str, s1.s, s1.n);
  memcpy(c->value.str + s1.n, s2.s, s2.n);
  c->value.str[n] = '\0';
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

/* todo: propagate types here */
OP(dep) {
  PRE(dep);
  int pos = c->pos;
  /* rely on another cell for reduction */
  /* don't need to drop arg, handled by other function */
  /* must temporarily reference to avoid replacement of p which is referenced elsewhere */
  cell_t *p = ref(c->expr.arg[0]);
  assert_error(is_dep_of(c, p));
  insert_root(&p);
  CHECK(reduce_one(&p, &CTX(any)));
  CHECK_DELAY();
  trace_dep(c);
  remove_root(&p);
  drop(p);
  if(pos) (*cp)->pos = pos; // ***
  return RETRY;

 abort:
  remove_root(&p);
  drop(p);
  return abort_op(rsp, cp, ctx);
}

cell_t *dep(cell_t *c) {
  return build11(OP_dep, c);
}

bool is_dep(cell_t const *c) {
  return c->op == OP_dep;
}

// this shouldn't reduced directly, but is called through reduce_partial from func_dep
WORD("??", placeholder, 0, 1)
OP(placeholder) {
  PRE(placeholder);
  CHECK_IF(!check_type(ctx->t, T_LIST), FAIL);
  csize_t in = closure_in(c), n = closure_args(c);

  if(n == 1) {
    *cp = CUT(c, expr.arg[0]);
    return RETRY;
  }

  assert_error(in >= 1);
  CHECK(reduce_arg(c, in - 1, &CTX(list, 0, 0))); // *** should use ctx
  COUNTUP(i, in - 1) {
    CHECK(reduce_arg(c, i, &CTX(any)));
    CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  }
  CHECK_DELAY();
  clear_flags(c);

  // compose X [] --> X
  if(in == 2 &&
     is_list(c->expr.arg[1]) &&
     list_size(c->expr.arg[1]) == 0 &&
     is_function(c->expr.arg[0])) {
    store_reduced(cp, mod_alt(ref(c->expr.arg[0]), c->alt, ctx->alt_set));
    return SUCCESS;
  }

  cell_t *res = var(T_LIST, c, ctx->pos);
  res->alt = c->alt;
  res->value.alt_set = ctx->alt_set;
  RANGEUP(i, in, n) {
    cell_t *d = c->expr.arg[i];
    if(d && is_dep(d)) {
      store_dep_var(c, res, i, T_ANY, ctx->alt_set);
    } else {
      LOG("dropped placeholder[%C] output", c);
    }
  }
  add_conditions_from_array(res, c->expr.arg, in);
  store_reduced(cp, res);
  ASSERT_REF();
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

bool is_placeholder(cell_t const *c) {
  return c && c->op == OP_placeholder;
}

OP(fail) {
  PRE(fail);
  stats.reduce_cnt--;
  return abort_op(FAIL, cp, ctx);
}
