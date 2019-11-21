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
#include "startle/support.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "trace.h"
#include "list.h"

bool ctx_has_pos(context_t *ctx) {
  FOLLOW(p, ctx, up) {
    if(p->src && p->src->pos) {
      return true;
    }
  }
  return false;
}

bool ctx_split(cell_t *c, context_t *ctx) {
  assert_error(is_var(c), "split on context only at variables");
  tcell_t *entry = var_entry(c->value.var);
  if(entry->entry.wrap) return false;
  if(ctx_has_pos(ctx)) return false; // ***
  alt_set_t c_as = c->value.alt_set;
  alt_set_t ctx_as = ctx_alt_set(ctx);
  if(c_as == ctx_as) return true;

  alt_set_t combined_as = c_as | ctx_as;
  alt_set_t common = c_as & ctx_as;
  if(as_conflict(combined_as)) return false;
  alt_set_t differ = (c_as ^ ctx_as) & AS_MASK;
  cell_t *alt = c->alt;
  cell_t **p = &c->alt;
  if(!differ) return true;
  c->value.alt_set = combined_as;
  FOR_MASK(i, differ) {
    alt_set_t as = common | as_from_mask(differ, i);
    if(!as_conflict(as | combined_as)) continue;
    cell_t *a = copy(c);
    a->value.alt_set = as;
    LOG("ctx_split %C -> %C %S", c, a, as);
    *p = a;
    p = &a->alt;
  }
  *p = alt;
  return true;
}

OP(value) {
  PRE(value);
  stats.reduce_cnt--;
  ctx->alt_set = c->value.alt_set;
  if(is_var(c)) {
    if(ctx->t == T_INT) {
      range_t r = range_intersect(c->value.range, ctx->bound);
      if(!range_eq(r, c->value.range)) {
        if(ctx_split(c, ctx)) {
          c->value.range = r;
          if(is_any(c) && ctx->t != T_ANY) {
            c->value.type = ctx->t;
            trace_update_type(c);
          }
        }
      }
    }
    CHECK_PRIORITY(PRIORITY_VAR);
  }

  // convert from T_BOTTOM
  if(c->value.type == T_BOTTOM && ctx->t != T_ANY) {
    c->value.type = ctx->t;
    trace_update_type(c);
  }

  // promote integer constants to float constants
  if(ctx->t == T_FLOAT &&
     NOT_FLAG(*c, value, VAR) &&
     c->value.type == T_INT) {
    val_t x = c->value.integer;
    LOG("convert integer constant %d", x);
    *cp = float_val(x);
    drop(c);
    return RETRY;
  }

  if(ctx->t == T_OPAQUE &&
     NOT_FLAG(*c, value, VAR) &&
     c->value.type == T_SYMBOL) {
    CHECK_IF(!convert_to_opaque(c), FAIL);
    return RETRY;
  }

  // TODO move this check out - ctx shouldn't cause a FAIL
  CHECK_IF(!check_type(ctx->t, c->value.type), FAIL);

  // TODO handle expected types other than symbols
  if(ctx->t == T_SYMBOL &&
     !is_var(c) &&
     range_singleton(ctx->bound) &&
     ctx->bound.min != c->value.symbol) {
    LOG("expected %C to be %Y, but got %Y",
        c, ctx->bound.min, c->value.symbol);
  }

  // NOTE: may create multiple placeholder
  // TODO use rows to work around this
  if(is_var(c)) {
    trace_dep(c);
    if(is_any(c) && ctx->t != T_ANY) {
      c->value.type = ctx->t;
      if(ctx->t == T_OPAQUE) {
        assert_error(range_singleton(ctx->bound),
                     "must provide expected value when reducing opaque %C", c);
        c->value.symbol = ctx->bound.min;
      }
      trace_update(c);
    }
    if(ctx->t == T_LIST) {
      placeholder_extend(cp, ctx->s, false);
    }
  } else if(is_row_list(c)) {
    placeholder_extend(cp, ctx->s, false);
  } else if(c->pos) {
    if(is_list(c) && !is_empty_list(c)) {
      TRAVERSE(c, ptrs) {
        mark_pos(*p, c->pos);
      }
    } else {
      // *** probably shouldn't be calling trace functions directly here
      tcell_t *entry = trace_expr_entry(c->pos);
      tcell_t *parent = entry->entry.parent;
      tcell_t *ve = c->value.var ? var_entry(c->value.var) : NULL;
      if(entry != ve && parent) {
        LOG_WHEN(is_list(c), "nil %C", c);
        int v = trace_store_value(parent, c);
        tcell_t *tc = trace_alloc_var(entry, c->value.type);

        /* bounds too tight in case of recursion
        switch(c->value.type) {
        case T_OPAQUE:
        case T_SYMBOL:
          c->value.range = RANGE(c->value.symbol);
          break;
        case T_INT:
          c->value.range = RANGE(c->value.integer);
          break;
        default:
          c->value.range = default_bound;
          break;
        }
        */

        c->value.range = default_bound;

        LOG("move value %C %s[%d] -> %s[%d]", c,
            entry->word_name, tc-entry,
            parent->word_name, v);
        c->value.var = tc;
        c->value.flags = VALUE_VAR;
        tc->value.var = &parent[v];
        c->pos = 0;
        trace_update_range(c); // ***
      }
    }
  }

  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

cell_t *make_val(type_t t) {
  cell_t *c = alloc_value();
  *c = (cell_t) {
    .size = c->size,
    .op = OP_value,
    .value.type = t
  };
  return c;
}

cell_t *set_val(cell_t *c, type_t t, val_t x) {
  assert_error(ONEOF(t, T_INT, T_SYMBOL));
  if(t == T_INT) {
    c->value.integer = x;
  } else if(t == T_SYMBOL) {
    c->value.symbol = x;
  }
  return c;
}

cell_t *val(type_t t, val_t x) {
  return(set_val(make_val(t), t, x));
}

cell_t *int_val(val_t x) {
  return val(T_INT, x);
}

cell_t *float_val(double x) {
  cell_t *c = make_val(T_FLOAT);
  c->value.flt = x;
  return c;
}

cell_t *opaque(val_t sym, void *p) {
  cell_t *c = val(T_OPAQUE, sym);
  c->value.opaque = p;
  return c;
}

cell_t *symbol(val_t sym) {
  return val(T_SYMBOL, sym);
}

bool _is_value(cell_t const *c) {
  return c && c->op == OP_value;
}

void placeholder_extend(cell_t **lp, qsize_t s, bool wrap_var) {
  cell_t *l = *lp;
  assert_error(l->value.type == T_LIST, "not a list, type of l (cells[%C]) is %t", l, l->value.type);
  if(s.in == 0 && s.out == 0 && !wrap_var) return;
  if(is_var(l)) {
    cell_t *ex = trace_extension(l, s.in, s.out);
    if(!ex) ex = var_create_list(l, s.in, s.out, 0, wrap_var);
    *lp = ex;
    return;
  }
  if(!is_row_list(l)) return;
  qsize_t ds = csub_size(s, quote_size(l, false));
  if(ds.in == 0 && ds.out == 0) return;
  cell_t **left = leftmost_row(&l);
  if(!left) return;
  // HACK need to map_assert after extending
  if((*left)->op == OP_assert) force(left);
  cell_t *f = *left;
  if(!(is_function(f) || is_placeholder(f))) return;
  cell_t *ph = func(OP_placeholder, ds.in + 1, ds.out + 1);

  if(l->n) {
    l->n--;
    l = copy(l);
    TRAVERSE_REF(l, alt, ptrs);
    left = leftmost_row(&l);
    f = *left;
  }

  // TODO replace placeholder when possible
  LOG(MARK("WARN") " placeholder_extend: (%d, %d) %C -> %C", s.in, s.out, *lp, l);

  if(ds.out) {
    cell_t *l_exp = make_list(ds.out + 1);
    l_exp->value.flags = VALUE_ROW;
    COUNTUP(i, ds.out) {
      cell_t *d = dep(ph);
      l_exp->value.ptr[i] = d;
      arg(ph, d);
    }
    *left = l_exp;
    left = &l_exp->value.ptr[ds.out];
  }

  arg(ph, f);
  refn(ph, ds.out);
  *left = ph;
  *lp = l;
}

cell_t *var_create(type_t t, tcell_t *tc, int in, int out) {
  cell_t *v = var_create_nonlist(t, tc);
  return t == T_LIST && (in || out) ?
    var_create_list(v, in, out, 0, false) :
    v;
}

cell_t *var_create_bound(cell_t *v, tcell_t *tc, const context_t *ctx) {
  assert_error(is_var(v));
  type_t t = v->value.type;
  cell_t *res = var_create(t, tc, 0, 0);
  res->value.range = range_intersect(ctx->bound, get_range(v));
  return res;
}

cell_t *var_create_nonlist(type_t t, tcell_t *tc) {
  cell_t *c = alloc_value();
  *c = (cell_t) {
    .size = c->size,
    .op = OP_value,
    .value = {
      .var = tc,
      .flags = VALUE_VAR,
      .type = t,
      .range = default_bound
    }
  };
  trace_update_type(c);
  return c;
}

cell_t *var_create_list(cell_t *f, int in, int out, int shift, bool row) {
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
  if(row) FLAG_SET(*ph, expr, ROW);
  return c;
}

cell_t *var_create_with_entry(type_t t, tcell_t *entry, csize_t size) {
  assert_error(entry);
  int ix = trace_alloc(entry, size);
  return var_create(t, tc_get(entry, ix), 0, 0);
}

tcell_t *infer_entry(cell_t *c, uint8_t pos) {
  assert_error(c);
  TRAVERSE(c, in) {
    cell_t *a = *p;
    if(a && is_var(a)) {
      // inherit entry with highest pos
      int ep = entry_pos(var_entry(a->value.var));
      if(ep > pos) {
        pos = ep;
      }
    }
  }

  tcell_t *entry = trace_expr_entry(pos);
  if(!entry) {
    entry = trace_current_entry();
    assert_error(entry);
    LOG(HACK " using current entry %s", entry->word_name);
  }
  return entry;
}

cell_t *var_(type_t t, cell_t *c, uint8_t pos) {
  return var_create_with_entry(t, infer_entry(c, pos), c->size);
}

#if INTERFACE
#define var(...) DISPATCH(var, __VA_ARGS__)
#define var_3(t, c, pos) var_(t, c, pos)
#define var_2(t, c) var_(t, c, 0)
#endif

cell_t *opaque_var(cell_t *c, val_t sym) {
  cell_t *v = var(T_OPAQUE, c);
  v->value.symbol = sym;
  return v;
}

#if INTERFACE
#define is_var(c) _is_var(GET_CELL(c))
#define is_value(c) _is_value(GET_CELL(c))
#define is_dep(c) _is_dep(GET_CELL(c))
#endif

bool _is_var(cell_t const *c) {
  return c && is_value(c) && FLAG(*c, value, VAR);
}

cell_t *make_map(csize_t s) {
  csize_t cs = calculate_map_size(s);
  cell_t *c = closure_alloc_cells(cs);
  uintptr_t size = (sizeof(cell_t) * cs - offsetof(cell_t, value.map)) / sizeof(pair_t) - 1;
  *c = (cell_t) {
    .op = OP_value,
    .size = 2 * (size + 1) + 1,
    .value = {
      .type = T_MAP,
      .map = {{size, 0}}
    }
  };
  return c;
}

bool is_map(cell_t const *c) {
  return c && is_value(c) && c->value.type == T_MAP;
}

cell_t *make_unescaped_string(seg_t s) {
  cell_t *c = alloc_string(s.n);
  int size = unescape_string(c->value.str, s.n, s);
  set_string_size(c, size);
  return c;
}

cell_t *make_string(seg_t s) {
  cell_t *c = alloc_string(s.n);
  if(s.n) memcpy(c->value.str, s.s, s.n);
  return c;
}

cell_t *make_strcat(seg_t s1, seg_t s2) {
  cell_t *c = alloc_string(s1.n + s2.n);
  c->op = OP_value;
  c->value.type = T_STRING;
  memcpy(c->value.str, s1.s, s1.n);
  memcpy(c->value.str + s1.n, s2.s, s2.n);
  return c;
}

seg_t value_seg(const cell_t *c) {
  assert_error(is_string(c));
  return (seg_t) {
    .s = c->value.str,
    .n = string_size(c)
  };
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
  assert_error(!c->alt);
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
  ASSERT_REF();
  return abort_op(rsp, cp, ctx);
}

cell_t *dep(cell_t *c) {
  return build11(OP_dep, c);
}

bool _is_dep(cell_t const *c) {
  return c->op == OP_dep;
}

// this shouldn't reduced directly, but is called through reduce_partial from func_dep
// see func_compose_ap
WORD("??", placeholder, 0, 1)
OP(placeholder) {
  PRE(placeholder);
  CHECK_IF(!check_type(ctx->t, T_LIST), FAIL);
  csize_t
    in = closure_in(c),
    out = closure_out(c),
    n = closure_args(c);

  assert_error(in >= 1);
  CHECK(reduce_arg(c, in - 1, &CTX(list, csub(ctx->s.in, in),
                                         csub(ctx->s.out, out))));
  COUNTDOWN(i, in - 1) {
    CHECK(reduce_arg(c, i, &CTX(any)));
    CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  }
  CHECK_DELAY();

  // compose X [] --> X
  if(in == 2 &&
     is_list(c->expr.arg[1]) &&
     list_size(c->expr.arg[1]) == 0 &&
     is_function(c->expr.arg[0])) {
    store_reduced(cp, ctx, ref(c->expr.arg[0]));
    return SUCCESS;
  }

  cell_t *res;
  if(n == 1) {
    res = copy(c->expr.arg[0]);
  } else {
    res = var(T_LIST, c, ctx->pos);
    RANGEUP(i, in, n) {
      cell_t *d = c->expr.arg[i];
      if(d && is_dep(d)) {
        store_dep_var(c, res, i, T_ANY, ctx->alt_set);
      } else {
        LOG("dropped placeholder[%C] output", c);
      }
    }
  }
  FLAG_SET(*res, value, ROW);
  if(c->expr.out) FLAG_SET(*c, expr, PARTIAL);
  add_conditions_from_array(res, c->expr.arg, in);
  store_reduced(cp, ctx, res);
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
