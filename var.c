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

#include "rt_types.h"
#include <string.h>
#include <stdio.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"
#include "startle/support.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "list.h"
#include "var.h"
#include "parse/lex.h"
#include "parse/parse.h"

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
  LOG("var_create_list %C %C %d %d %d %d", f, c, in, out, shift, row);
  return c;
}

cell_t *var_create_with_entry(type_t t, tcell_t *entry, csize_t size) {
  assert_error(entry);
  int ix = trace_alloc(entry, size);
  return var_create(t, tc_get(entry, ix), 0, 0);
}

tcell_t *infer_entry(cell_t *c) {
  uint8_t pos = 0;
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

  assert_error(!c->pos || pos < c->pos, "broken barrier %C", c);
  tcell_t *entry =
    pos ? pos_entry(pos) :
    c->pos ? pos_entry(c->pos)->entry.parent :
    trace_current_entry();
  assert_error(entry);
  LOG_WHEN(!pos && !c->pos, HACK " using current entry %s", entry->word_name);
  return entry;
}

cell_t *var(type_t t, cell_t *c) {
  return var_create_with_entry(t, infer_entry(c), c->size);
}

cell_t *opaque_var(cell_t *c, val_t sym) {
  cell_t *v = var(T_OPAQUE, c);
  v->value.range = RANGE(sym);
  return v;
}

#if INTERFACE
#define is_var(c) _is_var(GET_CELL(c))
#endif

bool _is_var(cell_t const *c) {
  return c && is_value(c) && FLAG(*c, value, VAR);
}

/* conditions */

// TODO document these

void apply_condition(cell_t *c, int *x) {
  assert_error(is_value(c));
  if(c->value.var) {
    tcell_t *entry = var_entry(c->value.var);
    tcell_t *t = concatenate_conditions(c->value.var, &entry[*x]);
    c->value.var = t;
    *x = var_index(entry, t);
  }
}

bool depends_on(tcell_t *entry, int x, int y) {
  if(x == y) return true;
  tcell_t *tc = &entry[x];
  if((trace_type(tc) == T_LIST && !ONEOF(tc->op, OP_assert, OP_seq)) ||
     ONEOF(tc->op, OP_unless, OP_exec)) return false; // *** conservative
  TRAVERSE(tc, in) {
    int px = tr_index(*p);
    if(px && depends_on(entry, px, y)) {
      LOG("%E %O %d depends on %d", entry, tc->op, x, y);
      return true;
    }
  }
  return false;
}

bool has_asserted(tcell_t *entry, int x, int y) {
  tcell_t *tc = &entry[x];
  if(tc->op == OP_assert && tr_index(tc->expr.arg[1]) == y) return true;
  if((trace_type(tc) == T_LIST && !ONEOF(tc->op, OP_assert, OP_seq)) ||
     ONEOF(tc->op, OP_unless, OP_exec)) return false; // *** conservative
  TRAVERSE(tc, in) {
    int px = tr_index(*p);
    if(px && has_asserted(entry, px, y)) {
      LOG("%E %O %d depends on %d", entry, tc->op, x, y);
      return true;
    }
  }
  return false;
}

// apply condition from trace i to v
int copy_conditions(tcell_t *entry, int i, int v) {
  int x = i;
  if(!i || i == v) return v;
  tcell_t *tc = &entry[i];
  if(!ONEOF(tc->op, OP_assert, OP_seq, OP_unless)) return v;
  if(tc->op == OP_seq && tr_index(tc->expr.arg[1]) == v) return v;
  int a = tr_index(tc->expr.arg[0]);
  int an = copy_conditions(entry, a, v);
  if(an) {
    switch(tc->op) {
    case OP_seq:
      if(depends_on(entry, an, tr_index(tc->expr.arg[1]))) return an;
      break;
    case OP_assert:
      if(has_asserted(entry, an, tr_index(tc->expr.arg[1]))) return an;
      break;
    default: break;
    }
  }
  if(a != an) {
    if(a) {
      x = trace_copy_cell(entry, &tc->c);
      tcell_t *tn = &entry[x];
      tn->n = ~0;
      entry[tr_index(tc->expr.arg[1])].n++;
    }
    entry[x].expr.arg[0] = index_tr(an);
    if(an) entry[an].n++;
  }
  if(an) {
    tcell_t *p = &entry[x], *q = &entry[an];
    p->trace.type = q->trace.type;
    p->trace.range = range_intersect(p->trace.range, q->trace.range);
  }
  LOG("copy condition in %s: %d -> %d", entry->word_name, i, x);
  return x;
}

void reserve_condition(tcell_t **p) {
  if(*p) {
    tcell_t *entry = var_entry(*p);
    *p = &entry[copy_conditions(entry, var_index(entry, *p), 0)];
  }
}

// assert & unless form lists that can be concatenated
// TODO use refcounting to avoid destructive concatenation
tcell_t *concatenate_conditions(tcell_t *a, tcell_t *b) {
  if(a == NULL) return b;
  if(b == NULL) return a;
  tcell_t *entry = var_entry(a);
  switch_entry_var(entry, b, false);
  b = var_for_entry(entry, b);
  assert_error(b, "%s %T %T", entry->word_name, a, b);
  int bi = var_index(entry, b);
  tcell_t *an = &entry[copy_conditions(entry, var_index(entry, a), bi)];
  LOG("condition %s %d ... %d %O arg <- %d",
      entry->word_name, a-entry, an-entry, an->op, b-entry);
  return an;
}

tcell_t *value_condition(cell_t *a) {
  return a && is_value(a) && !is_var(a) ? a->value.var : NULL;
}

void add_conditions_2(cell_t *res, cell_t *a0) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v, value_condition(a0));
  }
}

void add_conditions_3(cell_t *res, cell_t *a0, cell_t *a1) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
                                  value_condition(a1)));
  }
}

void add_conditions_4(cell_t *res, cell_t *a0, cell_t *a1, cell_t *a2) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
             concatenate_conditions(value_condition(a1),
                                    value_condition(a2))));
  }
}

void add_conditions_5(cell_t *res, cell_t *a0, cell_t *a1, cell_t *a2, cell_t *a3) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
             concatenate_conditions(value_condition(a1),
               concatenate_conditions(value_condition(a2),
                                      value_condition(a3)))));
  }
}

void add_conditions_6(cell_t *res, cell_t *a0, cell_t *a1, cell_t *a2, cell_t *a3, cell_t *a4) {
  tcell_t **v = &res->value.var;
  if(!is_var(res)) {
    *v = concatenate_conditions(*v,
           concatenate_conditions(value_condition(a0),
             concatenate_conditions(value_condition(a1),
               concatenate_conditions(value_condition(a2),
                 concatenate_conditions(value_condition(a3),
                                        value_condition(a4))))));
  }
}

void add_conditions_from_array(cell_t *res, cell_t **a, unsigned int n) {
  tcell_t **v = &res->value.var;
  if(!is_var(res) && n) {
    tcell_t *cs = value_condition(a[n-1]);
    COUNTDOWN(i, n-1) {
      cs = concatenate_conditions(value_condition(a[i]), cs);
    }
    *v = concatenate_conditions(*v, cs);
  }
}

void add_conditions_var_2(cell_t *res, tcell_t *t) {
  tcell_t **v = &res->value.var;
  *v = concatenate_conditions(t, *v);
}

void add_conditions_var_3(cell_t *res, tcell_t *t, cell_t *a0) {
  tcell_t **v = &res->value.var;
  *v = concatenate_conditions(t,
         concatenate_conditions(value_condition(a0), *v));
}

#if INTERFACE
#define add_conditions(...) DISPATCH(add_conditions, __VA_ARGS__)
#define add_conditions_var(...) DISPATCH(add_conditions_var, __VA_ARGS__)
#endif

range_t get_range(cell_t *c) {
  assert_error(is_value(c));
  if(is_var(c)) {
    return c->value.range;
  } else {
    switch(c->value.type) {
    case T_INT:
      return RANGE(c->value.integer);
    case T_SYMBOL:
    case T_OPAQUE:
      return RANGE(c->value.symbol);
    default:
      return default_bound;
    }
  }
}

cell_t *trace_quote_var(tcell_t *entry, cell_t *l) {
  assert_error(l != NULL);
  if(is_empty_list(l)) return l;
  if(is_var(l)) return l;
  int x = trace_build_quote(entry, l, false);
  drop(l);
  return var_create_nonlist(T_LIST, &entry[x]);
}

// builds a temporary list of referenced variables
cell_t **trace_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp_val && tail != &c->tmp && !is_persistent(c)) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
      tail = trace_var_list(c->alt, tail);
    } else {
      c->tmp_val = true; // prevent loops
      TRAVERSE(c, alt, in, ptrs) {
        tail = trace_var_list(*p, tail);
      }
      c->tmp_val = false;
    }
  }
  return tail;
}

size_t tmp_list_length(cell_t *c) {
  size_t n = 0;
  FOLLOW(p, c, tmp) {
    n++;
  }
  return n;
}

TEST(var_count) {
  cell_t *l = lex("? [? +] [[?] dup] [[[[?]]]] ? dup", 0);
  const cell_t *p = l;
  cell_t *c = parse_expr(&p, NULL, NULL);
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  size_t n = tmp_list_length(vl);
  printf("length(vl) = %d\n", (int)n);
  clean_tmp(vl);
  drop(c);
  return n == 5 ? 0 : -1;
}

tcell_t *trace_alloc_var(tcell_t *entry, type_t t) {
  int x = trace_alloc(entry, 2);
  if(x <= 0) return NULL;
  tcell_t *tc = &entry[x];
  tc->op = OP_value;
  tc->value.type = t;
  tc->trace.type = t;
  tc->value.flags = VALUE_VAR;
  tc->var_index = ++entry->entry.in;
  return tc;
}

cell_t *param(int t, tcell_t *entry) {
  return var_create_nonlist(t, trace_alloc_var(entry, t));
}
