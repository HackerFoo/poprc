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

#include <stdlib.h>
#include <inttypes.h>
#include "rt_types.h"

#include "startle/error.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "ir/trace.h"
#include "list.h"
#include "builders.h"
#include "var.h"
#include "primitive/other.h"
#include "primitive/core.h"

   /*-----------------------------------------------,
    |          VARIABLE NAME CONVENTIONS            |
    |-----------------------------------------------|
    |                                               |
    |  cell_t *c = closure being reduced            |
    |  cell_t *d = first dep                        |
    |    (second closure returned, lower on stack)  |
    |  cell_t *e = second dep                       |
    |  cell_t *f = ...                              |
    |  cell_t *p = expr.arg[0], leftmost arg        |
    |  cell_t *q = expr.arg[1]                      |
    |  bool s = success                             |
    |  cell_t *res = result to be stored in c       |
    |  cell_t *res_X = result to be stored in X     |
    |                                               |
    '-----------------------------------------------*/

cell_t *set_alt(cell_t *c, alt_set_t as, cell_t *alt) {
  if(!c) return NULL;
  if(!as && !alt) return c;
  if(!c->n && !is_dep(c)) {
    if(!as) {
      c->alt = conc_alt(alt, c->alt);
      return c;
    } else if(is_value(c)) {
      c->alt = conc_alt(alt, c->alt);
      c->value.alt_set = as;
      return c;
    }
  }
  cell_t *i = build_id(c);
  i->expr.alt_set = as;
  i->alt = alt;
  return i;
}

WORD("|", alt, 2, 1)
OP(alt) {
  PRE(alt);
  alt_set_t as0 = 0, as1 = 0;
  if(!is_linear(ctx)) {
    uint8_t a = new_alt_id(1);
    as0 = as_single(a, 0);
    as1 = as_single(a, 1);
  }
  cell_t *r1 = set_alt(c->expr.arg[1], as1, c->alt);
  cell_t *r0 = set_alt(c->expr.arg[0], as0, r1);
  store_lazy(cp, r0, 0);
  return RETRY;
}

WORD("!", assert, 2, 1)
OP(assert) {
  PRE(assert);
  CHECK_PRIORITY(PRIORITY_ASSERT);

  cell_t *res = NULL;
  tcell_t *tc = NULL;
  cell_t *q = NULL;
  bool q_var = false;
  CHECK(reduce_arg(c, 1, &CTX(symbol, SYM_True)));
  if(rsp == SUCCESS) {
    q = c->expr.arg[1];
    q_var = is_var(q);

    if(!q_var && q->value.symbol != SYM_True) {
      LOG_WHEN(q->value.var, "symbolic assert fail %C", q);
      ABORT(FAIL);
    }

    if(q_var) {
      tc = trace_partial(OP_assert, 1, q);
    } else {
      reserve_condition(&q->value.var);
    }
  }
  CHECK(reduce_arg(c, 0, &CTX_UP));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();

  cell_t **p = &c->expr.arg[0];

  if(q_var && is_var(*p)) {
    res = var_create_bound(*p, tc, ctx);
    trace_arg(tc, 0, *p);
  } else {
    // handle is_var(*p)?
    res = take(p);
    unique(&res);
    drop(res->alt);
    add_conditions_var(res, tc, q);
  }

  FLAG_SET(*c, expr, PARTIAL);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

// for internal use
// very similar to assert
WORD("seq", seq, 2, 1)
OP(seq) {
  PRE(seq);

  cell_t *res = NULL;
  tcell_t *tc = NULL;
  cell_t *q = NULL;
  bool q_var = false;
  int pos = c->pos;
  CHECK(WITH(x, &CTX(any),
             x->flags |= CONTEXT_SEQ,
             reduce_arg(c, 1, x))); // don't split arg here?
  if(rsp == SUCCESS) {
    q = c->expr.arg[1];
    q_var = is_var(q);

    if(q_var) {
      tc = trace_partial(OP_seq, 1, q); // drop on abort?
    } else {
      if(q->value.var) {
        reserve_condition(&q->value.var);
      } else if(is_empty_list(q) && !ctx->alt_set) {
        *cp = CUT(c, expr.arg[0]);
        if(!is_persistent(*cp)) mark_pos(*cp, pos);
        return RETRY;
      }
    }
  }
  CHECK(reduce_arg(c, 0, &CTX_UP));
  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();

  cell_t **p = &c->expr.arg[0];

  if(q_var && is_var(*p)) {
    if(q->value.var == (*p)->value.var) {
      res = build_id(ref(*p));
      res->alt = ref(c->alt);
      drop(c);
      *cp = res;
      ABORT(RETRY);
    }
    res = var_create_bound(*p, tc, ctx);
    trace_arg(tc, 0, *p);
  } else {
    res = take(p);
    unique(&res);
    drop(res->alt);
    add_conditions_var(res, tc, q);
  }

  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

// merge two unless's into a seq
// X Y Z unless unless --> X Z seq
// ** what if Y fails?
bool rule_merge_unless(context_t *ctx) {
  if(ctx->flags & CONTEXT_INV) {
    cell_t *c = *ctx->src;
    cell_t *p = *ctx->up->src;
    if(p && p->op == OP_unless && p->expr.arg[1] == c) {
      LOG("rule match: merge_unless %C %C", p, c);
      p->op = OP_seq;
      //p->expr.arg[1] = p->expr.arg[0];
      p->expr.arg[1] = ref(c->expr.arg[1]);
      drop(c);
      ctx->flags |= CONTEXT_RETRY;
      return true;
    }
  }
  return false;
}

// very similar to assert
// TODO merge common code
WORD("unless", unless, 2, 1)
OP(unless) {
  PRE(unless);
  RULE(merge_unless);
  CHECK_PRIORITY(PRIORITY_UNLESS);

  cell_t *res = NULL;
  tcell_t *tc = NULL, *tp = NULL;

  // reduce each alt
  cell_t **p = &c->expr.arg[0];
  cell_t **q = &c->expr.arg[1];
  while(*q) {
    response rsp0 = WITH(x, &CTX(any),
                         x->flags ^= CONTEXT_INV,
                         reduce(q, x));
    CHECK_IF(rsp0 == RETRY, RETRY);
    if(rsp0 == DELAY) {
      rsp = DELAY;
    } else {
      CHECK_IF(!is_var(*q) &&
               !(*q)->value.var &&
               rsp0 != FAIL, FAIL);
      if(rsp0 != FAIL) {
        tc = concatenate_conditions(trace_partial(OP_unless, 1, *q), tc);
        if(!tp) tp = tc;
      }
    }

    q = &(*q)->alt;
  }

  CHECK(reduce_arg(c, 0, &CTX_UP));
  CHECK_DELAY();

  if(tc && is_var(*p)) {
    res = var_create_bound(*p, tc, ctx);
    trace_arg(tp, 0, *p);
  } else {
    // handle is_var(*p)?
    res = take(p);
    unique(&res);
    drop(res->alt);
    add_conditions_var(res, tc);
  }

  FLAG_SET(*c, expr, PARTIAL);
  store_reduced(cp, ctx, res);
  return SUCCESS;

abort:
  return abort_op(rsp, cp, ctx);
}

WORD("otherwise", otherwise, 2, 1)
OP(otherwise) {
  cell_t *c = *cp;
  cell_t *p = c->expr.arg[0];
  c->expr.arg[0] = c->expr.arg[1];
  c->expr.arg[1] = p;
  c->op = OP_unless;
  return RETRY;
}

WORD("id", id, 1, 1)
OP(id) {
  PRE(id);
  ctx->alt_set = c->expr.alt_set;
  int pos = c->pos;

  if(ctx->alt_set || c->alt) {
    CHECK(reduce_arg(c, 0, &CTX_UP));
    CHECK_IF(as_conflict(ctx->alt_set), FAIL);
    CHECK_DELAY();

    cell_t *res = ref(c->expr.arg[0]);
    mark_pos(res, pos);
    store_reduced(cp, ctx, res);
    return SUCCESS;
  } else {
    *cp = CUT(c, expr.arg[0]);
    if(!is_persistent(*cp)) mark_pos(*cp, pos);
    return RETRY;
  }

 abort:
  return abort_op(rsp, cp, ctx);
}

WORD("drop", drop, 2, 1)
OP(drop) {
  PRE(drop);
  drop(c->expr.arg[1]);
  c->op = OP_id;
  c->size = 1;
  c->expr.alt_set = 0;
  return RETRY;
}

WORD("swap", swap, 2, 2)
OP(swap) {
  PRE(swap);
  int pos = c->pos;
  store_lazy_dep(c->expr.arg[2],
                 c->expr.arg[0], 0);
  store_lazy(cp, c->expr.arg[1], 0);
  mark_pos(*cp, pos);
  return RETRY;
}

// for testing
WORD("delay", delay, 1, 1)
OP(delay) {
  PRE(delay);
  CHECK_PRIORITY(PRIORITY_DELAY);
  store_lazy(cp, c->expr.arg[0], 0);
  return RETRY;

abort:
  return abort_op(rsp, cp, ctx);
}

cell_t *id(cell_t *c, alt_set_t as) {
  if(!c) return NULL;
  cell_t *i = build_id(c);
  i->expr.alt_set = as;
  return i;
}

WORD("dup", dup, 1, 2)
OP(dup) {
  PRE(dup);
  cell_t *d = c->expr.arg[1];
  store_lazy_dep(d, ref(c->expr.arg[0]), 0);
  store_lazy(cp, c->expr.arg[0], 0);
  return RETRY;
}

static
response reduce_rows(cell_t *l, csize_t out, context_t *ctx) {
  response rsp = SUCCESS;
  if(is_list(l)) {
    insert_root(&l);
    list_iterator_t it = list_begin(l);
    { // reduce rows as much as needed and check for delays
      COUNTUP(i, out) {
        rsp = reduce_row(&it, out - i, ctx);
        if(rsp != SUCCESS) break;
        list_next(&it, false);
      }
    }
  }
  remove_root(&l);
  return rsp;
}

// L M... R compMN --->
// L [M...] . R . ap0N ---> (pushr)
// [L: M...] R . ap0N ---> (actual implementation)
// [L: M... R ?? -> N...] ap0N ---> (placeholder)
// [L: M... R ??] N...
// Notes:
//  - ?? mirrors ap/comp
//  - ?? remains in the list, only deps are removed
//  - will this handle incomplete L?
static
response func_compose_ap(cell_t **cp, context_t *ctx, bool row) {
  CONTEXT("%s: %C", row ? "compose" : "ap", *cp);
  PRE_NO_CONTEXT(compose_ap);

  const csize_t
    in = closure_in(c) - 1,
    arg_in = in - row,
    n = closure_args(c),
    out = closure_out(c);

  assert_error(!row || in);

  cell_t *p = NULL;
  cell_t *res = NULL;
  cell_t *l = NULL;
  int pos = c->pos;

  CHECK_IF(!check_type(ctx->t, T_LIST), FAIL);

  // conservative guesses for the sizes of `a` and `b`
  int ctx_out = pos ? 0 : ctx->s.out;
  qsize_t
    cs = {ctx->s.in, ctx_out + out},
    bs = {(row ? 0 : cs.in) + arg_in, cs.out},
    as = {row ? cs.in : 0, 0};

  if(row && is_list(c->expr.arg[0])) { // ***
    as = quote_size(c->expr.arg[0], true);
    bs = compose_size_b(arg_in, as, cs);
  }

  CHECK(WITH(x, &CTX(list,
                     out ? arg_in : bs.in, // only account for known inputs when there are outputs
                     bs.out),
             x->flags &= ~CONTEXT_REDUCE_LISTS,
             reduce_arg(c, in, x)));
  bs = quote_size(c->expr.arg[in], false);
  as = row ? compose_size_a(arg_in, bs, cs) : (qsize_t) {0, 0};

  if(row) {
    if(!arg_in && is_nil(c->expr.arg[in])) {
      CHECK_DELAY();
      *cp = build_id(ref(c->expr.arg[0]));
      (*cp)->alt = ref(c->alt);
      drop(c);
      return RETRY;
    } else if(as.in || as.out || is_value(c->expr.arg[0])) {
      CHECK(reduce_arg(c, 0, &CTX(list, as.in, as.out)));
      CHECK_DELAY();
      p = c->expr.arg[0];
      as = quote_size(p, false);
    }
  }

  CHECK_IF(as_conflict(ctx->alt_set), FAIL);
  CHECK_DELAY();

  if(p) {
    placeholder_extend(&c->expr.arg[0], compose_size_a(arg_in, bs, cs), false);
    p = c->expr.arg[0];
    as = quote_size(p, true); // *** why?
  }
  cell_t **q = &c->expr.arg[in];
  placeholder_extend(q, compose_size_b(arg_in, as, cs), row);
  bs = quote_size(*q, false);
  cs = compose_size_c(arg_in, as, bs);

  assert_error(!is_var(*q));

  reverse_ptrs((void **)c->expr.arg, in);
  list_iterator_t it = {
    .array = c->expr.arg,
    .size = in - row,
    .row = row
  };

  insert_root(q);
  l = compose(it, ref(*q)); // TODO prevent leaking outside variables
  reverse_ptrs((void **)c->expr.arg, in);

  CHECK(reduce_rows(l, out, ctx));
  CHECK_DELAY();

  it = list_begin(l);
  {
    list_iterator_t end = it;
    LOOP(out) list_next(&end, false);
    res = list_rest(end);
    if(res->alt) res = id_list(res); // ***
  }

  cell_t **x;
  COUNTUP(i, out) {
    x = list_next(&it, false);
    cell_t *d = c->expr.arg[n-1-i];
    if(d) {
      if(!x) {
        LOG("null quote output: arg[%d] = %C", n-1-i, d);
        assert_error(c == d->expr.arg[0]);
        drop(c);
        store_fail(d, d->alt, ctx);
      } else {
        mark_pos(*x, pos);
        cell_t *seq_x = build_seq(ref(*x), ref(res));
        store_lazy_dep(d, seq_x, ctx->alt_set);
        LOG_WHEN(res->alt, "popr from alt quote %C <- %C #condition", d, seq_x);
      }
    }
  }
  if(pos &&
     (x = list_next(&it, true)) &&
     is_row_arg(&it)) {
    mark_pos(*x, pos);
  }
  remove_root(q);

  drop(l);
  if(out) FLAG_SET(*c, expr, PARTIAL);
  add_conditions(res, p, *q);
  if(res->value.var) { // ***
    tcell_t *v = res->value.var;
    if(v && v->trace.type != T_ANY) {
      LOG("removing type from %T, res = %C #type", v, res);
      v->trace.type = T_ANY;
      v->trace.range = RANGE_NONE;
    }
  }
  store_reduced(cp, ctx, res);
  ASSERT_REF();
  return SUCCESS;

 abort:
  drop(l);
  drop(res);
  return abort_op(rsp, cp, ctx);
}

WORD_ALIAS("pushl", ap, 2, 1, pushl, SYNC)
WORD_ALIAS("popr", ap, 1, 2, popr, SYNC)
OP(ap) {
  if(closure_args(*cp) == 1) {
    return func_type(cp, ctx, T_LIST);
  } else {
    return func_compose_ap(cp, ctx, false);
  }
}

WORD(".", compose, 2, 1, SYNC)
OP(compose) {
  return func_compose_ap(cp, ctx, true);
}

static
cell_t *expand_nil(cell_t *c) {
  int n = c->n;
  c->n = 0;
  c = expand(c, 1);
  c->n = n;
  c->expr.arg[closure_args(c) - 1] = empty_list();
  return c;
}

WORD("quote", quote, 1, 1, SYNC)
OP(quote) {
  (*cp)->op = OP_ap;
  *cp = expand_nil(*cp);
  return RETRY;
}

WORD("pushr", pushr, 2, 1, SYNC)
OP(pushr) {
  (*cp)->op = OP_compose;
  *cp = expand_nil(*cp);
  return RETRY;
}

/* Local Variables: */
/* eval: (add-to-list 'imenu-generic-expression '("Operator" "^.*OP(\\([a-z_]+\\)).*$" 1)) */
/* End: */
