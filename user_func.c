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
#include <stdlib.h>
#include <stdio.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "primitive.h"
#include "special.h"
#include "byte_compile.h"
#include "trace.h"
#include "list.h"
#include "user_func.h"
#include "print.h"
#include "parse.h" // for string_printf
#include "tags.h"

#if INTERFACE
#define is_user_func(c) _is_user_func(GET_CELL(c))
#endif

bool _is_user_func(const cell_t *c) {
  return c->op == OP_exec;
}

static
cell_t *map_cell(tcell_t *entry, intptr_t x) {
  return x <= 0 ? NULL : entry[x].alt;
}

static
cell_t *get_return_arg(tcell_t *entry, tcell_t *returns, intptr_t x) {
  assert_error(x < entry->entry.out);
  trace_index_t i = tr_index(returns->value.ptr[x]);
  assert_error(i > 0);
  return map_cell(entry, i);
}

// given a list l with arity in -> out, produce an application of the list
// [X -> Y] => [X -> Y] apXY
cell_t *apply_list(cell_t *l, csize_t in, csize_t out) {
  cell_t *c = func(OP_ap, in + 1, out + 1);
  if(in) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(in - 1);
  } else {
    FLAG_CLEAR(*c, expr, NEEDS_ARG);
  }
  c->expr.arg[in] = l;
  RANGEUP(i, in+1, in+1+out) {
    c->expr.arg[i] = dep(c);
  }
  refn(c, out);
  return c;
}

// print a representation of a pattern for debugging
void print_pattern(cell_t *pattern) {
  if(is_var(pattern)) {
    printf(" ?%d", (int)var_index(NULL, pattern->value.var));
  } else if(is_list(pattern)) {
    csize_t in = function_in(pattern);
    cell_t **p;
    printf(" [");
    if(in) printf(" %d ->", in);
    FORLIST(p, pattern) {
      print_pattern(*p);
    }
    printf(" ]");
  } else {
    printf(" %c", 'A' + (char)((pattern-cells) % 26));
  }
}

// print the list of bindings for debugging
void print_bindings(cell_t *vl) {
  FOLLOW(p, q, vl, tmp) {
    csize_t x = var_index(NULL, p->value.var);
    printf("?%d = %d\n", x, (int)(q-cells));
  }
}

// print the word to match and its patterns
void print_word_pattern(cell_t *word) {
  printf("pattern:");
  COUNTUP(i, closure_in(word)) {
    print_pattern(word->expr.arg[i]);
  }
  printf("\n");
}

// build a binding list by applying the pattern to c
// TODO add reduction back in
cell_t **bind_pattern(tcell_t *entry, cell_t *c, cell_t *pattern, cell_t **tail) {
  assert_error(c);
  CONTEXT("bind_pattern %s %C %C @barrier", strfield(entry, word_name), c, pattern);
  if(!pattern || !tail) return NULL;
  if(c->tmp || c == *tail) return tail;
  if(c == pattern) {
    cell_t **t = tail;
    tail = trace_var_list(c, tail);
    FOLLOW(p, *t, tmp) {
      ref(p);
    }
    if(entry && is_var(pattern)) switch_entry(entry, pattern);
  } else if(is_var(pattern)) {
    // found a binding
    assert_error(!pattern->alt);
    assert_error(c->n != PERSISTENT);
    if(entry) {
      switch_entry(entry, pattern);
    } else {
      entry = var_entry(pattern->value.var);
    }
    LOG("bound %s[%d] = %C", entry->word_name, pattern->value.var-entry, c);
    LIST_ADD(tmp, tail, ref(c));
  } else if(is_list(pattern) && !is_empty_list(pattern)) {

    // push entry in
    if(pattern->pos) entry = trace_expr_entry(pattern->pos);

    if(is_list(c)) {
      assert_eq(list_size(c), list_size(pattern));
      COUNTUP(i, list_size(pattern)) {
        cell_t **a = &c->value.ptr[i];
        if(closure_is_ready(*a)) {
          simplify(a); // *** should this should be done at a higher level?
          LOG_WHEN((*a)->alt, MARK("WARN") " bind drops alt %C -> %C", *a, (*a)->alt);
        }
        tail = bind_pattern(entry,
                            *a,
                            pattern->value.ptr[i],
                            tail);
      }
    } else {
      csize_t
        in = function_in(pattern),
        out = function_out(pattern, false);
      if(out) {
        // this will rip the list apart (later)
        cell_t *l = apply_list(ref(c), in, out);
        drop(l); // don't care about the result

        list_iterator_t it = list_begin(pattern);
        COUNTDOWN(i, out) {
          cell_t **p = list_next(&it, false);
          assert_error(p);
          cell_t *d = l->expr.arg[in+1+i];
          tail = bind_pattern(entry, d, *p, tail);
          drop(d);
        }
      }
    }
  } else if(pattern->op == OP_id &&
            pattern->expr.alt_set == 0) {
    return bind_pattern(entry, c, pattern->expr.arg[0], tail);
  } else if(pattern->op == OP_value && (pattern->pos || entry)) { // HACK to move constants out
    assert_error(!is_list(pattern) || is_empty_list(pattern));
    if(!pattern->pos) {
      LOG(HACK " forcing pos for %C (%C) to %s", pattern, c, entry->word_name);
      pattern->pos = entry->pos; // HACKity HACK
    }
    force(&pattern);
    return bind_pattern(entry, c, pattern, tail);
  } else {
    // This can be caused by an operation before an infinite loop
    // This will prevent reducing the operation, and therefore fail to unify
    assert_error(false, "binding error: %C %C", c, pattern);
    // TODO promote to var instead of failing
    return NULL;
  }
  return tail;
}

static
response exec_list(cell_t **cp, context_t *ctx) {
  PRE_NO_CONTEXT(exec_list);

  if(ctx->t != T_LIST || closure_out(c) != 0 || !ctx->s.out) {
    return SUCCESS;
  }

  CONTEXT_LOG("exec_list %C", c);
  assert_error(ctx->s.out);
  cell_t *res;
  csize_t
    in = closure_in(c),
    out = ctx->s.out - 1,
    n = in + out;

  // if ctx->t == T_LIST, need to wrap here
  // move to func_exec; expand, wrap, and return the original function
  cell_t *nc = expand(c, out);
  nc->expr.out = out;
  res = make_list(ctx->s.out);
  res->value.ptr[out] = nc;
  COUNTUP(i, out) {
    cell_t *d = dep(ref(nc));
    int j = n - i;
    res->value.ptr[i] = d;
    nc->expr.arg[j] = d;
  }

  LOG(TODO " fix condition %C @condition", res);

  *cp = res;
  return RETRY;
}

// unify c with pattern pat if possible, returning the unified result
response unify_exec(cell_t **cp, tcell_t *parent_entry, context_t *ctx) {
  PRE(unify_exec, "#wrap");

  csize_t
    in = closure_in(c),
    out = closure_out(c);
  tcell_t *entry = (tcell_t *)c->expr.arg[in];
  cell_t *pat = entry->entry.wrap->initial;

  if(!pat) return FAIL;
  if(!FLAG(*c, expr, NO_UNIFY)) {
    assert_eq(in, closure_in(pat)); // TODO skip over non-changing args

    LOG_WHEN(out != 0, TODO " unify_convert %d: out(%d) != 0 @unify-multiout", c-cells, out);

    cell_t
      *vl = 0,
      **tail = &vl;
    COUNTUP(i, in) {
      simplify(&c->expr.arg[i]); // FIX this can cause arg flips
      tail = bind_pattern(NULL, c->expr.arg[i], pat->expr.arg[i], tail);
      if(!tail) {
        LOG("bind_pattern failed %C %C", c->expr.arg[i], pat->expr.arg[i]);
        break;
      }
    }

    if(!tail) {
      FOLLOW(p, vl, tmp) {
        drop(p);
      }
      clean_tmp(vl);
      return FAIL;
    }

    csize_t in = tmp_list_length(vl);
    cell_t *n = ALLOC(in + out + 1,
      .expr.out = out,
      .op = OP_exec
    );
    n->expr.arg[in] = (cell_t *)entry;
    FLAG_SET(*n, expr, RECURSIVE);
    FLAG_SET(*n, expr, NO_UNIFY);
    int pos = 0;

    // TODO this list should be sorted first by the parent variable pos's
    FOLLOW(p, vl, tmp) { // get arguments from the binding list
      n->expr.arg[in - 1 - pos] = p;
      pos++;
    }
    clean_tmp(vl);

    LOG("unified %s %C with initial_word in %s %C",
        entry->word_name, c,
        parent_entry->word_name, pat);
    LOG_WHEN(closure_out(c), TODO " handle deps in c = %C, n = %C", c, n);

    TRAVERSE(c, in) {
      drop(*p);
    }
    store_lazy_and_update_deps(cp, n, 0);
  }
  return exec_list(cp, ctx);
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

static
cell_t *exec_expand(cell_t *c) {
  size_t
    in = closure_in(c),
    out = closure_out(c),
    n = closure_args(c);
  tcell_t *entry = (tcell_t *)c->expr.arg[in];
  cell_t *res;
  tcell_t *returns = NULL;
  cell_t **results[out + 1];

  results[out] = &res;
  COUNTUP(i, out) {
    results[i] = &c->expr.arg[n - 1 - i]; // ***
  }

  CONTEXT("exec_expand %s: %C 0x%x", entry->word_name, c, c->expr.flags);

  assert_error(entry->entry.len && FLAG(*entry, entry, COMPLETE));
  trace_clear_alt(entry); // *** probably shouldn't need this

  COUNTUP(i, in) {
    tcell_t *p = &entry[i + 1];
    cell_t *a = c->expr.arg[REVI(i)];
    assert_error(is_var(p), "%d", i);
    if(p->n + 1 == 0) {
      drop(a);
    } else {
      p->alt = refn(a, p->n);
    }
  }

  // allocate, copy, and index
  FOR_TRACE(p, entry, in + 1) {
    int i = p - entry;
    if(!p->op) {
      p->alt = 0;
      continue; // skip empty cells TODO remove these
    }
    if(trace_type(p) == T_RETURN) {
      if(!returns) returns = p;
      continue;
    }
    tcell_t *e = is_user_func(p) ? get_entry(p) : NULL;
    cell_t *nc;
    csize_t p_in;
    if(e && e->entry.out == 1 &&
       (p_in = closure_in(p)) < e->entry.in) {
      // wrap incomplete function in a quote
      LOG("quote wrap %d", (int)i);
      nc = closure_alloc(e->entry.in + 1);
      memcpy(nc, &p->c, offsetof(cell_t, expr.arg));
      nc->size = e->entry.in + 1;
      csize_t remaining = e->entry.in - p_in;
      memcpy(&nc->expr.arg[remaining], p->expr.arg, p_in * sizeof(cell_t *));
      nc->expr.idx[0] = remaining - 1;
      nc->expr.arg[e->entry.in] = p->expr.arg[p_in];
      closure_set_ready(nc, false);
      nc = row_quote(nc);
    } else {
      nc = copy(&p->c);
      nc->expr.flags &= EXPR_TRACE; // only keep TRACE
      nc->n = p->n;
    }
    nc->tmp = 0;
    p->alt = nc;
  }

  // check that a return was found
  assert_error(returns);

  // rewrite pointers
  FOR_TRACE(p, entry, in + 1) {
    int i = p - entry;
    cell_t *t = map_cell(entry, i);
    if((is_value(p) && p->value.type == T_RETURN) || !t) continue;

    if(is_row_list(t)) t = t->value.ptr[0]; // for row quotes created above

    // skip rewriting for the entry argument
    tcell_t **t_entry = NULL;
    if(is_user_func(t)) {
      t_entry = (tcell_t **)&t->expr.arg[closure_in(t)];
      *t_entry = get_entry(t);
    }

    TRAVERSE(t, alt, args, ptrs) {
      if(*p) {
        trace_index_t x = tr_index(*p);
        *p = map_cell(entry, x);
      }
    }

    // TODO remove this
    if(t_entry &&
       *t_entry == entry) { // mark recursion
      FLAG_SET(*t, expr, RECURSIVE);
      //LOG("recursive exec %C -> %d", c, trace_ptr-trace_cur);
    }
  }

  // handle returns
  // HACK: only allocate alt ids when compiling
  bool tracing = trace_current_entry() != NULL || NOT_FLAG(*entry, entry, RECURSIVE);
  uint8_t alt_n = tracing ? int_log2(entry->entry.alts) : 0;
  uint8_t alt_id = new_alt_id(alt_n);
  unsigned int branch = 0;

  // first one
  alt_set_t alt_set = tracing ? as_multi(alt_id, alt_n, branch++) : 0;
  *results[out] = id(get_return_arg(entry, returns, out), alt_set);
  COUNTUP(i, out) {
    store_lazy_dep(*results[i], get_return_arg(entry, returns, i), alt_set);
  }

  // rest
  trace_index_t next = tr_index(returns->alt);
  while(next > 0) {
    alt_set_t as = tracing ? as_multi(alt_id, alt_n, branch++) : 0;
    returns = &entry[next];
    FOREACH(i, results) {
      cell_t *a = get_return_arg(entry, returns, i);
      if(*results[i]) {
        results[i] = &(*results[i])->alt;
        *results[i] = a ? id(a, as) : NULL;
      } else {
        drop(a);
      }
    }
    next = tr_index(returns->alt);
  }

  return res;
}

bool is_within_entry(tcell_t *entry, tcell_t *p) {
  return p > entry && p <= entry + entry->entry.len;
}

// builds a temporary list of referenced variables
cell_t **input_var_list(cell_t *c, cell_t **tail) {
  if(c && !c->tmp_val && tail != &c->tmp) {
    if(is_var(c) && !is_list(c)) {
      LIST_ADD(tmp, tail, c);
    } else {
      c->tmp_val = true; // prevent loops
      if(is_list(c)) {
        COUNTDOWN(i, list_size(c)) {
          tail = trace_var_list(c->value.ptr[i], tail);
        }
      } else {
        TRAVERSE(c, in) {
          tail = trace_var_list(*p, tail);
        }
      }
      c->tmp_val = false;
    }
  }
  return tail;
}

void vars_in_entry(cell_t **p, tcell_t *entry) {
  while(*p) {
    cell_t *v = *p;
    cell_t **next = &v->tmp;
    if(!var_for_entry(entry, v->value.var)) {
      // remove from list
      LOG("remove var %C", v);
      *p = *next;
      v->tmp = 0;
    } else {
      p = next;
    }
  }
}

void reassign_input_order(tcell_t *entry) {
  if(!entry->entry.wrap) return;
  cell_t *c = entry->entry.wrap->expand;
  set_ptr_tag(c, "wrap-&gt;expand");
  if(!c) return;
  tcell_t *parent_entry = entry->entry.parent;
  CONTEXT("reassign input order %C (%s -> %s)", c,
          parent_entry->word_name, entry->word_name);
  UNUSED csize_t in = entry->entry.in;
  cell_t *vl = 0;
  input_var_list(c, &vl);
  vars_in_entry(&vl, entry); // ***
  on_assert_error(tmp_list_length(vl) == in,
                  "%d != %d, %s %C @wrap",
                  tmp_list_length(vl), in, entry->word_name, c) {
    FOLLOW(p, vl, tmp) {
      LOG("input var %C", p);
    }
  }

  int pos = 1;
  FOLLOW(p, vl, tmp) {
    tcell_t *tn = var_for_entry(entry, p->value.var);
    assert_error(tn);
    assert_error(tn->pos, "%s[%d] (%C)",
                 entry->word_name, tn-entry, p);
    if(tn->pos != pos) {
      tn->pos = pos;
    }
    pos++;
  }
  clean_tmp(vl);
}

// this doesn't work for quotes
// all c's input args must be params
cell_t *flat_call(cell_t *c, tcell_t *entry) {
  tcell_t *parent_entry = entry->entry.parent;
  CONTEXT("flat call %C (%s -> %s)", c,
          parent_entry->word_name, entry->word_name);
  csize_t
    in = entry->entry.in,
    out = entry->entry.out;
  cell_t *nc = ALLOC(in + out,
    .expr.out = out - 1,
    .op = OP_exec
  );
  cell_t *vl = 0;
  input_var_list(c, &vl);
  assert_error(tmp_list_length(vl) == in,
               "%d != %d, %s @wrap",
               tmp_list_length(vl), in, entry->word_name);

  int pos = 1;
  FOLLOW(p, vl, tmp) {
    tcell_t *tn = get_var(entry, p);
    // Is it okay to update this from reassign_input_order?
    tn->pos = pos;
    // assert_error(tn->pos == pos, "%T (%C)", p->value.var, p);
    switch_entry(parent_entry, p);
    assert_error(entry_has(parent_entry, tn->value.var));
    cell_t *v = var_create_nonlist(T_ANY, tn->value.var);
    nc->expr.arg[in - pos] = v;
    LOG("arg[%d] -> %d", in - pos, tn->value.var - parent_entry);
    pos++;
  }
  clean_tmp(vl);
  nc->expr.arg[in] = (cell_t *)entry;
  return nc;
}

// TODO generalize these

// [[...] ...] -> [...]
static
cell_t *unwrap(cell_t *c, uintptr_t dep_mask, int out, int dropped) {
  int offset = list_size(c) - 1;

  // head
  //assert_error(is_list(c) && list_size(c) == 1, TODO " size = %d, %C", list_size(c), c);

  // N = out, ap0N swapN drop
  cell_t *l = make_list(out + offset - dropped);
  cell_t *ap = ready_func(OP_ap, 1, out + 1);
  LOG("unwrap %d %C [%C]", out, l, ap);
  COUNTUP(i, offset) {
    l->value.ptr[i] = ref(c->value.ptr[i]);
  }
  cell_t **p = &l->value.ptr[offset];
  COUNTDOWN(i, out) {
    cell_t **a = &ap->expr.arg[i + 1];
    if(dep_mask & (1 << i)) {
      *p++ = *a = dep(ref(ap));
    } else {
      *a = NULL;
    }
  }
  ap->expr.arg[0] = ref(c->value.ptr[offset]);
  drop(ap);
  drop(c);
  return l;
}

// [res dep_0 ... dep_out-1]
cell_t *wrap_vars(cell_t **res, cell_t *p, uintptr_t dep_mask, csize_t out) {
  cell_t *l = make_list(out);
  int offset = closure_args(p) - closure_out(p);
  cell_t **out_arg = &p->expr.arg[offset];
  cell_t *out_arg0 = NULL;
  LOG("wrap_vars %d %C", out, l);
  int dpos = offset;
  COUNTUP(i, out) {
    cell_t *d;
    if(dep_mask & (1 << i)) {
      if(out_arg0) {
        d = closure_alloc(1);
        store_dep(d, (*res)->value.var, dpos++, T_ANY, 0);
        *out_arg++ = d;
      } else {
        d = out_arg0 = *res;
      }
    } else {
      d = &fail_cell;
    }
    l->value.ptr[REVI(i)] = d;
  }
  assert_error(out_arg0);
  *res = l;
  return out_arg0;
}

// expand a user function into a list of outputs
static
cell_t *expand_list(cell_t *c) {
  size_t out = closure_out(c);
  cell_t *l = make_list(out + 1);
  int n = out;
  TRAVERSE(c, out) {
    *p = dep(c);
    l->value.ptr[--n] = *p;
  }
  refn(c, out);
  l->value.ptr[out] = exec_expand(c); // deps will be in c ***
  return l;
}

static
context_t *collect_ap_deps(context_t *ctx, cell_t **deps, int n) {
  while(n > 0) {
    const cell_t *c = ctx->src;
    if(c->op != OP_ap ||
       closure_in(c) != 1 ||
       c->n > count_out_used(c)) return NULL;
    int out = closure_out(c);
    int out_n = min(out, n);
    n -= out_n;
    memcpy(&deps[n],
           &c->expr.arg[closure_args(c) - out],
           out_n * sizeof(cell_t *));
    if(n > 0) ctx = ctx->up;
  }
  return ctx;
}

static
response func_exec_wrap(cell_t **cp, context_t *ctx, tcell_t *parent_entry) {
  csize_t
    in = closure_in(*cp),
    out = closure_out(*cp);
  tcell_t *entry = (tcell_t *)(*cp)->expr.arg[in];
  wrap_data wrap;
  PRE(exec_wrap, "%s 0x%x #wrap", entry->word_name, (*cp)->expr.flags);
  LOG_UNLESS(entry->entry.out == 1, "out = %d #unify-multiout", entry->entry.out);

  wrap.entry = entry;
  assert_error(ctx->s.out < sizeof(wrap.dep_mask) * 8);
  wrap.dep_mask = (1 << ctx->s.out) - 1;
  int dropped = 0;
  context_t *top = NULL;
  if(ctx->t == T_LIST) {
    cell_t *deps[ctx->s.out];
    top = collect_ap_deps(ctx->up, deps, ctx->s.out);
    if(top) {
      COUNTUP(i, ctx->s.out) {
        if(!deps[i]) {
          wrap.dep_mask &= ~(1 << i);
          dropped++;
          LOG("dropped ap %C, out = %d", c, i);
        }
      }
      LOG("dep_mask %x", wrap.dep_mask);
    }
  }

  tcell_t *new_entry = trace_start_entry(parent_entry, entry->entry.out);
  new_entry->entry.wrap = &wrap;
  new_entry->module_name = parent_entry->module_name;
  new_entry->word_name = string_printf("%s_r%d", parent_entry->word_name, parent_entry->entry.sub_id++);
  LOG("created entry for %s", new_entry->word_name);

  COUNTUP(i, in) {
    if(c->expr.arg[i]->op != OP_ap ||
       TWEAK(true, "to disable ap simplify %C", c->expr.arg[i]))
    simplify(&c->expr.arg[i]);
  }

  wrap.initial = ref(c);
  insert_root(&wrap.initial);
  move_changing_values(new_entry, c);

  // make a list with expanded outputs of c
  cell_t *nc = COPY_REF(c, in);
  mark_barriers(new_entry, nc);
  wrap.expand = nc;

  cell_t *l = expand_list(nc);

  // eliminate intermediate list
  if(ctx->t == T_LIST) {
    l = unwrap(l, wrap.dep_mask, ctx->s.out, dropped);
    new_entry->entry.out += ctx->s.out - 1 - dropped;
  }

  // IDEA break here?

  // TODO delay this to avoid quote creation
  TRAVERSE_REF(nc, in);
  insert_root(&nc);
  new_entry->entry.alts = trace_reduce(new_entry, &l);
  drop(l);
  remove_root(&nc);

  // IDEA split this up? {
  cell_t *p = flat_call(nc, new_entry);
  drop(nc);
  drop(c);

  if(top) {
    trace_drop_return(new_entry, ctx->s.out, wrap.dep_mask);
  }
  trace_final_pass(new_entry);
  trace_end_entry(new_entry);
  remove_root(&wrap.initial);
  drop(wrap.initial);

  trace_clear_alt(parent_entry);
  type_t rtypes[new_entry->entry.out];
  resolve_types(new_entry, rtypes);
  cell_t *res = var_create_with_entry(rtypes[0], parent_entry, p->size);
  // }

  tcell_t *tc = res->value.var;

  // build list expected by caller
  if(ctx->t == T_LIST) {
    trace_reduction(p, wrap_vars(&res, p, wrap.dep_mask, ctx->s.out));
  }

  // handle deps
  if(out) {
    // replace outputs with variables
    cell_t **c_out = &c->expr.arg[in + 1];
    COUNTUP(i, out) {
      cell_t *d = c_out[i];
      if(d && is_dep(d)) {
        assert_error(d->expr.arg[0] == c);
        drop(c);
        store_dep(d, tc, i + in + 1, rtypes[i+1], 0);
        p->expr.arg[in + 1 + i] = d;
      }
    }
  }

  *cp = p;
  add_conditions_from_array(res, p->expr.arg, in);
  store_reduced(cp, ctx, res);
  return SUCCESS;
}

static
response func_exec_trace(cell_t **cp, context_t *ctx, tcell_t *parent_entry) {
  size_t in = closure_in(*cp);
  tcell_t *entry = (tcell_t *)(*cp)->expr.arg[in];
  PRE(exec_trace, "%s 0x%x", entry->word_name, (*cp)->expr.flags);

  assert_error(parent_entry);

  cell_t *res;
  const size_t out = closure_out(c) + 1;
  const size_t entry_out = entry->entry.out;
  type_t rtypes[out];
  assert_error(out >= entry_out, "%d %d", out, entry_out);

  // reduce all inputs
  if(in) {
    csize_t n = 0;
    uint8_t in_types[in];
    memset(in_types, 0, in * sizeof(in_types[0]));
    reassign_input_order(entry);

    FOR_TRACE(p, entry) {
      if(is_var(p) && p->pos) {
        int i = in - p->pos;
        type_t t = p->value.type;
        if(t == T_LIST) t = T_ANY; // HACK, T_FUNCTION breaks things
        in_types[i] = t;
        if(t == T_OPAQUE) {
          CHECK_IF(reduce(&c->expr.arg[i],
                          WITH(&CTX(opaque, p->value.symbol), priority, PRIORITY_ASSERT - 1)) == FAIL, FAIL);
        } else {
          CHECK_IF(reduce(&c->expr.arg[i],
                          WITH(&CTX(t, t), priority, PRIORITY_ASSERT - 1)) == FAIL, FAIL);
        }
        if(++n >= in) break;
      }
    }

    COUNTUP(i, in) {
      CHECK(reduce_arg(c, i, &CTX(t, in_types[i])));
      CHECK_IF(as_conflict(ctx->alt_set), FAIL);
      cell_t *a = c->expr.arg[i];
      LOG_WHEN(a->alt, "split %C[%d] = %C #exec_split", c, i, a);
    }
  }
  CHECK_DELAY();

  // HACK force lists
  COUNTUP(i, in) {
    cell_t **ap = &c->expr.arg[i];
    cell_t *left;
    if(is_list(*ap) &&
       closure_is_ready(left = *leftmost(ap))) {
      LOG(HACK " forced cells[%C].expr.arg[%d]", c, i);
      // hacky, switches function var in placeholders
      if(is_placeholder(left)) {
        cell_t *f = left->expr.arg[closure_in(left) - 1];
        if(is_var(f)) {
          switch_entry(parent_entry, f);
        }
      }
      CHECK(func_list(ap, WITH(&CTX(return), priority, PRIORITY_TOP)));
      CHECK_DELAY();

      // ensure quotes are stored first
      *ap = trace_quote_var(parent_entry, *ap);
    }
  }

  resolve_types(entry, rtypes);
  wrap_data *wrap = entry->entry.wrap;
  uintptr_t mask = wrap ? wrap->dep_mask : 0;
  if(mask == 0) mask = (1 << entry_out) - 1;
  {
    int j = next_bit(&mask);
    assert_error(j >= 0, "not enough bits in dep_mask");
    type_t t = rtypes[j];
    if(ONEOF(t, T_ANY, T_BOTTOM) && ctx->t != T_ANY) {
      t = ctx->t;
    }
    res = var(t, c, parent_entry->pos);
  }

  // replace outputs with variables
  cell_t **c_out = &c->expr.arg[in + 1];
  COUNTUP(i, out-1) {
    cell_t *d = c_out[i];
    if(d && is_dep(d)) {
      assert_error(d->expr.arg[0] == c);
      drop(c);
      int j = next_bit(&mask);
      assert_error(j >= 0, "not enough bits in dep_mask; maybe dangling dep references? %C", d);
      type_t t = rtypes[j];
      store_dep(d, res->value.var, i + in + 1, t, ctx->alt_set); // TODO opaque symbol
      if(t == T_OPAQUE) { // opaque types must match in position, so get symbol from input
        FOR_TRACE_CONST(c, entry) { // TODO optimize this
          if(is_var(c) && c->pos == entry->entry.in - 1 - i) {
            d->value.symbol = c->value.symbol;
          }
        }
      }
    }
  }

  add_conditions_from_array(res, c->expr.arg, in);
  store_reduced(cp, ctx, res);
  return SUCCESS;

 abort:
  return abort_op(rsp, cp, ctx);
}

bool is_input(cell_t *v) {
  return is_var(v) && is_var(v->value.var);
}

bool all_dynamic(tcell_t *entry, cell_t *c) {
  int in = entry->entry.in;
  assert_error(in == closure_in(c));
  if(NOT_FLAG(*entry, entry, RECURSIVE)) {
    if(entry->entry.alts == 1) return false;
    COUNTUP(i, in) {
      if(ONEOF(entry[i+1].value.type, T_LIST, T_ANY)) {
        return false;
      }
    }
    return true;
  }
  COUNTUP(i, in) {
    if(NOT_FLAG(entry[i+1], trace, CHANGES)) {
      int a = REVI(i);
      force(&c->expr.arg[a]); // HACK
      if(!is_input(c->expr.arg[a])) {
        LOG("not dynamic: %C %s arg[%d] = %C", c, entry->word_name, a, c->expr.arg[a]);
        return false;
      }
    }
  }
  return true;
}

static tcell_t *substitute_entry(tcell_t **entry) {
  if(FLAG(**entry, entry, RECURSIVE)) {
    tcell_t *e = trace_wrap_entry(*entry);
    if(e) *entry = e;
  }
  return *entry;
}

OP(exec) {
  tcell_t *entry = substitute_entry((tcell_t **)&(*cp)->expr.arg[closure_in(*cp)]);
  PRE(exec, "%s", entry->word_name);

  tcell_t *parent_entry = trace_current_entry();

  if(NOT_FLAG(*entry, entry, COMPLETE)) {
    delay_branch(ctx, PRIORITY_DELAY);
    CHECK_PRIORITY(PRIORITY_EXEC_SELF);
    assert_error(parent_entry,
                 "incomplete entry can't be unified without "
                 "a parent entry %C @exec_split", c);
    if(entry->entry.wrap) {
      rsp = unify_exec(cp, parent_entry, ctx);
      if(rsp == FAIL) {
        LOG(MARK("WARN") " unify failed: %C %C",
            *cp, entry->entry.wrap->initial);
        ABORT(FAIL);
      }
    }
    return AND0(rsp, func_exec_trace(cp, ctx, parent_entry));
  } else if(parent_entry && all_dynamic(entry, c)) {
    return func_exec_trace(cp, ctx, parent_entry);
  } else if(parent_entry && FLAG(*entry, entry, RECURSIVE)) {
    return func_exec_wrap(cp, ctx, parent_entry);
  } else {
    assert_counter(1000);

    if(FLAG(*c, expr, RECURSIVE)) {
      TRAVERSE(c, in) {
        CHECK(force(p));
      }
      CHECK_DELAY();
    }
    cell_t *res = exec_expand(c);

    if(FLAG(*entry, entry, TRACE)) {
      printf(NOTE("TRACE") " %s.%s", entry->module_name, entry->word_name);
      TRAVERSE(c, in) {
        putchar(' ');
        show_one(*p);
      }
      printf("\n");
      if(break_on_trace) {
        LOG("break on trace: %C", c);
        breakpoint();
      }
    }
    store_lazy(cp, res, 0);
    return RETRY;
  }

 abort:
  return abort_op(rsp, cp, ctx);
}

void reduce_quote(cell_t **cp) {
  if(is_user_func(*cp) && closure_is_ready(*cp)) { // HACKy
    LOG("HACK reduce_quote[%C]", *cp);
    insert_root(cp);
    force(cp);
    remove_root(cp);
  }
}
