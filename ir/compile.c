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
#include <stdlib.h>
#include <inttypes.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "eval.h"
#include "special.h"
#include "ir/compile.h"
#include "parse/parse.h"
#include "debug/print.h"
#include "parse/lex.h"
#include "module.h"
#include "user_func.h"
#include "list.h"
#include "ir/trace.h"
#include "var.h"
#include "ir/analysis.h"
#include "parameters.h"

static void print_value(const cell_t *c) {
  switch(c->value.type) {
  case T_INT:
    printf(" val %" PRIdPTR, c->value.integer);
    break;
  case T_SYMBOL: {
    val_t x = c->value.symbol;
    const char *str = symbol_string(x);
    if(!str) str = "??";
    printf(" val %s", str);
    break;
  }
  case T_FLOAT:
    printf(" val %g", c->value.flt);
    break;
  case T_STRING:
    printf(" val \"");
    print_escaped_string(value_seg(c), false);
    printf("\"");
    break;
  default:
    printf(" val ??");
    break;
  }
}

void print_bound(tcell_t *tc) {
  type_t t = trace_type(tc);
  range_t r = tc->trace.range;
  if(t == T_INT) {
    if(range_empty(r)) {
      printf(" is empty");
    } else if(range_singleton(r)) {
      printf(" is %" PRIdPTR, r.min);
    } else if(range_bounded(r)) {
      printf(" in [%" PRIdPTR ", %" PRIdPTR "]",
             r.min, r.max);
    } else if(range_has_lower_bound(r)) {
      printf(" >= %" PRIdPTR, r.min);
    } else if(range_has_upper_bound(r)) {
      printf(" <= %" PRIdPTR, r.max);
    }
  } else if(t == T_SYMBOL) {
    if(range_singleton(r)) {
      printf(" is %s", symbol_string(tc->trace.range.min));
    }
  } else if(t == T_LIST) {
    if(tc->trace.bit_width) {
      printf(" %db", tc->trace.bit_width);
    }
  } else if(t == T_OPAQUE) {
    printf(" is %s", symbol_string(tc->trace.range.min));
    if(tc->trace.range.min == SYM_Array &&
       tc->trace.addr_width && tc->trace.bit_width) {
      printf(" %da %db", tc->trace.addr_width, tc->trace.bit_width);
    }
  }
}

// print bytecode for entry e
void print_bytecode(tcell_t *entry, bool tags) {
  // word info (top line)
  printf("___ %s.%s (%d -> %d)",
         entry->module_name, entry->word_name,
         entry->entry.in, entry->entry.out);
  if(entry->entry.alts != 1) {
    if(entry->entry.alts == 0) {
      printf(" FAIL");
    } else {
      printf(" x%d", entry->entry.alts);
    }
  }
  if(FLAG(*entry, entry, MUTUAL)) {
    printf(" mut");
  }
  if(FLAG(*entry, entry, RECURSIVE)) {
    printf(" rec");
  }
  if(FLAG(*entry, entry, ROW)) {
    printf(" row");
  }
  if(tags) {
    tag_t tag;
    write_tag(tag, entry->trace.hash);
    printf(" " FADE("(hash: " FORMAT_TAG ")"), tag);
  }

  printf(" ___\n");

  // body
  FOR_TRACE(tc, entry) {
    int t = tc - entry;
    cell_t *c = &tc->c;
    printf("[%d]", t);
    if(!c->op) {
      printf("\n");
      continue;
    }
    if(is_value(c)) {
      bool is_ret = c->value.type == T_RETURN;
      if(is_list(c) || is_ret) { // return or list
        if(is_ret) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          cell_t *p = c->value.ptr[i];
          printf("%s%d", tr_flags(p, TR_FINAL) ? "" : "&", tr_index(p));
          if(i) printf(" ");
        }
        printf("]");
      } else if(is_var(c)) { // variable
        if(FLAG(*tc, trace, CHANGES)) printf(" changing");
        printf(" var");
        print_bound(tc);
      } else { // value
        print_value(c);
      }
      if(!is_ret) printf(" :: %s", show_type_all_short(c));
      if(is_ret && c->alt) {
        printf(" -> %d", tr_index(c->alt));
      }
      if(!is_ret) printf(" x%d", c->n + 1);
    } else { // print a call
      const char *module_name = NULL, *word_name = NULL;
      if(FLAG(*tc, trace, JUMP)) {
        printf(" jump");
      }
      if(NOT_FLAG(*tc, trace, INCOMPLETE)) {
        trace_get_name(tc, &module_name, &word_name);
        printf(" %s.%s", module_name, word_name);
      } else {
        printf(" incomplete %s", op_name(c->op));
      }
      TRAVERSE(c, in) {
        int x = tr_index(*p);
        if(x == 0) {
          printf(" X");
        } else {
          printf(" %s%d", is_dep(c) || tr_flags(*p, TR_FINAL) ? "" : "&", x);
        }
      }
      if(closure_out(c)) {
        printf(" ->");
        TRAVERSE(c, out) {
          int x = tr_index(*p);
          if(x == 0) {
            printf(" X");
          } else {
            printf(" %d", x);
          }
        }
      }
      print_bound(tc);
      printf(" :: %c", type_char(tc->trace.type));
      if(FLAG(*c, expr, PARTIAL)) printf("?");
      printf(" x%d", c->n + 1);
    }
    if(FLAG(*tc, trace, NO_SKIP)) printf(".");
    if(!is_value(c) && FLAG(*c, expr, TRACE)) {
      printf(" [TRACING]");
    }
    if(t > entry->entry.in &&
       c->n + 1 == 0) {
      printf(" <-- WARNING: zero refcount");
    } else if(is_dep(c) && !c->expr.arg[0]) {
      printf(" <-- WARNING: broken dep");
    }
    if(tags) {
      tag_t tag;
      write_tag(tag, tc->trace.hash);
      printf(" " FADE(FORMAT_TAG) "\n", tag);
    } else {
      printf("\n");
    }
  }

  // print sub-functions once
  FOR_TRACE(tc, entry) {
    if(is_user_func(tc)) {
      tcell_t *e = get_entry(tc);
      if(e->entry.parent == entry && !e->op) {
        printf("\n");
        print_bytecode(e, tags);
        e->op = OP_value;
      }
    }
  }
  FOR_TRACE(tc, entry) {
    if(is_user_func(tc)) {
      tcell_t *e = get_entry(tc);
      e->op = OP_null;
    }
  }
}

// to call from a debugger
void print_bytecode_from_entry_number(int n) {
  print_bytecode(&trace_cells[n], true);
}

// drop an expression from the trace
void drop_trace(tcell_t *entry, tcell_t *tc) {
  if(tc->n <= 0) {
    LOG("drop %s[%d] %O", entry->word_name, tc-entry, tc->op);
    if(tc->op) {
      TRAVERSE(tc, in, ptrs) {
        int x = tr_index(*p);
        if(x > 0) {
          drop_trace(entry, &entry[x]);
        }
      }
    }
    if(is_var(tc)) {
      tc->n = ~0;
    } else {
      tc->op = OP_null;
    }
  } else {
    tc->n--;
  }
}

// remove unreferenced expressions from the entry
static
void condense(tcell_t *entry) {
  if(entry->entry.len == 0) return;
  tcell_t *ret = NULL;
  csize_t in = entry->entry.in;
  tcell_t *vars = get_trace_ptr(in);
  int nvars = 0;
  int idx = 1 + in;

  // drop unreferenced expressions
  FOR_TRACE(tc, entry) {
    if(tc->n < 0 && !(is_var(tc) && tc->var_index)) {
      drop_trace(entry, tc);
    }
  }

  // calculate mapping
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      if(is_var(tc)) {
        // move variables out into temporary space
        assert_error(tc->var_index);
        nvars++;
        memcpy(&vars[tc->var_index-1], tc, sizeof(tcell_t));
        tc->op = OP_null;
        tc->alt = index_tr(tc->var_index);
        LOG_WHEN(tc->var_index != tc-entry, "move var %d -> %d", tc-entry, tc->var_index);
      } else if(is_return(tc)) {
        if(ret) ret->alt = index_tr(idx);
        ret = tc;
        idx += calculate_tcells(tc->size);
      } else {
        tc->alt = index_tr(idx);
        idx += calculate_tcells(tc->size);
      }
    } else {
      LOG("collapse %d", tc-entry);
    }
  }

  // update switch map (for propagating types)
  map_clear(switch_rev_map);
  FORMAP(i, switch_map) {
    tcell_t *key = (tcell_t *)switch_map[i].first;
    tcell_t **val = (tcell_t **)&switch_map[i].second;
    if(entry_has(entry, *val)) *val = &entry[(*val)->var_index];
    map_insert(switch_rev_map, PAIR(*val, key));
  }

  csize_t len = idx - 1;
  assert_error(nvars == entry->entry.in);

  // update references
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      TRAVERSE(tc, args, ptrs) {
        int x = tr_index(*p);
        if(x > 0) {
          tr_set_index(p, tr_index(entry[x].alt));
        }
      }
    }
  }
  // condense
  idx = 1;
  FOR_TRACE(p, entry) {
    if(p->op) {
      csize_t s = calculate_tcells(p->size);
      if(!(is_value(p) && p->value.type == T_RETURN)) {
        p->alt = NULL;
      }
      if(idx < p - entry) {
        tcell_t *n = &entry[idx];
        memmove(n, p, s * sizeof(tcell_t));
        memset(n + s, 0, (p - n) * sizeof(tcell_t)); // *** keeps zeroing p - n
        p = n;
      }
      idx += s;
    }
  }

  // prepend vars
  memmove(&entry[in + 1], &entry[1], (len - in) * sizeof(tcell_t));
  memcpy(&entry[1], vars, in * sizeof(tcell_t));
  memset(vars, 0, in * sizeof(tcell_t));
  entry->entry.len = len;
}

static
void trace_replace_arg(tcell_t *entry, int old, int new) {
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      TRAVERSE(tc, args, ptrs) {
        if(tr_index(*p) == old) *p = index_tr(new);
      }
    }
  }
}

// remove unused outputs
static
void shift_out(cell_t *c, uintptr_t dep_mask) {
  int out = closure_out(c);
  int n = closure_args(c);
  int j = 0;
  cell_t **out_arg = &c->expr.arg[n - out];
  COUNTUP(i, out) {
    cell_t *t = out_arg[i];
    if(dep_mask & (1 << (i + 1))) {
      out_arg[j++] = t;
    }
  }
  while(j < out) {
    out_arg[j++] = 0;
    c->size--;
    c->expr.out--;
  }
}

// remove unused outputs in a recursive function
void trace_drop_return(tcell_t *entry, int out, uintptr_t dep_mask) {
  int used = 0;
  FORMASK(i, j, dep_mask) {
    used++;
  }
  if(used == out) return;
  FOR_TRACE(p, entry) {
    if(p->op == OP_exec && get_entry(p) == entry) {
      shift_out(&p->c, dep_mask);
    }
  }
  entry->entry.out = used;
}

// condense and run all analyses on the entry
static
void condense_and_analyze(tcell_t *entry) {
  condense(entry);
  last_use_analysis(entry);
  no_skip_analysis(entry);
  mark_jumps(entry);

  // mark first_return
  FOR_TRACE(tc, entry) {
    if(is_return(tc)) {
      entry->trace.first_return = tc - entry;
      break;
    }
  }
  LOOP(2) { // run twice to for self calls
    calculate_bit_width(entry);
  }
}

// runs after reduction to finish functions marked incomplete
void trace_final_pass(tcell_t *entry) {
  // replace alts with trace cells
  tcell_t *prev = NULL;

  // propagate types to asserts ***
  FOR_TRACE(p, entry) {
    if(p->op == OP_assert &&
       p->trace.type == T_ANY) {
      p->trace.type = entry[tr_index(p->expr.arg[0])].trace.type;
      p->trace.range = entry[tr_index(p->expr.arg[0])].trace.range;
    }
  }

  // Placeholder Conversion
  // Placeholders are converted in ap/compose on list variables.
  // Rules:
  //   ?x* ... ?y placeholder  ===>  ?x ... ?y compose
  //   ?x* ... [] placeholder  ===>  ?x ...    pushr (special case of compose)
  //       ...    placeholder  ===>     ...    ap
  // where ?x* is indicated with EXPR_ROW set on placeholder
  FOR_TRACE(tc, entry) {
    cell_t *p = &tc->c;
    if(FLAG(*tc, trace, INCOMPLETE)) {
      if(p->op == OP_placeholder) { // convert a placeholder
        FLAG_CLEAR(*tc, trace, INCOMPLETE);
        if(FLAG(*p, expr, ROW)) { // to compose
          assert_error(closure_in(p) > 1);
          cell_t **right_arg = &p->expr.arg[closure_in(p)-1];
          cell_t *l = &entry[tr_index(*right_arg)].c;
          if(is_nil(l)) { // ?x ?y [] compose -> ?x ?y pushr
            p->op = OP_pushr;
            l->n--;
            p->size--;
            *right_arg = NULL;
          } else {
            p->op = OP_compose;
          }
        } else { // to ap
          p->op = OP_ap;
        }
        if(prev && prev->op == OP_ap &&
           tr_index(p->expr.arg[closure_in(p) - 1]) == prev - entry &&
           prev->n == 0) {
          LOG("merging ap %d to %d", tc - entry, prev - entry);
          csize_t
            p_in = closure_in(p),
            p_out = closure_out(p),
            p_size = closure_args(p);
          refcount_t p_n = p->n;
          cell_t *tmp = copy(p);
          trace_replace_arg(entry, tc - entry, prev - entry);
          prev->op = p->op;
          memset(p, 0, calculate_cells(p_size) * sizeof(cell_t));
          ARRAY_SHIFTR(prev->expr.arg[0], p_in-1, prev->size);
          ARRAY_COPY(prev->expr.arg[0], tmp->expr.arg[0], p_in-1);
          ARRAY_COPY(prev->expr.arg[prev->size + p_in-1], tmp->expr.arg[p_in], p_out);
          prev->size += p_size - 1;
          prev->expr.out += p_out;
          prev->n = p_n;
          closure_free(tmp);
        }
      }
    }
    prev = tc;
  }
  if(TWEAK(true, "to disable condense/move_vars in %s", entry->word_name)) {
    condense_and_analyze(entry);
  }
}

// add an entry and all sub-entries to a module
static
void store_entries(tcell_t *entry, cell_t *module) {
  assert_error(module_name(module) == entry->module_name);
  module_set(module, string_seg(entry->word_name), &entry->c);
  FOR_TRACE(tc, entry) {
    if(tc->op != OP_exec) continue;
    tcell_t *sub_e = get_entry(tc);
    if(sub_e->entry.parent == entry) store_entries(sub_e, module);
  }
}

static
cell_t *compile_entry(seg_t name, cell_t *module) {
  csize_t in, out;
  cell_t *l = implicit_lookup(name, module);
  if(!is_list(l)) return l;
  cell_t *ctx = get_module(string_seg(l->module_name)); // switch context module for words from other modules
  if(pre_compile_word(l, ctx, &in, &out) &&
     compile_word(&l, name, ctx, in, out)) {
    store_entries(tcell_entry(l), ctx);
    return l;
  } else {
    return NULL;
  }
}

void compile_module(cell_t *module) {
  map_t map = module->value.map;
  FORMAP(i, map) {
    pair_t *x = &map[i];
    char *name = (char *)x->first;
    if(strcmp(name, "imports") != 0) {
      compile_entry(string_seg(name), module);
    }
  }
}

// lookup an entry and compile if needed
cell_t *module_lookup_compiled(seg_t path, cell_t **context) {
  return compile_def(module_lookup(path, context), path, context);
}

// compile a definition
cell_t *compile_def(cell_t *p, seg_t path, cell_t **context) {
  if(!p) return NULL;
  if(!is_list(p)) return p;
  if(FLAG(*p, value, TRACED)) {
    if(p->alt) { // HACKy
      return p->alt;
    } else {
      return lookup_word(string_seg("??"));
    }
  }
  FLAG_SET(*p, value, TRACED);
  seg_t name = path_name(path);
  cell_t *res = compile_entry(name, *context);
  if(!res) FLAG_CLEAR(*p, value, TRACED);
  return res;
}

// compile lexed source (rest) with given name and store in the eval module
cell_t *parse_eval_def(seg_t name, cell_t *rest) {
  cell_t *eval_module = module_get_or_create(modules, string_seg("eval"));
  cell_t *l = quote(rest);
  l->module_name = "eval";
  module_set(eval_module, name, l);
  return module_lookup_compiled(name, &eval_module);
}

// prepare for compilation of a word
bool pre_compile_word(cell_t *l, cell_t *module, csize_t *in, csize_t *out) {
  cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1
  // arity (HACKy)
  // must parse twice, once for arity, and then reparse with new entry
  // also compiles dependencies
  // TODO make mutual recursive words compile correctly
  bool res = get_arity(toks, in, out, module);
  return res;
}


// compile one user word (function)
bool compile_word(cell_t **entry, seg_t name, cell_t *module, csize_t in, csize_t out) {
  cell_t *l;
  if(!entry || !(l = *entry)) return false;
  if(!is_list(l)) return true;
  if(is_empty_list(l)) return false;

  const cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1

  // set up
  rt_init();
  trace_init();
  tcell_t *e = trace_start_entry(NULL, out);
  e->entry.in = in;
  // make recursive return this entry
  (*entry)->alt = &e->c; // ***
  *entry = &e->c;

  e->module_name = module_name(module);
  e->word_name = seg_string(name); // TODO fix unnecessary alloc
  e->src = src_text(toks);
  CONTEXT_LOG("compiling %s", e->word_name);

  // parse
  cell_t *c = parse_expr(&toks, module, e);
  e->entry.in = 0;

  // compile
  cell_t *left = *leftmost(&c);
  fill_args(e, left);
  e->entry.alts = trace_reduce(e, &c);
  drop(c);
  trace_end_entry(e);
  dedup_subentries(e);
  trace_compact(e);

  // finish
  free_def(l);
  return true;
}

void dedup_subentries(tcell_t *e) {
  FOR_TRACE(c, e) {
    // duplicate quotes are common
    if(is_user_func(c)) {
      tcell_t *ce = get_entry(c);
      if(FLAG(*ce, entry, QUOTE)) {
        dedup_entry(&ce);
        set_entry(c, ce);
      }
    }
  }
}

// mark inputs so they don't lift consumers out
void mark_barriers(tcell_t *entry, cell_t *c) {
  if(c->pos) entry = pos_entry(c->pos);
  TRAVERSE(c, in) {
    cell_t *x = *p;
    if(x) {
      LOG("barrier %s %C[%d]: %C #barrier", entry->word_name, c, p-c->expr.arg, x);
      mark_pos(x, entry->pos);
    }
  }
}

// mark values to be lifted out of recursive functions by setting pos
void move_changing_values(tcell_t *entry, cell_t *c) {
  // lift changing values out of all loops
  while(entry->entry.parent &&
        entry->entry.parent->entry.specialize) {
    entry = entry->entry.parent;
  }
  tcell_t *expanding = (tcell_t *)c->expr.arg[closure_in(c)];
  TRAVERSE(c, in) {
    if(is_value(*p) && !is_var(*p) && !is_persistent(*p)) {
      int i = expanding->entry.in - (p - c->expr.arg);
      if(FLAG(expanding[i], trace, CHANGES)) {
        WATCH(*p, "move changing value");
        LOG("set pos for %C to %s", *p, entry->word_name);
        (*p)->pos = entry->pos;
      }
    }
  }
}

// mark shared and boundary expressions so they don't lift consumers out
// TODO pull out/duplicate things referenced in multiple quotes
void mark_quote_barriers(tcell_t *entry, cell_t *c) {
  TRAVERSE(c, in) {
    cell_t *x = *p;
    if(!x) continue;
    if(x->n >= 0 || x->pos) {
      LOG("quote barrier %s %C #barrier", entry->word_name, x);
      mark_pos(x, entry->pos);
    } else {
      mark_quote_barriers(entry, x);
    }
  }
}

// build a call into the quote (new_entry) from parent
// see flat_call
cell_t *flat_quote(tcell_t *new_entry, tcell_t *parent_entry) {
  CONTEXT("flat quote (%s -> %s)", parent_entry->word_name, new_entry->word_name);

  // count switched (external) variables
  unsigned int i = 0;
  FOR_TRACE(tc, new_entry) {
    if(is_var(tc) && tc->value.var) {
      tc->var_index = ++i;
    }
  }

  unsigned int in = i;
  cell_t *nc = ALLOC(in + 1,
    .op = OP_exec
  );

  // use var_index to assign to nc
  FOR_TRACE(tc, new_entry) {
    if(is_var(tc)) {
      if(tc->value.var) {
        tcell_t *tp = var_for_entry(parent_entry, tc);
        cell_t *v = var_create_nonlist(trace_type(tp), tp);
        if(trace_type(tp) == T_OPAQUE) { // ***
          v->value.range = tp->trace.range;
        }
        assert_error(INRANGE(tc->var_index, 1, in));
        nc->expr.arg[in - tc->var_index] = v;
        LOG("arg[%d] -> %d", in - tc->var_index, tp - parent_entry);
      } else {
        tc->var_index = ++i;
      }
    }
  }
  nc->expr.arg[in] = (cell_t *)new_entry;
  return nc;
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
int compile_quote(tcell_t *parent_entry, cell_t *l) {
  // set up
  tcell_t *e = trace_start_entry(parent_entry, 1);
  e->module_name = parent_entry->module_name;
  int sub_id = parent_entry->entry.sub_id++;
  e->word_name = sub_id ?
    string_printf("%s:quote_%d", parent_entry->word_name, sub_id) :
    string_printf("%s:quote", parent_entry->word_name);
  FLAG_SET(*e, entry, QUOTE);
  CONTEXT_LOG("compiling %C to quote %s", l, e->word_name);

  // conversion
  cell_t *r = l->value.ptr[0];
  bool add_nil = is_var(r) ? NOT_FLAG(*r, value, ROW) : !is_placeholder(r);
  csize_t len = function_out(l, true) + add_nil;
  cell_t *ph = func(OP_placeholder, len, 1);
  if(add_nil) arg(ph, empty_list());
  cell_t **p;
  FORLIST(p, l, true) {
    LOG("arg %C %C", ph, *p);
    arg(ph, ref(*p));
  }
  if(is_row_list(*left_list(&l))) { // ***
    FLAG_SET(*ph, expr, ROW);
  }

  EACH(fake_drop, l, ph);
  mark_quote_barriers(e, ph);
  EACH(fake_undrop, ph, l);

  // compile
  fill_args(e, ph);
  cell_t *init = COPY_REF(ph, in);
  insert_root(&init);
  e->entry.alts = trace_reduce_one(e, ph);

  cell_t *q = flat_quote(e, parent_entry);
  //reverse_ptrs((void **)q->expr.arg, closure_in(q));
  drop(init);
  remove_root(&init);

  trace_clear_alt(parent_entry);
  cell_t *res = var_create_with_entry(T_LIST, parent_entry, q->size);
  assert_error(entry_has(parent_entry, res->value.var),
               "parent: %s, var: %s[%d]",
               parent_entry->word_name,
               var_entry(res->value.var),
               var_index(NULL, res->value.var));
  int x = var_index(parent_entry, res->value.var);
  trace_reduction(q, res);
  EACH(drop, q, res);

  trace_end_entry(e);

  apply_condition(l, &x);
  return x;
}

// decode a pointer to an index and return a trace pointer
const tcell_t *tref(const tcell_t *entry, const cell_t *c) {
  int i = tr_index(c);
  return i <= 0 ? NULL : &entry[i];
}

// get trace_t info for the nth output of entry e
void get_trace_info_for_output(trace_t *tr, const tcell_t *e, int n) {
  assert_error(n < e->entry.out);
  int i = e->entry.out - 1 - n;

  tr->type = T_BOTTOM;
  tr->bit_width = 0;
  tr->addr_width = 0;
  tr->range = RANGE_NONE;

  if(!e->trace.first_return) return;
  const tcell_t *p = &e[e->trace.first_return];
  assert_error(p->trace.type == T_RETURN);

  while(p) {
    const tcell_t *tc = tref(e, p->value.ptr[i]);
    type_t pt = trace_type(tc);
    if(tr->type == T_BOTTOM) {
      tr->type = pt;
    } else if(tr->type == pt) {
      // do nothing
    } else if(pt != T_BOTTOM) {
      tr->type = T_ANY;
    }
    tr->bit_width = max(tr->bit_width, tc->trace.bit_width);
    tr->addr_width = max(tr->addr_width, tc->trace.addr_width);
    tr->range = range_union(tr->range, tc->trace.range);
    p = tref(e, p->alt);
  }

  if(NOT_FLAG(*e, entry, COMPLETE) &&
     tr->type != T_OPAQUE) {
    tr->range = range_union(tr->range, default_bound);
  }
}

// very similar to get_name() but decodes entry
void trace_get_name(const tcell_t *tc, const char **module_name, const char **word_name) {
  if(is_user_func(tc)) {
    tcell_t *e = get_entry(tc); // <- differs from get_name()
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = op_name(tc->op);
  }
}

// token can be the entry number or the name
tcell_t *entry_from_token(cell_t *tok) {
  seg_t id = tok_seg(tok);
  if(tok->char_class == CC_NUMERIC) {
    return entry_from_number(strtol(id.s, NULL, 0));
  } else {
    cell_t *m = eval_module();
    return tcell_entry(module_lookup_compiled(id, &m));
  }
}

// to allow backwards iteration
void set_prev_cells(tcell_t *entry) {
  csize_t prev_cells = 0;
  FOR_TRACE(tc, entry) {
    tc->trace.prev_cells = prev_cells;
    prev_cells = calculate_tcells(tc->size);
  }
  entry[1].trace.prev_cells = prev_cells;
}

#if INTERFACE
#define is_return(c) _is_return(GET_CELL(c))
#define is_expr(c) _is_expr(GET_CELL(c))
#endif

bool _is_return(const cell_t *c) {
  return is_value(c) && c->value.type == T_RETURN;
}

bool _is_expr(cell_t *c) {
  return c && c->op != OP_value;
}

COMMAND(bc, "print bytecode for a word, or all") {
  if(rest) {
    CONTEXT("bytecode command");
    command_define(rest);
    tcell_t *e = entry_from_token(rest);
    if(!e) {
      printf("not found\n");
    } else {
      printf("\n");
      if(e->entry.alts) {
        print_bytecode(e, true);
      } else {
        highlight_errors(e->src);
        printf(" |\n");
        printf(" '-( " MARK("FAILED!") " )\n\n");
      }
    }
  } else {
    print_all_bytecode();
    if(command_line) quit = true;
  }
}

COMMAND(trace, "trace an instruction") {
  if(rest) {
    CONTEXT("trace command");
    tcell_t *e = entry_from_token(rest);
    if(e) {
      bool set = false;
      cell_t *arg = rest->tok_list.next;
      if(!arg) {
        FLAG_SET(*e, entry, TRACE);
        set = true;
        printf("tracing %s.%s\n",
               e->module_name,
               e->word_name);
      } else if(arg->char_class == CC_NUMERIC) {
        int x = strtol(arg->tok_list.location, NULL, 0);
        if(x > 0 &&
           x <= e->entry.len &&
           !is_value(&e[x])) {
          FLAG_SET(e[x], expr, TRACE);
          set = true;
          printf("tracing %s.%s [%d]\n",
                 e->module_name,
                 e->word_name,
                 x);
        }
      }
      if(!set) printf("Invalid argument\n");
    } else {
      printf("Entry not found\n");
    }
  }
}

PARAMETER(break_on_trace, bool, false, "break on trace") {
  break_on_trace = arg;
}
