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
#include <inttypes.h>

#include "startle/error.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"
#include "startle/log.h"

#include "cells.h"
#include "rt.h"
#include "eval.h"
#include "primitive.h"
#include "special.h"
#include "byte_compile.h"
#include "parse.h"
#include "print.h"
#include "lex.h"
#include "module.h"
#include "user_func.h"
#include "list.h"
#include "trace.h"

bool break_on_trace = false;

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
    print_escaped_string(value_seg(c));
    printf("\"");
    break;
  default:
    printf(" val ??");
    break;
  }
}

// print bytecode for entry e
static uint32_t hashes[1024];
void print_bytecode(tcell_t *entry, bool tags) {
  uint32_t entry_hash = 0;
  if(entry->entry.len > LENGTH(hashes)) tags = false;
  if(tags) {
    zero(hashes);
    entry_hash = hash_entry(entry, hashes);
  }
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
  if(tags) {
    tag_t tag;
    write_tag(tag, entry_hash);
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
        if(FLAG(*c, value, BOUNDED)) {
          if(tc->trace.min > INTPTR_MIN &&
             tc->trace.max < INTPTR_MAX) {
            printf(" in [%" PRIdPTR ", %" PRIdPTR "]",
                   tc->trace.min, tc->trace.max);
          } else if(tc->trace.min > INTPTR_MIN) {
            printf(" >= %" PRIdPTR, tc->trace.min);
          } else if(tc->trace.max < INTPTR_MAX) {
            printf(" <= %" PRIdPTR, tc->trace.max);
          }
        }
        if(c->value.type == T_OPAQUE) {
          printf(" is %s", symbol_string(c->value.symbol));
        }
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
      if(tc->trace.type == T_OPAQUE) {
        val_t sym = trace_get_opaque_symbol(entry, tc);
        if(sym >= 0) {
          printf(" is %s", symbol_string(sym));
        }
      }
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
      write_tag(tag, hashes[t-1]);
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

static
void condense(tcell_t *entry) {
  if(entry->entry.len == 0) return;
  tcell_t *ret = NULL;
  csize_t in = entry->entry.in;
  tcell_t *vars = get_trace_ptr(in);
  int nvars = 0;
  int idx = 1 + in;

  // drop unreferenced instructions
  FOR_TRACE(tc, entry) {
    if(tc->n < 0 && !(is_var(tc) && tc->pos)) {
      drop_trace(entry, tc);
    }
  }

  // calculate mapping
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      if(is_var(tc)) {
        // move variables out into temporary space
        assert_error(tc->pos);
        nvars++;
        memcpy(&vars[tc->pos-1], tc, sizeof(tcell_t));
        tc->op = OP_null;
        tc->alt = index_tr(tc->pos);
        LOG_WHEN(tc->pos != tc-entry, "move var %d -> %d", tc-entry, tc->pos);
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

  csize_t len = idx - 1;
  assert_error(nvars == entry->entry.in);

  // update references
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      TRAVERSE(tc, args, ptrs) {
        int x = tr_index(*p);
        if(x > 0) {
          tr_set_index(p, tr_index(entry[x].alt));
          assert_error(tr_index(*p) > 0, "at %s[%d]", entry->word_name, tc-entry);
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

static
void condense_and_analyze(tcell_t *entry) {
  condense(entry);
  last_use_analysis(entry);
  no_skip_analysis(entry);
  mark_jumps(entry);
}

// runs after reduction to finish functions marked incomplete
void trace_final_pass(tcell_t *entry) {
  // replace alts with trace cells
  tcell_t *prev = NULL;

  // propagate types to asserts
  FOR_TRACE(p, entry) {
    if(p->op == OP_assert &&
       p->trace.type == T_ANY) {
      p->trace.type = trace_type(&entry[tr_index(p->expr.arg[0])]);
    }
  }

  FOR_TRACE(tc, entry) {
    cell_t *p = &tc->c;
    if(FLAG(*tc, trace, INCOMPLETE)) {
      if(p->op == OP_placeholder) { // convert a placeholder to ap or compose
        FLAG_CLEAR(*tc, trace, INCOMPLETE);
        if(closure_in(p) > 1 && FLAG(*p, expr, ROW)) {
          cell_t **right_arg = &p->expr.arg[closure_in(p)-1];
          cell_t *l = &entry[tr_index(*right_arg)].c;
          if(is_nil(l)) {
            p->op = OP_pushr;
            l->n--;
            p->size--;
            *right_arg = NULL;
          } else {
            p->op = OP_compose;
          }
        } else {
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
  if(NOT_FLAG(*entry, entry, QUOTE) &&
    TWEAK(true, "to disable condense/move_vars in %s", entry->word_name)) {
    condense_and_analyze(entry);
  }
  FOR_TRACE(tc, entry) {
    if(tc->op == OP_exec) {
      tcell_t *e = get_entry(tc);
      if(FLAG(*e, entry, QUOTE)) {
        condense_and_analyze(e);
      }
    }
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
  cell_t *p = module_lookup(path, context);
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

const char *sym_to_ident(unsigned char c) {
  static const char *table[] = {
    ['^'] = "__caret__"
    // TODO add all the other valid symbols
  };
  if(c < LENGTH(table)) {
    return table[c];
  } else {
    return NULL;
  }
}

size_t expand_sym(char *buf, size_t n, seg_t src) {
  char *out = buf, *stop = out + n - 1;
  const char *in = src.s;
  size_t left = src.n;
  while(left-- &&
        *in &&
        out < stop) {
    char c = *in++;
    const char *s = sym_to_ident(c);
    if(s) {
      out = stpncpy(out, s, stop - out);
    } else {
      *out++ = c;
    }
  }
  *out = '\0';
  return out - buf;
}

COMMAND(ident, "convert symbol to C identifier") {
  cell_t *p = rest;
  while(p) {
    char ident[64]; // ***
    seg_t ident_seg = {
      .s = ident,
      .n = expand_sym(ident, LENGTH(ident), tok_seg(p))
    };
    COUNTUP(i, ident_seg.n) {
      if(ident[i] == '.') ident[i] = '_';
    }
    printseg("", ident_seg, "\n");
    p = p->tok_list.next;
  }
  if(command_line) quit = true;
}

bool compile_word(cell_t **entry, seg_t name, cell_t *module, csize_t in, csize_t out) {
  cell_t *l;
  char ident[64]; // ***
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
  seg_t ident_seg = {
    .s = ident,
    .n = expand_sym(ident, LENGTH(ident), name)
  };
  e->word_name = seg_string(ident_seg); // TODO fix unnecessary alloc
  CONTEXT_LOG("compiling %s", e->word_name);

  // parse
  cell_t *c = parse_expr(&toks, module, e);
  e->entry.in = 0;

  // compile
  cell_t *left = *leftmost(&c);
  fill_args(e, left);
  e->entry.alts = trace_reduce(e, &c);
  drop(c);
  trace_final_pass(e);
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

// replace variable c if there is a matching entry in a
void replace_var(cell_t *c, cell_t **a, csize_t a_n, tcell_t *entry) {
  int x = var_index(entry, c->value.var);
  COUNTUP(j, a_n) {
    int y = tr_index(a[j]);
    if(y == x) {
      int xn = a_n - j;
      tcell_t *tc = &entry[xn];
      tc->value.type = trace_type(c->value.var);
      c->value.var = &entry[xn];
      return;
    }
  }

  { // diagnostics for fall through, which shouldn't happen
    CONTEXT_LOG("replace_var fall through: %C (%d)", c, x);
    COUNTUP(j, a_n) {
      LOG("%d -> %d", tr_index(a[j]), j);
    }
  }
}

// need a quote version that only marks vars
void mark_barriers(tcell_t *entry, cell_t *c) {
  TRAVERSE(c, in) {
    cell_t *x = *p;
    if(x) {
      LOG("barrier %s %C[%d]: %C #barrier", entry->word_name, c, p-c->expr.arg, x);
      mark_pos(x, entry->pos);
    }
  }
}

void set_pos_for_values(cell_t *c, tcell_t *entry) {
  if(is_value(c)) {
    if(!is_var(c) && is_cell(c)) {
      LOG("set pos for %C to %s", c, entry->word_name);
      c->pos = entry->pos;
    }
  } else if(c->op == OP_ap ||
            c->op == OP_swap ||
            c->op == OP_exec) { // *** HACK
    TRAVERSE(c, in) {
      if(*p) set_pos_for_values(*p, entry);
    }
  }
}

void move_changing_values(tcell_t *entry, cell_t *c) {
  tcell_t *expanding = (tcell_t *)c->expr.arg[closure_in(c)];
  TRAVERSE(c, in) {
    int i = expanding->entry.in - (p - c->expr.arg);
    if(FLAG(expanding[i], trace, CHANGES)) {
      set_pos_for_values(*p, entry);
    }
  }
}

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

cell_t *flat_quote(tcell_t *new_entry, tcell_t *parent_entry) {
  CONTEXT("flat quote (%s -> %s)", parent_entry->word_name, new_entry->word_name);
  unsigned int in = new_entry->entry.in;

  FOR_TRACE(tc, new_entry) {
    cell_t *p = &tc->c;
    if(is_var(p) && !p->value.var) {
      in--;
    }
  }

  cell_t *nc = ALLOC(in + 1,
    .op = OP_exec
  );

  FOR_TRACE(tc, new_entry) {
    cell_t *p = &tc->c;
    if(is_var(p) && p->value.var) {
      tcell_t *tp = var_for_entry(parent_entry, tc);
      cell_t *v = var_create_nonlist(trace_type(tp), tp);
      assert_error(INRANGE(p->pos, 1, in));
      nc->expr.arg[in - p->pos] = v;
      LOG("arg[%d] -> %d", in - p->pos, tp - parent_entry);
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
  e->word_name = string_printf("%s_q%d", parent_entry->word_name, parent_entry->entry.sub_id++);
  FLAG_SET(*e, entry, QUOTE);
  CONTEXT_LOG("compiling %C to quote %s", l, e->word_name);

  // conversion
  bool add_nil = !is_placeholder(l->value.ptr[0]);
  csize_t len = function_out(l, true) + add_nil;
  cell_t *ph = func(OP_placeholder, len, 1);
  if(add_nil) arg(ph, empty_list());
  cell_t **p;
  FORLIST(p, l, true) {
    LOG("arg %C %C", ph, *p);
    arg(ph, ref(*p));
  }
  if(is_row_list(l)) { // ***
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

  trace_final_pass(e); // *** wait?
  trace_end_entry(e);

  trace_clear_alt(parent_entry);
  cell_t *res = var(T_LIST, q, parent_entry->pos);
  assert_error(entry_has(parent_entry, res->value.var),
               "parent: %s, var: %s[%d]",
               parent_entry->word_name,
               var_entry(res->value.var),
               var_index(NULL, res->value.var));
  int x = var_index(parent_entry, res->value.var);
  trace_reduction(q, res);
  EACH(drop, q, res);

  apply_condition(l, &x);
  return x;
}

// decode a pointer to an index and return a trace pointer
static
const tcell_t *tref(const tcell_t *entry, const cell_t *c) {
  int i = tr_index(c);
  return i <= 0 ? NULL : &entry[i];
}

// get the return type
type_t trace_type(const tcell_t *tc) {
  assert_error(tc);
  return is_value(tc) ? tc->value.type : tc->trace.type;
}

// resolve types in each return in e starting at c, storing the resulting types in t
void resolve_types(const tcell_t *e, type_t *t) {
  csize_t out = e->entry.out;

  COUNTUP(i, out) {
    t[i] = T_BOTTOM;
  }

  // find first return
  const tcell_t *p = NULL;
  FOR_TRACE_CONST(tc, e) {
    if(trace_type(tc) == T_RETURN) {
      p = tc;
      break;
    }
  }
  if(!p) return;

  while(p) {
    COUNTUP(i, out) {
      type_t pt = trace_type(tref(e, p->value.ptr[i]));
      type_t *rt = &t[out-1 - i];
      if(*rt == T_BOTTOM) {
        *rt = pt;
      } else if(*rt != pt &&
                pt != T_BOTTOM) {
        *rt = T_ANY;
      }
    }
    p = tref(e, p->alt);
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

static
void last_use_mark(tcell_t *entry, cell_t **p) {
  int x = tr_index(*p);
  tcell_t *a = &entry[x];
  if(x && NOT_FLAG(*a, trace, USED)) {
    tr_set_flags(p, TR_FINAL);
    FLAG_SET(*a, trace, USED);
  }
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

static
void clear_used(tcell_t *entry) {
  FOR_TRACE(tc, entry) {
    FLAG_CLEAR(*tc, trace, USED);
  }
}

static
void last_use_mark_cell(tcell_t *entry, cell_t *c) {
  if(is_value(c)) {
    if(is_return(c)) {
      COUNTUP(i, list_size(c)) {
        last_use_mark(entry, &c->value.ptr[i]);
      }
    }
  } else if(!is_dep(c)) {
    COUNTDOWN(i, closure_in(c)) {
      last_use_mark(entry, &c->expr.arg[i]);
    }
  }
}

void last_use_analysis(tcell_t *entry) {
  set_prev_cells(entry);
  clear_used(entry);

  // calls and returns
  FOR_TRACE_REV(tc, entry) {
    cell_t *c = &tc->c;
    last_use_mark_cell(entry, c);
    if(!is_return(c) && !is_dep(c) && !direct_refs(c)) {
      trace_set_type(tc, T_BOTTOM, 0);
    }
  }

  // go back over non-partial segments for each branch
  bool partial = false;
  FOR_TRACE_REV(tc, entry) {
    cell_t *c = &tc->c;
    if(is_return(c)) {
      clear_used(entry);
      partial = false;
    } else if(is_expr(c) && FLAG(*c, expr, PARTIAL)) {
      partial = true;
    }

    if(!partial) {
      last_use_mark_cell(entry, c);
    }
  }
}

void mark_jumps(tcell_t *entry) {
  tcell_t *jump = NULL;
  FOR_TRACE(tc, entry) {
    cell_t *c = &tc->c;
    if(ONEOF(c->op, OP_seq, OP_assert, OP_unless, OP_pushr, OP_compose, OP_dep)) continue;
    if(is_return(c) && jump) {
      tcell_t *e = get_entry(jump);
      if(e == entry) {
        FLAG_SET(*tc, trace, JUMP);
      }
      FLAG_SET(*jump, trace, JUMP);
      jump = NULL;
      continue;
    }
    if(is_value(c)) continue;
    if(is_user_func(c)) {
      jump = tc;
      continue;
    }
    jump = NULL;
  }
}

// Recursively mark dependencies in a previous branch, breaking the dependency chain
// at a partial. Direct dependencies on that partial are not marked.
// TODO optimize
static
bool mark_no_skip(tcell_t *entry, tcell_t *c, int last_return, int last_partial) {
  bool res = true;
  int i = c - entry;
  if(!is_expr(c) || i < last_partial) return true;
  if(i < last_return && last_partial == 0) {
    // look for last partial before this
    FOR_TRACE_REV(p, entry, i) {
      if(FLAG(*p, expr, PARTIAL)) {
        last_partial = p - entry;
        break;
      }
    }

    // return if none is found
    if(last_partial == 0) return true;
  }

  if(i == last_partial) {
    last_partial = 0;
    res = false;
  }

  // mark dependencies
  if(ONEOF(c->op, OP_seq, OP_assert, OP_unless)) {
    res &= mark_no_skip(entry, &entry[tr_index(c->expr.arg[0])], last_return, last_partial);
  } else {
    TRAVERSE(c, in) {
      res &= mark_no_skip(entry, &entry[tr_index(*p)], last_return, last_partial);
    }
  }

  if(res && i < last_return) FLAG_SET(*c, trace, NO_SKIP);
  return res;
}

// mark instructions that can't be skipped on failure
// used in cgen
void no_skip_analysis(tcell_t *entry) {
  int last_return = 0;
  FOR_TRACE(tc, entry) {
    if(is_return(tc)) {
      last_return = tc - entry;
    } else {
      mark_no_skip(entry, tc, last_return, 0);
    }
  }
}

COMMAND(bc, "print bytecode for a word, or all") {
  if(rest) {
    CONTEXT("bytecode command");
    command_define(rest);
    tcell_t *e = entry_from_token(rest);
    if(e) {
      printf("\n");
      print_bytecode(e, true);
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

COMMAND(bt, "break on trace") {
  break_on_trace = true;
}

static
csize_t dep_pos(const tcell_t *entry,
                const tcell_t *tc) {
  const tcell_t *src = &entry[tr_index(tc->expr.arg[0])];
  int index = tc - entry;
  int pos = 1;
  TRAVERSE(src, const, out) {
    if(tr_index(*p) == index) {
      return pos;
    } else {
      pos++;
    }
  }
  return 0;
}

#define HASH(l, x) (hash = (hash * 1021 + (uint32_t)(l)) * 1979 + (uint32_t)(x))
uint32_t hash_trace_cell(tcell_t *entry, tcell_t *tc, uint32_t *arr) {
  uint32_t hash = 1;
  int idx = tc - entry - 1;

  if(!tc || !tc->op ||
     !INRANGE(idx, 0, entry->entry.len)) {
    HASH('n', 1);
    return hash;
  }

  if(arr && arr[idx]) return arr[idx];

  if(is_value(tc) && !is_list(tc)) {
    if(is_var(tc)) {
      HASH('v', tc->pos);
    } else if(tc->value.type == T_INT) {
      HASH('i', tc->value.integer);
    } else if(tc->value.type == T_SYMBOL) {
      HASH('y', tc->value.symbol);
    }
    // HASH('t', tc->value.type);
    goto end;
  }
  TRAVERSE(tc, in, ptrs) {
    HASH('a', hash_trace_cell(entry, &entry[tr_index(*p)], arr));
  }
  if(is_dep(tc)) {
    HASH('d', dep_pos(entry, tc));
  }
  HASH('o', tc->op);
  if(!is_value(tc)) HASH('O', closure_out(tc));
end:
  if(arr) arr[idx] = hash;
  return hash;
}

uint32_t hash_entry(tcell_t *entry, uint32_t *arr) {
  uint32_t hash = 1;
  FOR_TRACE(tc, entry) {
    HASH('c', hash_trace_cell(entry, tc, arr));
  }
  return hash;
}
#undef HASH

FORMAT(tag, 'H') {
  tag_t tag;
  write_tag(tag, i);
  printf(FORMAT_TAG, tag);
}

size_t backrefs_size(const tcell_t *entry) {
  size_t n = entry->entry.len;
  FOR_TRACE_CONST(tc, entry) {
    if(!is_return(tc)) {
      n += tc->n + 1;
    }
  }
  return n;
}

bool set_nonzero(uintptr_t *arr, size_t n, uintptr_t x) {
  assert_error(x);
  COUNTUP(i, n) {
    if(!arr[i]) {
      arr[i] = x;
      return true;
    }
  }
  return false;
}

size_t get_outputs(const tcell_t *entry, const tcell_t *c, uintptr_t const *const *backrefs, uintptr_t const **outputs) {
  int i = c - entry;
  assert_error(INRANGE(i, 1, entry->entry.len));
  *outputs = backrefs[i - 1];
  return entry[i].n + 1;
}

void build_backrefs(const tcell_t *entry, uintptr_t **table, size_t size) {
  uintptr_t **index = table;
  uintptr_t *backref = (uintptr_t *)(table + entry->entry.len);
  memset(table, 0, sizeof(*table) * size);

  // build the index
  FOR_TRACE_CONST(tc, entry) {
    if(!is_return(tc)) {
      index[tc - entry - 1] = backref;
      backref += tc->n + 1;
    } else {
      index[tc - entry - 1] = NULL;
    }
  }

  assert_le((uintptr_t **)backref - table, (int)size);

  // fill backrefs
  FOR_TRACE_CONST(tc, entry) {
    TRAVERSE(tc, const, in, ptrs) {
      int x = tr_index(*p);
      bool success = set_nonzero(index[x-1], entry[x].n + 1, tc - entry);
      assert_error(success);
    }
  }
}
