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
    val_t x = c->value.integer;
    const char *str = symbol_string(x);
    if(!str) str = "??";
    printf(" val %s", str);
    break;
  }
  case T_FLOAT:
    printf(" val %g", c->value.flt);
    break;
  case T_STRING:
    printf(" val \"%s\"", c->value.str);
    break;
  default:
    printf(" val ??");
    break;
  }
}

// print bytecode for entry e
void print_bytecode(cell_t *entry) {
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
  if(entry->entry.rec) {
    printf(" rec");
  }
  printf(" ___\n");

  // body
  FOR_TRACE(c, entry) {
    int t = c - entry;
    printf("[%d]", t);
    if(!c->op) {
      printf("\n");
      continue;
    }
    if(is_value(c)) {
      if(is_list(c) || c->value.type == T_RETURN) { // return
        if(c->value.type == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %d", trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
      } else if(is_var(c)) { // variable
        printf(" var");
      } else { // value
        print_value(c);
      }
      printf(", type = %s", show_type_all_short(c));
      if(c->value.type == T_RETURN && c->alt) {
        printf(" -> %d", trace_decode(c->alt));
      }
    } else { // print a call
      const char *module_name = NULL, *word_name = NULL;
      if(NOT_FLAG(c->trace, TRACE_INCOMPLETE)) {
        trace_get_name(c, &module_name, &word_name);
        printf(" %s.%s", module_name, word_name);
      } else {
        printf(" incomplete %s", op_name(c->op));
      }
      TRAVERSE(c, in) {
        int x = trace_decode(*p);
        if(x == 0) {
          printf(" X");
        } else {
          printf(" %d", x);
        }
      }
      if(closure_out(c)) {
        printf(" ->");
        TRAVERSE(c, out) {
          int x = trace_decode(*p);
          if(x == 0) {
            printf(" X");
          } else {
            printf(" %d", x);
          }
        }
      }
      printf(", type = %c", type_char(c->trace.type));
    }
    printf(" x%d", c->n + 1);
    if(!is_value(c) && FLAG(c->expr, EXPR_TRACE)) {
      printf(" [TRACING]");
    }
    if(t > entry->entry.in &&
       c->n + 1 == 0) {
      printf(" <-- WARNING: zero refcount\n");
    } else if(is_dep(c) && !c->expr.arg[0]) {
      printf(" <-- WARNING: broken dep\n");
    } else {
      printf("\n");
    }
  }

  // print sub-functions once
  FOR_TRACE(c, entry) {
    if(is_user_func(c)) {
      cell_t *e = get_entry(c);
      if(e->entry.parent == entry && !e->op) {
        printf("\n");
        print_bytecode(e);
        e->op = OP_value;
      }
    }
  }
  FOR_TRACE(c, entry) {
    if(is_user_func(c)) {
      cell_t *e = get_entry(c);
      e->op = OP_null;
    }
  }
}

void drop_trace(cell_t *entry, cell_t *tc) {
  if(tc->n <= 0) {
    LOG("drop %T %O", tc, tc->op);
    TRAVERSE(tc, in, ptrs) {
      int x = trace_decode(*p);
      if(x > 0) {
        drop_trace(entry, &entry[x]);
      }
    }
    tc->op = OP_null;
  } else {
    tc->n--;
  }
}

static
void condense(cell_t *entry) {
  if(entry->entry.len == 0) return;
  cell_t *ret = NULL;
  int idx = 1;

  // drop unreferenced instructions
  FOR_TRACE(tc, entry) {
    if(tc->n < 0 && !(is_var(tc) && tc->pos)) {
      drop_trace(entry, tc);
    }
  }

  // calculate mapping
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      if(is_value(tc) && tc->value.type == T_RETURN) {
        if(ret) ret->alt = trace_encode(idx);
        ret = tc;
      } else {
        tc->alt = trace_encode(idx);
      }
      idx += calculate_cells(tc->size);
    } else {
      LOG("collapse %d", tc - entry);
    }
  }

  // update references
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type == T_RETURN) {
        COUNTUP(i, list_size(tc)) {
          cell_t **p = &tc->value.ptr[i];
          int x = trace_decode(*p);
          if(x > 0) {
            *p = entry[x].alt;
            assert_error(trace_decode(*p) > 0);
          }
        }
      } else {
        TRAVERSE(tc, args, ptrs) {
          if(p != e) {
            int x = trace_decode(*p);
            if(x > 0) {
              *p = entry[x].alt;
              assert_error(trace_decode(*p) > 0, "at %T", tc);
            }
          }
        }
      }
    }
  }
  // condense
  idx = 1;
  FOR_TRACE(p, entry) {
    if(p->op) {
      csize_t s = calculate_cells(p->size);
      if(!(is_value(p) && p->value.type == T_RETURN)) {
        p->alt = NULL;
      }
      if(idx < p - entry) {
        cell_t *n = &entry[idx];
        memmove(n, p, s * sizeof(cell_t));
        memset(n + s, 0, (p - n) * sizeof(cell_t));
        p = n;
      }
      idx += s;
    }
  }
  entry->entry.len = idx - 1;
}

// TODO optimize
static
void move_vars(cell_t *entry) {
  if(NOT_FLAG(entry->entry, ENTRY_MOV_VARS) ||
     entry->entry.len == 0) return;
  cell_t *ret = NULL;
  csize_t in = entry->entry.in;
  csize_t len = entry->entry.len;
  cell_t *vars = get_trace_ptr(in);
  int idx = 1 + in;
  int nvars = 0;

  CONTEXT("move_vars for %E", entry);

  // calculate mapping
  FOR_TRACE(p, entry) {
    if(!is_var(p)) {
      if(is_value(p) && p->value.type == T_RETURN) {
        if(ret) ret->alt = trace_encode(idx);
        ret = p;
      } else {
        LOG_WHEN(idx != p-entry, "move %d -> %d", p-entry, idx);
        p->alt = trace_encode(idx);
      }
      idx += calculate_cells(p->size);
    } else {
      assert_error(p->pos);
      nvars++;
      int i = p->pos - 1;
      memcpy(&vars[i], p, sizeof(cell_t));
      p->op = OP_null;
      p->alt = trace_encode(p->pos);
      LOG_WHEN(i + 1 != p-entry, "move var %d -> %d", p-entry, p->pos);
    }
  }

  assert_error(nvars == entry->entry.in);

  // update references
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type == T_RETURN) {
        COUNTUP(i, list_size(tc)) {
          cell_t **p = &tc->value.ptr[i];
          int x = trace_decode(*p);
          if(x > 0) *p = entry[x].alt;
        }
      } else {
        TRAVERSE(tc, args, ptrs) {
          if(p != e) {
            int x = trace_decode(*p);
            if(x > 0) *p = entry[x].alt;
          }
        }
      }
    }
  }
  // condense
  idx = 1;
  FOR_TRACE(p, entry) {
    if(p->op) {
      csize_t s = calculate_cells(p->size);
      if(!(is_value(p) && p->value.type == T_RETURN)) {
        p->alt = NULL;
      }
      if(idx < p - entry) {
        cell_t *n = &entry[idx];
        memmove(n, p, s * sizeof(cell_t));
        memset(n + s, 0, (p - n) * sizeof(cell_t));
        p = n;
      }
      idx += s;
    }
  }

  // prepend vars
  memmove(&entry[in + 1], &entry[1], (len - in) * sizeof(cell_t));
  memcpy(&entry[1], vars, in * sizeof(cell_t));
  memset(vars, 0, in * sizeof(cell_t));
  FLAG_CLEAR(entry->entry, ENTRY_MOV_VARS);
}

static
void trace_replace_arg(cell_t *entry, cell_t *old, cell_t *new) {
  FOR_TRACE(tc, entry) {
    if(tc->op) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type == T_RETURN) {
        COUNTUP(i, list_size(tc)) {
          cell_t **p = &tc->value.ptr[i];
          if(*p == old) *p = new;
        }
      } else {
        TRAVERSE(tc, args, ptrs) {
          if(p != e) {
            if(*p == old) *p = new;
          }
        }
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

void trace_drop_return(cell_t *entry, int out, uintptr_t dep_mask) {
  int used = 0;
  FORMASK(i, j, dep_mask) {
    used++;
  }
  if(used == out) return;
  FOR_TRACE(p, entry) {
    if(p->op == OP_exec && get_entry(p) == entry) {
      shift_out(p, dep_mask);
    }
  }
  entry->entry.out = used;
}

// runs after reduction to finish functions marked incomplete
void trace_final_pass(cell_t *entry) {
  // replace alts with trace cells
  cell_t *prev = NULL;

  // propagate types to asserts
  FOR_TRACE(p, entry) {
    if(p->op == OP_assert &&
       p->trace.type == T_ANY) {
      p->trace.type = trace_type(&entry[trace_decode(p->expr.arg[0])]);
    }
  }

  FOR_TRACE(p, entry) {
    if(FLAG(p->trace, TRACE_INCOMPLETE)) {
      if(p->op == OP_placeholder) { // convert a placeholder to ap or compose
        FLAG_CLEAR(p->trace, TRACE_INCOMPLETE);
        trace_index_t left = trace_decode(p->expr.arg[0]);
        assert_error(left >= 0);
        if(closure_in(p) > 1 && trace_type(&entry[left]) == T_LIST) {
          p->op = OP_compose;
        } else {
          p->op = OP_ap;
        }
        if(prev && prev->op == OP_ap &&
           trace_decode(p->expr.arg[closure_in(p) - 1]) == prev - entry &&
           prev->n == 0) {
          LOG("merging ap %d to %d", p - entry, prev - entry);
          csize_t
            p_in = closure_in(p),
            p_out = closure_out(p),
            p_size = closure_args(p);
          refcount_t p_n = p->n;
          cell_t *tmp = copy(p);
          cell_t
            *p_enc = trace_encode(p - entry),
            *prev_enc = trace_encode(prev - entry);
          trace_replace_arg(entry, p_enc, prev_enc);
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
    prev = p;
  }
  if(NOT_FLAG(entry->entry, ENTRY_QUOTE) &&
    TWEAK(true, "to disable condense/move_vars in %e", entry)) {
    condense(entry);
    move_vars(entry);
  }
  FOR_TRACE(p, entry) {
    if(p->op == OP_exec) {
      cell_t *e = get_entry(p);
      if(FLAG(e->entry, ENTRY_QUOTE)) {
        condense(e);
        move_vars(e);
      }
    }
  }
}

// add an entry and all sub-entries to a module
static
void store_entries(cell_t *entry, cell_t *module) {
  module_set(module, string_seg(entry->word_name), entry);
  FOR_TRACE(c, entry) {
    if(c->op != OP_exec) continue;
    cell_t *sub_e = get_entry(c);
    if(sub_e->entry.parent == entry) store_entries(sub_e, module);
  }
}

static
cell_t *compile_entry(seg_t name, cell_t *module) {
  csize_t in, out;
  cell_t *entry = implicit_lookup(name, module);
  if(!is_list(entry)) return entry;
  cell_t *ctx = get_module(string_seg(entry->module_name)); // switch context module for words from other modules
  if(pre_compile_word(entry, ctx, &in, &out) &&
     compile_word(&entry, name, ctx, in, out)) {
    store_entries(entry, module);
    return entry;
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
  if(FLAG(p->value, VALUE_TRACED)) {
    if(p->alt) { // HACKy
      return p->alt;
    } else {
      return lookup_word(string_seg("??"));
    }
  }
  FLAG_SET(p->value, VALUE_TRACED);
  seg_t name = path_name(path);
  cell_t *res = compile_entry(name, *context);
  if(!res) FLAG_CLEAR(p->value, VALUE_TRACED);
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
  cell_t *e = trace_start_entry(NULL, out);
  e->entry.in = in;
  // make recursive return this entry
  (*entry)->alt = e; // ***
  *entry = e;

  e->module_name = module_name(module);
  seg_t ident_seg = {
    .s = ident,
    .n = expand_sym(ident, LENGTH(ident), name)
  };
  e->word_name = seg_string(ident_seg); // TODO fix unnecessary alloc
  CONTEXT_LOG("compiling %E", e);

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
  trace_compact(e);

  // finish
  free_def(l);
  return true;
}

// replace variable c if there is a matching entry in a
void replace_var(cell_t *c, cell_t **a, csize_t a_n, cell_t *entry) {
  int x = var_index(entry, c->value.var);
  COUNTUP(j, a_n) {
    int y = trace_decode(a[j]);
    if(y == x) {
      int xn = a_n - j;
      cell_t *tc = &entry[xn];
      tc->value.type = trace_type(c->value.var);
      c->value.var = &entry[xn];
      return;
    }
  }

  { // diagnostics for fall through, which shouldn't happen
    CONTEXT_LOG("replace_var fall through: %C (%d)", c, x);
    COUNTUP(j, a_n) {
      LOG("%d -> %d", trace_decode(a[j]), j);
    }
  }
}

// need a quote version that only marks vars
void mark_barriers(cell_t *entry, cell_t *c) {
  TRAVERSE(c, in) {
    cell_t *x = *p;
    if(x) {
      LOG("barrier %E %C[%d]: %C #barrier", entry, c, p-c->expr.arg, x);
      mark_pos(x, entry->pos);
    }
  }
}

void set_pos_for_values(cell_t *c, cell_t *entry) {
  if(is_value(c)) {
    if(!is_var(c) && is_cell(c)) {
      LOG("set pos for %C to %e", c, entry);
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

void move_changing_values(cell_t *entry, cell_t *c) {
  cell_t *expanding = c->expr.arg[closure_in(c)];
  TRAVERSE(c, in) {
    int i = expanding->entry.in - (p - c->expr.arg);
    if(FLAG(expanding[i].value, VALUE_CHANGES)) {
      set_pos_for_values(*p, entry);
    }
  }
}

// TODO pull out/duplicate things referenced in multiple quotes
void mark_quote_barriers(cell_t *entry, cell_t *c) {
  TRAVERSE(c, in) {
    cell_t *x = *p;
    if(!x) continue;
    if(x->n >= 0) {
      LOG("quote barrier %E %C #barrier", entry, x);
      mark_pos(x, entry->pos);
    } else {
      mark_quote_barriers(entry, x);
    }
  }
}

cell_t *flat_quote(cell_t *new_entry, cell_t *parent_entry) {
  CONTEXT("flat quote (%e -> %e)", parent_entry, new_entry);
  unsigned int in = new_entry->entry.in;

  FOR_TRACE(p, new_entry) {
    if(is_var(p) && !p->value.var) {
      in--;
    }
  }

  cell_t *nc = closure_alloc(in + 1);
  nc->op = OP_exec;

  FOR_TRACE(p, new_entry) {
    if(is_var(p) && p->value.var) {
      cell_t *tp = var_for_entry(parent_entry, p);
      cell_t *v = var_create_nonlist(trace_type(tp), tp);
      assert_error(INRANGE(p->pos, 1, in));
      nc->expr.arg[in - p->pos] = v;
      LOG("arg[%d] -> %d", in - p->pos, tp - parent_entry);
    }
  }
  nc->expr.arg[in] = new_entry;
  return nc;
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
int compile_quote(cell_t *parent_entry, cell_t *l) {
  // set up
  cell_t *e = trace_start_entry(parent_entry, 1);
  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_q%d", parent_entry->word_name, parent_entry->entry.sub_id++);
  FLAG_SET(e->entry, ENTRY_QUOTE);
  CONTEXT_LOG("compiling %C to quote %E", l, e);

  // conversion
  csize_t len = function_out(l, true);
  cell_t *ph = func(OP_placeholder, len + 1, 1);
  arg(ph, empty_list());
  cell_t **p;
  FORLIST(p, l, true) {
    LOG("arg %C %C", ph, *p);
    arg(ph, ref(*p));
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

  // duplicate quotes are common
  dedup_entry(&q->expr.arg[closure_in(q)]);

  trace_clear_alt(parent_entry);
  cell_t *res = var(T_LIST, q, parent_entry->pos);
  assert_error(entry_has(parent_entry, res->value.var),
               "parent: %E, var: %T",
               parent_entry, res->value.var);
  int x = var_index(parent_entry, res->value.var);
  trace_reduction(q, res);
  EACH(drop, q, res);

  apply_condition(l, &x);
  return x;
}

// decode a pointer to an index and return a trace pointer
static
cell_t *tref(cell_t *entry, cell_t *c) {
  int i = trace_decode(c);
  return i <= 0 ? NULL : &entry[i];
}

// get the return type
type_t trace_type(cell_t *c) {
  assert_error(c);
  return is_value(c) ? c->value.type : c->trace.type;
}

// resolve types in each return in e starting at c, storing the resulting types in t
void resolve_types(cell_t *e, type_t *t) {
  csize_t out = e->entry.out;

  COUNTUP(i, out) {
    t[i] = T_BOTTOM;
  }

  // find first return
  cell_t *p = NULL;
  FOR_TRACE(tc, e) {
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
void trace_get_name(const cell_t *c, const char **module_name, const char **word_name) {
  if(is_user_func(c)) {
    cell_t *e = get_entry(c); // <- differs from get_name()
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = op_name(c->op);
  }
}

cell_t *entry_from_token(cell_t *tok) {
  seg_t id = tok_seg(tok);
  if(tok->char_class == CC_NUMERIC) {
    return entry_from_number(strtol(id.s, NULL, 0));
  } else {
    cell_t *m = eval_module();
    return module_lookup_compiled(id, &m);
  }
}

COMMAND(bc, "print bytecode for a word, or all") {
  if(rest) {
    CONTEXT("bytecode command");
    command_define(rest);
    cell_t *e = entry_from_token(rest);
    if(e) {
      printf("\n");
      print_bytecode(e);
    }
  } else {
    print_all_bytecode();
    if(command_line) quit = true;
  }
}

COMMAND(trace, "trace an instruction") {
  if(rest) {
    CONTEXT("trace command");
    cell_t *e = entry_from_token(rest);
    if(e) {
      bool set = false;
      cell_t *arg = rest->tok_list.next;
      if(!arg) {
        FLAG_SET(e->entry, ENTRY_TRACE);
        set = true;
        printf("tracing %s.%s\n",
               e->module_name,
               e->word_name);
      } else if(arg->char_class == CC_NUMERIC) {
        int x = strtol(arg->tok_list.location, NULL, 0);
        if(x > 0 &&
           x <= e->entry.len &&
           !is_value(&e[x])) {
          FLAG_SET(e[x].expr, EXPR_TRACE);
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
