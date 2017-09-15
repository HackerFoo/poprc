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
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

#include "gen/error.h"
#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/byte_compile.h"
#include "gen/parse.h"
#include "gen/print.h"
#include "gen/lex.h"
#include "gen/module.h"
#include "gen/user_func.h"
#include "gen/list.h"
#include "gen/log.h"
#include "gen/trace.h"

static int graph_entry = -1;

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
    if(!c->func) {
      printf("\n");
      continue;
    }
    if(is_value(c)) {
      bool can_have_alt = false;
      if(is_list(c) || c->value.type.exclusive == T_RETURN) { // return
        can_have_alt = true;
        if(c->value.type.exclusive == T_RETURN) printf(" return");
        printf(" [");
        COUNTDOWN(i, list_size(c)) {
          printf(" %d", trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
      } else if(is_var(c)) { // variable
        printf(" var");
      } else { // value
        printf(" val %" PRIdPTR, c->value.integer[0]);
      }
      printf(", type = %s", show_type_all_short(c->value.type));
      if(can_have_alt && c->alt) printf(" -> %d", trace_decode(c->alt));
    } else { // print a call
      const char *module_name = NULL, *word_name = NULL;
      if(NOT_FLAG(c->expr_type, T_INCOMPLETE)) trace_get_name(c, &module_name, &word_name);
      printf(" %s.%s", module_name, word_name);
      TRAVERSE(c, in) {
        int x = trace_decode(*p);
        if(x == 0) {
          printf(" X");
        } else if(x == NIL_INDEX) {
          printf(" []");
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
      printf(", type = %s", show_type_all_short(c->expr_type));
    }
    printf(" x%d", c->n + 1);
    if(t >= entry->entry.in &&
       c->n + 1 == 0) {
      printf(" <-- WARNING: zero refcount\n");
    } else if(is_dep(c) && !c->expr.arg[0]) {
      printf(" <-- WARNING: broken dep\n");
    } else {
      printf("\n");
    }
  }

  // print sub-functions
  FOR_TRACE(c, entry) {
    if(is_user_func(c)) {
      cell_t *e = get_entry(c);
      if(e->entry.parent == entry) {
        printf("\n");
        print_bytecode(e);
      }
    }
  }
}

static
void condense(cell_t *entry) {
  cell_t *ret = NULL;
  int idx = 1;

  // calculate mapping
  FOR_TRACE(p, entry) {
    if(p->n < 0) p->func = NULL;
    if(p->func) {
      if(is_value(p) && p->value.type.exclusive == T_RETURN) {
        if(ret) ret->alt = trace_encode(idx);
        ret = p;
      } else {
        p->alt = trace_encode(idx);
      }
      idx += calculate_cells(p->size);
    } else {
      LOG("collapse %d", p - entry);
    }
  }

  // update references
  FOR_TRACE(tc, entry) {
    if(tc->func) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type.exclusive == T_RETURN) {
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
    if(p->func) {
      csize_t s = calculate_cells(p->size);
      if(!(is_value(p) && p->value.type.exclusive == T_RETURN)) {
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

static
void trace_replace_arg(cell_t *entry, cell_t *old, cell_t *new) {
  FOR_TRACE(tc, entry) {
    if(tc->func) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type.exclusive == T_RETURN) {
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

// runs after reduction to finish functions marked incomplete
void trace_final_pass(cell_t *entry) {
  // replace alts with trace cells
  cell_t *prev = NULL;

  // propagate types to asserts
  FOR_TRACE(p, entry) {
    if(p->func == func_assert &&
       p->expr_type.exclusive == T_ANY) {
      p->expr_type.exclusive = trace_type(&entry[trace_decode(p->expr.arg[0])]).exclusive;
    }
  }

  FOR_TRACE(p, entry) {
    if(FLAG(p->expr_type, T_INCOMPLETE)) {
      if(p->func == func_placeholder) { // convert a placeholder to ap or compose
        FLAG_CLEAR(p->expr_type, T_INCOMPLETE);
        trace_index_t left = trace_decode(p->expr.arg[0]);
        assert_error(left >= 0);
        if(closure_in(p) > 1 && trace_type(&entry[left]).exclusive == T_FUNCTION) {
          p->func = func_compose;
        } else {
          p->func = func_ap;
        }
        if(prev && prev->func == func_ap &&
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
          prev->func = p->func;
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
  condense(entry);

  // compile quotes
  FOR_TRACE(p, entry) {
    if(FLAG(p->expr_type, T_INCOMPLETE)) {
      if(p->func == func_exec) { // compile a quote
        FLAG_CLEAR(p->expr_type, T_INCOMPLETE);
        compile_quote(entry, p);
      }
    }
  }
}

// add an entry and all sub-entries to a module
static
void store_entries(cell_t *entry, cell_t *module) {
  module_set(module, string_seg(entry->word_name), entry);
  FOR_TRACE(c, entry) {
    if(c->func != func_exec) continue;
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
  if(FLAG(p->value.type, T_TRACED)) {
    if(p->alt) { // HACKy
      return p->alt;
    } else {
      return lookup_word(string_seg("??"));
    }
  }
  FLAG_SET(p->value.type, T_TRACED);
  seg_t name = path_name(path);
  return compile_entry(name, *context);
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

// convert symbol to C identifier
void command_ident(cell_t *rest) {
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
  cell_t *e = trace_start_entry(in, out);
  // make recursive return this entry
  (*entry)->alt = e; // ***
  *entry = e;

  bool context_write_graph = write_graph;
  if(entry_number(e) == graph_entry) write_graph = true;
  e->module_name = module_name(module);
  seg_t ident_seg = {
    .s = ident,
    .n = expand_sym(ident, LENGTH(ident), name)
  };
  e->word_name = seg_string(ident_seg); // TODO fix unnecessary alloc
  CONTEXT_LOG("compiling %s.%.*s at entry %d", e->module_name, name.n, name.s, entry_number(e));

  // parse
  cell_t *c = parse_expr(&toks, module, e);

  // compile
  fill_args(e, c);
  e->entry.alts = trace_reduce(e, &c);
  drop(c);
  trace_final_pass(e);
  trace_end_entry(e, NULL);

  // finish
  free_def(l);
  write_graph = context_write_graph;
  return true;
}

// replace variable c if there is a matching entry in a
void replace_var(cell_t *c, cell_t **a, csize_t a_n, cell_t *entry) {
  int x = c->value.tc.index;
  COUNTUP(j, a_n) {
    int y = trace_decode(a[j]);
    if(y == x) {
      int xn = a_n - j;
      cell_t *tc = &entry[xn];
      tc->value.type.exclusive = trace_type(trace_cell_ptr(c->value.tc)).exclusive;
      c->value.tc.index = xn;
      c->value.tc.entry = entry;
      return;
    }
  }

  { // diagnostics for fall through, which shouldn't happen
    CONTEXT_LOG("replace_var fall through: %d (%d)", CELL_INDEX(c), x);
    COUNTUP(j, a_n) {
      LOG("%d -> %d", trace_decode(a[j]), j);
    }
  }
}

static
void substitute_free_variables(cell_t *c, cell_t **a, csize_t a_in, cell_t *entry) {
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  FOLLOW(p, vl, tmp) {
    replace_var(p, a, a_in, entry);
  }
  clean_tmp(vl);
}

// matches:
// ___ f (1 -> 1) ___
// [0] var, type = ?f x1
// [1] __primitive.ap 0 -> X, type = f x1
// [2] return [ 1 ], type = @r x1
#define OPERAND(x) FLIP_PTR((cell_t *)(x))
static
bool is_tail(cell_t *e) {
  static const cell_t pattern[] = {
    [0] = {
      .func = func_value,
      .size = 2,
      .value = {
        .type = {
          .exclusive = T_FUNCTION,
          .flags = T_VAR }}},
    [1] = {
      .func = func_ap,
      .expr_type = {
        .exclusive = T_FUNCTION},
      .size = 2,
      .expr = {
        .out = 1,
        .arg = { OPERAND(1), 0 }}},
    [2] = {
      .func = func_value,
      .size = 2,
      .value = {
        .type = {
          .exclusive = T_RETURN },
        .ptr = { OPERAND(2) }}}
  };
  return
    e->entry.in == 1 &&
    e->entry.out == 1 &&
    e->entry.len == LENGTH(pattern) &&
    memcmp(pattern, e+1, sizeof(pattern)) == 0;
}

// all variable quote can be replaced with ap
static
bool is_ap(cell_t *e) {
  cell_t *code = e + 1;
  size_t in = e->entry.in;
  if(in >= e->entry.len ||
     e->entry.alts != 1) return false;
  cell_t *ap = &code[in];
  if((ap->func != func_ap &&
      ap->func != func_compose) ||
     closure_out(ap)) return false;
  cell_t *ret = &code[in + closure_cells(ap)];
  if(!is_value(ret) ||
     list_size(ret) != 1 ||
     trace_decode(ret->value.ptr[0]) != ap - code ||
     FLAG(ret->value.type, T_ROW) ||
     ret->value.type.exclusive != T_RETURN) return false;
  return true;
}

static
bool simplify_quote(cell_t *e, cell_t *parent_entry, cell_t *q) {
  cell_t **root = &q->expr.arg[closure_in(q)];
  if(is_tail(e)) {
    LOG("%d -> tail", q-parent_entry-1);
    trace_shrink(q, 2);
    q->func = func_ap;
    q->expr_type.exclusive = T_FUNCTION;
    q->expr_type.flags = 0;
    q->expr.out = 1;
    // q->expr.arg[0] stays the same
    q->expr.arg[1] = NULL;
    goto finish;
  } else if (e->entry.in + 1 == q->size && is_ap(e)) {
    LOG("%d -> ap", q-parent_entry-1);
    csize_t in = e->entry.in;
    assert_error(q->expr.out == 0);
    cell_t *code = e + 1;
    cell_t *ap = &code[in];
    csize_t args = closure_in(ap);
    if(args <= q->size) {
      // store arguments in alts
      COUNTUP(i, in) {
        code[in - 1 - i].alt = q->expr.arg[i];
      }

      trace_shrink(q, args);

      // look up arguments stored earlier
      COUNTUP(i, args) {
        trace_index_t x = trace_decode(ap->expr.arg[i]);
        q->expr.arg[i] = x == NIL_INDEX ? trace_encode(NIL_INDEX) : code[x].alt;
      }

      q->func = ap->func;
      q->expr_type.exclusive = T_FUNCTION;
      q->expr_type.flags = 0;
      goto finish;
    }
  }
  return false;
finish:
  trace_clear(e);
  remove_root(root);
  return true;
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
cell_t *compile_quote(cell_t *parent_entry, cell_t *q) {
  // set up
  csize_t in = closure_in(q);
  cell_t *e = trace_start_entry(in, 1);
  cell_t **fp = &q->expr.arg[in];
  assert_error(*fp);
  cell_t *c = *fp;
  assert_error(remove_root(fp));

  *fp = trace_encode(entry_number(e));

  CONTEXT_LOG("compiling quote %s.%s_%d at entry %d",
              parent_entry->module_name,
              parent_entry->word_name,
              (int)(q - parent_entry) - 1,
              entry_number(e));

  trace_allocate_vars(e, in);
  substitute_free_variables(c, q->expr.arg, in, e);

  // conversion
  csize_t len = function_out(c, true);
  //bool row = is_row_list(c);
  cell_t *ph = func(func_placeholder, len + 1, 1);
  arg(ph, &nil_cell);
  COUNTUP(i, len) {
    arg(ph, ref(c->value.ptr[i]));
  }
  drop(c);
  c = quote(ph);

  // compile
  e->entry.in += fill_args(e, c);
  e->entry.alts = trace_reduce(e, &c);
  assert_throw(c && NOT_FLAG(c->value.type, T_FAIL), "reduction failed");
  assert_error(e->entry.out);
  drop(c);

  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(q - parent_entry) - 1);
  trace_final_pass(e);
  trace_end_entry(e, parent_entry);

  if(simplify_quote(e, parent_entry, q)) return NULL;

  return e;
}

// decode a pointer to an index and return a trace pointer
static
cell_t *tref(cell_t *entry, cell_t *c) {
  int i = trace_decode(c);
  return i <= 0 ? NULL : &entry[i];
}

// get the return type
type_t trace_type(cell_t *c) {
  return is_value(c) ? c->value.type : c->expr_type;
}

// resolve types in each return in e starting at c, storing the resulting types in t
void resolve_types(cell_t *e, type_t *t) {
  csize_t out = e->entry.out;

  COUNTUP(i, out) {
    t[i].exclusive = T_BOTTOM;
  }

  // find first return
  cell_t *p = NULL;
  FOR_TRACE(tc, e) {
    if(trace_type(tc).exclusive == T_RETURN) {
      p = tc;
      break;
    }
  }
  if(!p) return;

  while(p) {
    COUNTUP(i, out) {
      int pt = trace_type(tref(e, p->value.ptr[i])).exclusive;
      if(t[i].exclusive == T_BOTTOM) {
        t[i].exclusive = pt;
      } else if(t[i].exclusive != pt &&
                pt != T_BOTTOM) {
        t[i].exclusive = T_ANY;
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
    *word_name = function_name(c->func);
  }
}

// print bytecode for a word, or all
void command_bc(cell_t *rest) {
  if(rest) {
    seg_t name = tok_seg(rest);
    CONTEXT("bytecode command");
    command_define(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(name, &m);
    if(e) {
      printf("\n");
      print_bytecode(e);
    }
  } else {
    print_all_bytecode();
    if(command_line) quit = true;
  }
}

// graph the given entry number
void command_gre(cell_t *rest) {
  if(!rest) {
    graph_entry = -1;
  } else {
    const char *s = rest->tok_list.location;
    if(is_num(s)) {
      graph_entry = atoi(s);
    } else {
      printf("graph_entry: requires an integer argument\n");
    }
  }
}
