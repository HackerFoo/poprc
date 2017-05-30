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
#include <assert.h>
#include <inttypes.h>

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
void print_bytecode(cell_t *e) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;

  // word info (top line)
  printf("___ %s.%s (%d -> %d)", e->module_name, e->word_name, e->entry.in, e->entry.out);
  if(e->entry.alts != 1) {
    if(e->entry.alts == 0) {
      printf(" FAIL");
    } else {
      printf(" x%d", e->entry.alts);
    }
  }
  if(e->entry.rec) {
    printf(" rec");
  }
  printf(" ___\n");

  // body
  FOR_TRACE(c, start, end) {
    int t = c - start;
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
          printf(" %" PRIdPTR, trace_decode(c->value.ptr[i]));
        }
        printf(" ]");
      } else if(is_var(c)) { // variable
        printf(" var");
      } else { // value
        printf(" val %" PRIdPTR, c->value.integer[0]);
      }
      printf(", type = %s", show_type_all_short(c->value.type));
      if(can_have_alt && c->alt) printf(" -> %" PRIdPTR, trace_decode(c->alt));
    } else { // print a call
      const char *module_name = NULL, *word_name = NULL;
      if(!(c->expr_type.flags & T_INCOMPLETE)) trace_get_name(c, &module_name, &word_name);
      if(c->func == func_quote) printf(" quote");
      printf(" %s.%s", module_name, word_name);
      TRAVERSE(c, in) {
        trace_index_t x = trace_decode(*p);
        if(x == -1) {
          printf(" X");
        } else if(x == NIL_INDEX) {
          printf(" []");
        } else {
          printf(" %" PRIdPTR, x);
        }
      }
      if(closure_out(c)) {
        printf(" ->");
        TRAVERSE(c, out) {
          trace_index_t x = trace_decode(*p);
          if(x == -1) {
            printf(" X");
          } else {
            printf(" %" PRIdPTR, x);
          }
        }
      }
      printf(", type = %s", show_type_all_short(c->expr_type));
    }
    printf(" x%d", c->n + 1);
    if(t >= e->entry.in &&
       c->n + 1 == 0) {
      printf(" <-- WARNING: zero refcount\n");
    } else {
      printf("\n");
    }
  }

  // print sub-functions
  FOR_TRACE(c, start, end) {
    if(!(c->expr_type.flags & T_SUB)) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
    printf("\n");
    print_bytecode(e);
  }
}

static
void condense(cell_t *e) {
  cell_t
    *start = e + 1,
    *end = start + e->entry.len,
    *ret = NULL;
  trace_index_t idx = 0;

  // calculate mapping
  FOR_TRACE(p, start, end) {
    if(p->func) {
      if(is_value(p) && p->value.type.exclusive == T_RETURN) {
        if(ret) ret->alt = trace_encode(idx);
        ret = p;
      } else {
        p->alt = trace_encode(idx);
      }
      idx += calculate_cells(p->size);
    }
  }

  // update references
  FOR_TRACE(tc, start, end) {
    if(tc->func) {
      cell_t **e = is_user_func(tc) ? &tc->expr.arg[closure_in(tc)] : NULL;
      if(is_value(tc) &&
         tc->value.type.exclusive == T_RETURN) {
        COUNTUP(i, list_size(tc)) {
          cell_t **p = &tc->value.ptr[i];
          trace_index_t x = trace_decode(*p);
          if(x >= 0) *p = start[x].alt;
        }
      } else {
        TRAVERSE(tc, args, ptrs) {
          if(p != e) {
            trace_index_t x = trace_decode(*p);
            if(x >= 0) *p = start[x].alt;
          }
        }
      }
    }
  }
  // condense
  idx = 0;
  FOR_TRACE(p, start, end) {
    if(p->func) {
      csize_t s = calculate_cells(p->size);
      if(!(is_value(p) && p->value.type.exclusive == T_RETURN)) {
        p->alt = NULL;
      }
      if(idx < p - start) {
        cell_t *n = start + idx;
        memmove(n, p, s * sizeof(cell_t));
        memset(n + s, 0, (p - n) * sizeof(cell_t));
        p = n;
      }
      idx += s;
    }
  }
  e->entry.len = idx;
}

static
void trace_replace_arg(cell_t *e, cell_t *old, cell_t *new) {
  cell_t
    *start = e + 1,
    *end = start + e->entry.len;
  FOR_TRACE(tc, start, end) {
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
static
void trace_final_pass(cell_t *e) {
  // replace alts with trace cells
  cell_t
    *start = e + 1,
    *end = start + e->entry.len,
    *prev = NULL;

  FOR_TRACE(p, start, end) {
    if(p->expr_type.flags & T_INCOMPLETE) {
      if(p->func == func_exec) { // compile a specialized function
        p->expr_type.flags &= ~T_INCOMPLETE;
#if SPECIALIZE
        cell_t *se = compile_specialized(e, p);
        p->expr.arg[closure_in(p)] = trace_encode(se - trace_cells);
        p->expr_type.flags |= T_SUB;
#endif
      } else if(p->func == func_placeholder) { // convert a placeholder to ap or compose
        p->expr_type.flags &= ~T_INCOMPLETE;
        trace_index_t left = trace_decode(p->expr.arg[0]);
        assert(left >= 0);
        if(closure_in(p) > 1 && trace_type(&trace_cur[left]).exclusive == T_FUNCTION) {
          p->func = func_compose;
        } else {
          p->func = func_ap;
        }
        if(prev && prev->func == func_ap &&
           trace_decode(p->expr.arg[closure_in(p) - 1]) == prev - start &&
           prev->n == 0) {
          LOG("merging ap %d to %d\n", p - start, prev - start);
          csize_t
            p_in = closure_in(p),
            p_out = closure_out(p),
            p_size = closure_args(p);
          refcount_t p_n = p->n;
          cell_t *tmp = copy(p);
          cell_t
            *p_enc = trace_encode(p - start),
            *prev_enc = trace_encode(prev - start);
          trace_replace_arg(e, p_enc, prev_enc);
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
  condense(e);

  // compile quotes
  FOR_TRACE(p, start, end) {
    if(p->expr_type.flags & T_INCOMPLETE) {
      if(p->func == func_quote) { // compile a quote
        p->expr_type.flags &= ~T_INCOMPLETE;
        compile_quote(e, p);
      }
    }
  }
}

// count the maximum number of changed variables in recursive calls
static
uint8_t trace_recursive_changes(cell_t *e) {
  unsigned int changes = 0;
  const cell_t *encoded_entry = trace_encode(e - trace_cells);
  cell_t
    *start = e + 1,
    *end = start + e->entry.len;

  FOR_TRACE(p, start, end) {
    csize_t in;
    if(p->func == func_exec &&
       p->expr.arg[in = closure_in(p)] == encoded_entry) {
      unsigned int cnt = 0;
      COUNTUP(i, in) {
        if(trace_decode(p->expr.arg[i]) != (trace_index_t)(in - 1 - i)) cnt++;
      }

      // if cnt == 0, a recusive call has been made without modifying any arguments
      // so it will loop forever
      assert_throw(cnt, "infinite recursion");
      if(cnt > changes) changes = cnt;
    }
  }
  return changes;
}

// add an entry and all sub-entries to a module
static
void store_entries(cell_t *e, cell_t *module) {
  size_t count = e->entry.len;
  cell_t *start = e + 1;
  cell_t *end = start + count;
  module_set(module, string_seg(e->word_name), e);
  FOR_TRACE(c, start, end) {
    if(c->func != func_quote) continue;
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c)])];
    store_entries(e, module);
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
  if(p->value.type.flags & T_TRACED) {
    if(p->alt) { // HACKy
      return p->alt;
    } else {
      return lookup_word(string_seg("??"));
    }
  }
  p->value.type.flags |= T_TRACED;
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

void command_expsym(cell_t *rest) {
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
}

bool compile_word(cell_t **entry, seg_t name, cell_t *module, csize_t in, csize_t out) {
  cell_t *l;
  char ident[64]; // ***
  if(!entry || !(l = *entry)) return false;
  if(!is_list(l)) return true;
  if(is_empty_list(l)) return false;

  // make recursive return this entry
  (*entry)->alt = trace_ptr;

  const cell_t *toks = l->value.ptr[0]; // TODO handle list_size(l) > 1

  // set up
  rt_init();
  cell_t *e = *entry = trace_start();
  bool context_write_graph = write_graph;
  if(e - trace_cells == graph_entry) write_graph = true;
  e->n = PERSISTENT;
  e->module_name = module_name(module);
  seg_t ident_seg = {
    .s = ident,
    .n = expand_sym(ident, LENGTH(ident), name)
  };
  e->word_name = seg_string(ident_seg); // TODO fix unnecessary alloc
  e->entry.in = in;
  e->entry.out = out;
  e->entry.len = 0;
  LOG("compiling %s.%.*s at entry %d\n", e->module_name, name.n, name.s, TRACE_INDEX(e));

  // parse
  e->entry.flags = ENTRY_NOINLINE;
  e->func = func_exec;
  cell_t *c = parse_expr(&toks, module);

  // compile
  fill_args(c);
  e->entry.alts = trace_reduce(&c);
  drop(c);
  trace_stop();
  e->entry.len = trace_ptr - trace_cur;
  trace_final_pass(e);
  e->entry.flags &= ~ENTRY_NOINLINE;
  e->entry.rec = trace_recursive_changes(e);

  // finish
  free_def(l);
  write_graph = context_write_graph;
  return true;
}

// replace variable c if there is a matching entry in a
void replace_var(cell_t *c, cell_t **a, csize_t a_n, cell_t *entry) {
  trace_index_t x = (c->value.ptr[0] - entry) - 1;
  COUNTUP(j, a_n) {
    trace_index_t y = trace_decode(a[j]);
    if(y == x) {
      trace_cur[j].value.type.exclusive = trace_type(c->value.ptr[0]).exclusive;
      c->value.ptr[0] = &trace_cur[a_n - 1 - j];
      return;
    }
  }
  c->value.ptr[0] = trace_alloc(2);
  //assert(false);
}

static
void substitute_free_variables(cell_t *c, cell_t **a, csize_t a_in, cell_t *parent_entry) {
  cell_t *vl = 0;
  trace_var_list(c, &vl);
  FOLLOW(p, vl, tmp) {
    replace_var(p, a, a_in, parent_entry);
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
      .func = func_placeholder,
      .expr_type = {
        .exclusive = T_FUNCTION,
        .flags = T_INCOMPLETE },
      .size = 2,
      .expr = {
        .out = 1,
        .arg = { OPERAND(0), 0 }}},
    [2] = {
      .func = func_value,
      .size = 2,
      .value = {
        .type = {
          .exclusive = T_RETURN,
          .flags = T_ROW },
        .ptr = { OPERAND(1) }}}
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
  COUNTUP(i, in) {
    if(!is_var(&code[i])) return false;
  }
  if(!is_value(&code[in]) ||
     code[in].value.type.exclusive != T_RETURN) return false;
  return true;
}

static
bool simplify_quote(cell_t *e, cell_t *parent_entry, cell_t *q) {
  cell_t **root = &q->expr.arg[closure_in(q)];
  if(is_tail(e)) {
    LOG("%d -> tail\n", q-parent_entry-1);
    trace_shrink(q, 2);
    q->func = func_ap;
    q->expr_type.exclusive = T_FUNCTION;
    q->expr_type.flags = 0;
    q->expr.out = 1;
    // q->expr.arg[0] stays the same
    q->expr.arg[1] = NULL;
    goto finish;
  } else if (is_ap(e)) {
    LOG("%d -> ap\n", q-parent_entry-1);
    csize_t in = e->entry.in;
    assert(in + 1 == q->size && q->expr.out == 0);
    csize_t out = e->entry.out;
    cell_t *code = e + 1;
    cell_t *ret = &code[in];
    csize_t args = out + 1;
    if(args <= q->size) {
      // store arguments in alts
      COUNTUP(i, in) {
        code[in - 1 - i].alt = q->expr.arg[i];
      }

      trace_shrink(q, args);

      // look up arguments stored earlier
      COUNTUP(i, out) {
        q->expr.arg[i] = code[trace_decode(ret->value.ptr[out - 1 - i])].alt;
      }
      q->expr.arg[out] = trace_encode(NIL_INDEX);

      q->func = ret->value.type.flags & T_ROW ? func_compose : func_ap;
      q->expr_type.exclusive = T_FUNCTION;
      q->expr_type.flags = 0;
      goto finish;
    }
  }
  return false;
finish:
  FLAG_CLEAR(q->expr.flags, FLAGS_USER_FUNC);
  trace_clear(e);
  trace_ptr = trace_cur;
  remove_root(root);
  return true;
}

// takes a parent entry and offset to a quote, and creates an entry from compiling the quote
cell_t *compile_quote(cell_t *parent_entry, cell_t *q) {
  // set up
  cell_t *e = trace_start();
  csize_t in = closure_in(q);
  cell_t **fp = &q->expr.arg[in];
  assert(*fp);
  cell_t *c = *fp;
  e->n = PERSISTENT;
  e->entry.len = 0;
  e->entry.flags = ENTRY_NOINLINE | ENTRY_QUOTE | (is_row_list(c) ? ENTRY_ROW : 0);
  e->func = func_exec;

  trace_allocate_vars(in);
  substitute_free_variables(c, q->expr.arg, in, parent_entry);

  // compile
  e->entry.in = in + fill_args(c);
  e->entry.alts = trace_reduce(&c);
  assert_throw(c && !(c->value.type.flags & T_FAIL), "reduction failed");
  e->entry.out = function_out(c, true);
  assert(e->entry.out);
  drop(c);
  trace_stop();
  e->entry.flags &= ~ENTRY_NOINLINE;
  e->entry.rec = trace_recursive_changes(e);
  e->entry.len = trace_ptr - trace_cur;

  if(simplify_quote(e, parent_entry, q)) return NULL;

  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(q - parent_entry) - 1);
  trace_final_pass(e);
  assert(remove_root(fp));

  q->expr.arg[closure_in(q)] = trace_encode(e - trace_cells);
  q->expr_type.flags |= T_SUB;
  return e;
}

#if SPECIALIZE
cell_t *compile_specialized(cell_t *parent_entry, cell_t *tc) {
  // set up
  cell_t *e = trace_start();
  dont_specialize = true;

  csize_t
    in = closure_in(tc),
    out = closure_out(tc) + 1;
  cell_t **fp = &tc->expr.arg[in];
  cell_t *c = *fp;
  assert(c);
  assert(remove_root(fp));
  e->n = PERSISTENT;
  e->entry.len = 0;
  e->entry.flags = ENTRY_NOINLINE;
  e->func = func_exec;

  trace_allocate_vars(in);
  substitute_free_variables(c, q->expr.arg, in, parent_entry);

  FLAG_CLEAR(c->expr.flags, FLAGS_RECURSIVE); // always expand
  initial_word = copy(c);

  // compile
  e->entry.in = in;
  e->entry.out = out;
  cell_t *q = quote(c);
  e->entry.alts = trace_reduce(&q);
  drop(q);
  dont_specialize = false;
  trace_stop();
  e->entry.flags &= ~ENTRY_NOINLINE;
  e->entry.rec = trace_recursive_changes(e);
  e->entry.len = trace_ptr - trace_cur;
  e->module_name = parent_entry->module_name;
  e->word_name = string_printf("%s_%d", parent_entry->word_name, (int)(tc - parent_entry) - 1);
  trace_final_pass(e);

  return e;
}
#endif

// decode a pointer to an index and return a trace pointer
static
cell_t *tref(cell_t *entry, cell_t *c) {
  trace_index_t i = trace_decode(c);
  return i < 0 ? NULL : &entry[i+1];
}

// get the return type
type_t trace_type(cell_t *c) {
  return is_value(c) ? c->value.type : c->expr_type;
}

// resolve types in each return in e starting at c, storing the resulting types in t
void resolve_types(cell_t *e, type_t *t) {
  cell_t *code = e + 1;
  size_t count = e->entry.len;
  csize_t out = e->entry.out;

  // find first return
  cell_t *l = NULL;
  RANGEUP(i, e->entry.in, count) {
    if(trace_type(&code[i]).exclusive == T_RETURN) {
      l = &code[i];
      break;
    }
  }
  if(!l) return;

  // first store types from c
  COUNTUP(i, out) {
    t[i].exclusive = trace_type(tref(e, l->value.ptr[i])).exclusive;
  }

  // then resolve the rest
  cell_t *p = tref(e, l->alt);
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
    cell_t *e = &trace_cells[trace_decode(c->expr.arg[closure_in(c)])]; // <- differs from get_name()
    *module_name = e->module_name;
    *word_name = e->word_name;
  } else {
    *module_name = PRIMITIVE_MODULE_NAME;
    *word_name = function_name(c->func);
  }
}

// print bytecode for a word
void command_bytecode(cell_t *rest) {
  if(rest) {
    command_def(rest);
    cell_t
      *m = eval_module(),
      *e = module_lookup_compiled(tok_seg(rest), &m);
    if(e) {
      printf("\n");
      print_bytecode(e);
    }
  }
}

void command_graph_entry(cell_t *rest) {
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
