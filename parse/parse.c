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
#include <stdio.h>

#if INTERFACE
#include <stdarg.h>
#endif

#include "startle/error.h"
#include "startle/log.h"
#include "startle/test.h"
#include "startle/support.h"
#include "startle/map.h"

#include "cells.h"
#include "rt.h"
#include "special.h"
#include "parse/parse.h"
#include "ir/compile.h"
#include "ir/trace.h"
#include "parse/tok.h"
#include "parse/lex.h"
#include "module.h"
#include "user_func.h"
#include "list.h"
#include "var.h"

// List of predefined symbols in value order.
STATIC_ALLOC_DEPENDENT(symbol_index, const char *, symbols_size);
#define ENTRY(name) [SYM_##name] = #name
static const char *init_symbol_index[] = {
  ENTRY(False),
  ENTRY(True),
  ENTRY(IO),
  ENTRY(Dict),
  ENTRY(Something),
  ENTRY(Array),
  ENTRY(File)
};
#undef ENTRY

// List of predefined symbols sorted alphabetically.
STATIC_ALLOC(symbols, pair_t, 64);
#define ENTRY(name) {(uintptr_t)#name, SYM_##name}
static pair_t init_symbols[] = {
  {0, 7},
  ENTRY(Array),
  ENTRY(Dict),
  ENTRY(False),
  ENTRY(File),
  ENTRY(IO),
  ENTRY(Something),
  ENTRY(True)
};
#undef ENTRY

STATIC_ALLOC(strings, char, 1 << 14);
static char *strings_top;

void print_symbols() {
  print_string_map(symbols);
}

const char *seg_string(seg_t s) {
  assert_throw((uintptr_t)((strings + strings_size) - strings_top) >= s.n + 1,
               "`strings` too small");
  char *str = strings_top;
  memcpy(str, s.s, s.n);
  strings_top += s.n;
  *strings_top++ = '\0';
  return str;
}

const char *string_vprintf(const char *format, va_list ap) {
  size_t remaining = ((strings + strings_size) - strings_top);
  if(remaining > 1) {
    char *str = strings_top;
    int n = vsnprintf(str, remaining, format, ap);
    if(n < 0) return NULL;
    strings_top += n + 1;
    return str;
  } else return NULL;
}

const char *string_printf(const char *format, ...) {
 va_list valist;
 va_start(valist, format);
 const char *ret = string_vprintf(format, valist);
 va_end(valist);
 return ret;
}

void strings_drop() {
  assert_error(strings_top > strings);
  do {
    strings_top--;
  } while(strings_top > strings && strings_top[-1]);
}

TEST(strings) {
  const char *start = strings_top;
  seg_t s = {"Test", 4};
  const char *test = seg_string(s);
  if(strcmp(s.s, test) != 0) return -1;
  strings_drop();
  if(start != strings_top) return -2;
  return 0;
}

void parse_init() {
  strings_top = strings;
  memcpy(symbol_index, init_symbol_index, sizeof(init_symbol_index));
  memcpy(symbols, init_symbols, sizeof(init_symbols));
  symbols[0].first = symbols_size - 1;
}

bool is_num(char const *str) {
  return char_class(str[0]) == CC_NUMERIC ||
    (str[0] == '-' && char_class(str[1]) == CC_NUMERIC);
}

cell_t *lookup_word(seg_t w) {
  pair_t *res = seg_map_find(primitive_module, w);
  if(res) {
    return (cell_t *)res->second;
  } else {
    return NULL;
  }
}

bool match_param_word(const char *pre, seg_t seg, csize_t *in, csize_t *out) {
  const char *end = seg_end(seg);
  const char *s = seg.s;
  while(*pre) {
    if(s >= end ||
       *pre != *s) return false;
    s++;
    pre++;
  }
  switch(end - s) {
  case 2:
    if(char_class(s[1]) != CC_NUMERIC) return false;
    *out = s[1] - '0';
  case 1:
    if(char_class(s[0]) != CC_NUMERIC) return false;
    *in = s[0] - '0';
  case 0:
    return true;
  default:
    return false;
  }
}

cell_t *parse_word(seg_t w, cell_t *module, unsigned int n, tcell_t *entry) {
  cell_t *c;
  cell_t *data = NULL;
  csize_t in = 0, out = 1;
  if(w.s[0] == '?' && w.n == 1) {
    c = param(T_ANY, entry);
  } else if(in = 1, out = 1, match_param_word("ap", w, &in, &out)) {
    c = func(OP_ap, ++in, ++out);
  } else if(in = 1, out = 1, match_param_word("comp", w, &in, &out)) {
    in += 2;
    c = func(OP_compose, in, ++out);
  } else if(in = 1, out = 1, match_param_word("external", w, &in, &out)) {
    c = func(OP_external, ++in, out);
  } else {
    cell_t *e = lookup_word(w);
    if(!e) e = module_lookup_compiled(w, &module);
    if(e) {
      in = e->entry.in;
      out = e->entry.out;
      if(FLAG(*e, entry, PRIMITIVE)) {
        if(e->op == OP_placeholder) {
          c = func(OP_placeholder, n + 1, 1);
          int x = trace_alloc(entry, n + 2);
          in = n;
          out = 1;
          data = var_create(T_LIST, tc_get(entry, x), 0, 0);
        } else {
          c = func(e->op, e->entry.in, e->entry.out);
        }
      } else {
        c = func(OP_exec, e->entry.in + 1, e->entry.out);
        data = (cell_t *)tcell_entry(e);
      }
    } else {
      return NULL;
    }
  }
  if(in) {
    c->expr.arg[0] = (cell_t *)(intptr_t)(in - 1);
  } else {
    closure_set_ready(c, true);
  }
  TRAVERSE(c, out) {
    *p = dep(c);
  }
  refn(c, out-1);
  if(data) {
    c->expr.arg[in] = data;
  }
  return c;
}

val_t fill_args(tcell_t *entry, cell_t *l) {
  if(!l) return 0;
  val_t i = 0;
  while(!closure_is_ready(l)) {
    cell_t *v = param(T_ANY, entry);
    arg(l, v);
    ++i;
  }
  return i;
}

const char *reserved_words[] = {
  "module",
  ":"
};

bool match(const cell_t *c, const char *str) {
  return c && segcmp(str, tok_seg(c)) == 0;
}

bool match_class(const cell_t *c, char_class_t cc,
                 uint min_length, uint max_length) {
  return
    c &&
    c->char_class == cc &&
    c->tok_list.length >= min_length &&
    c->tok_list.length <= max_length;
}

long int parse_num(const cell_t *c) {
  return strtol(c->tok_list.location, NULL, 0);
}

#define MATCH_IF_4(p, label, cond, var)      \
  do {                                       \
    if(!(p) || !(cond)) goto label;          \
    var = p;                                 \
    p = p->tok_list.next;                    \
  } while(0)
#define MATCH_IF_3(p, label, cond)      \
  do {                                  \
    if(!(p) || !(cond)) goto label;     \
    cell_t *tmp = p;                    \
    p = p->tok_list.next;               \
    cell_free(tmp);                     \
  } while(0)

#define MATCH_IF(cond, ...) DISPATCH(MATCH_IF, p, fail, cond , ##__VA_ARGS__)
#define MATCH_ONLY(str, ...) MATCH_IF(match(p, str) , ##__VA_ARGS__)

bool is_reserved(seg_t s) {
  FOREACH(i, reserved_words) {
    if(segcmp(reserved_words[i], s) == 0) return true;
  }
  return false;
}

cell_t *list_insert(cell_t *c, cell_t *x) {
  c = expand(c, 1);
  c->value.ptr[list_size(c) - 1] = x;
  return c;
}

bool parse_rhs_expr(cell_t **c, cell_t **res) {
  cell_t *head = *c;
  cell_t **it = &head;
  cell_t *r = NULL;
  if(!*it) goto fail;
  const uintptr_t left_indent = tok_indent(*it);
  if(left_indent == 0) goto done;

  r = empty_list();

  do {
    const char *current_line = (*it)->tok_list.line;
    const uintptr_t indent = tok_indent(*it);
    if(indent < left_indent) {
      goto done;
    } else if(indent == left_indent) {
      r = list_insert(r, *it);
      cell_t **end = it;
      it = &(*it)->tok_list.next;
      *end = NULL;
    } else {
      it = &(*it)->tok_list.next;
    }
    while(*it && (*it)->tok_list.line == current_line) {
      it = &(*it)->tok_list.next;
    }
  } while(*it);
done:
  if(*c == *it) {
    return false;
  } else {
    r->n = PERSISTENT;
    *res = r;
    *c = *it;
    *it = NULL;
    return true;
  }
fail:
  *c = *it;
  return false;
}

bool parse_def(cell_t **c, cell_t **name, cell_t **l) {
  cell_t *p = *c, *colon;
  MATCH_IF(!is_reserved(tok_seg(p)), *name);
  MATCH_ONLY(":", colon);
  // apply attributes after colon to the name
  (*name)->tok_list.attributes |= colon->tok_list.attributes;
  if(!parse_rhs_expr(&p, l)) goto fail;
  *c = p;
  return true;
fail:
  return false;
}

cell_t *check_reserved(cell_t *p) {
  UNUSED cell_t *keep;
  while(p) {
    MATCH_IF(!is_reserved(tok_seg(p)), keep);
  }
  return NULL;
fail:
  return p;
}

bool has_IO(const cell_t *p) {
  UNUSED const cell_t *keep;
  while(p) {
    MATCH_IF(segcmp("IO", tok_seg(p)) != 0, keep);
  }
  return false;
fail:
  return true;
}

cell_t *check_def(cell_t *l) {
  // check expression types
  cell_t *p = NULL;
  UNUSED cell_t *keep;
  csize_t n = list_size(l);
  if(n > 0) {
    if(segcmp("module", tok_seg(l->value.ptr[0])) == 0) { // module expression
      COUNTUP(i, n) {
        p = l->value.ptr[i];
        MATCH_ONLY("module", keep);
        MATCH_IF(!is_reserved(tok_seg(p)), keep);
        if(p) goto fail;
      }
    } else { // concatenative expression
      COUNTUP(i, n) {
        p = check_reserved(l->value.ptr[i]);
        if(p) goto fail;
      }
    }
  }
  return NULL;
fail:
  return p;
}

cell_t *parse_defs(cell_t **c, const char *module_name, cell_t **err) {
  cell_t
    *l = NULL,
    *m = make_module(),
    *n = NULL,
    *p = *c;
  while(parse_def(&p, &n, &l)) {
    seg_t name = tok_seg(n);
    l->module_name = module_name;
    l->value.attributes = n->tok_list.attributes | scan_attributes(l);
    cell_free(n);
    UNUSED cell_t *old = module_set(m, name, l);
    assert_error(old == NULL, TODO " merge defs");
    if((*err = check_def(l))) break;
  }
  *c = p;
  return m;
}

TEST(parse_def) {
  cell_t *p = lex("word: hi there this\n"
                  "        is the first definition\n"
                  "      and this\n"
                  "_comment_ is the\n"
                  "        second\n"
                  "      oh hey heres\n"
                  "        the third\n"
                  "          one\n"
                  "another:\n"
                  " oh heres\n"
                  "  another\n"
                  " again\n"
                  // "and.more: stuff, listed on, one line\n"
                  "some.modules:\n"
                  " module one\n"
                  " module two\n"
                  " module three\n", 0);
  cell_t *err = NULL;
  cell_t *m = parse_defs(&p, NULL, &err);
  print_defs(m);
  free_defs(m);
  return err ? -1 : 0;
}

bool parse_module(cell_t **c, seg_t *name, cell_t **err) {
  cell_t *p = *c, *n = NULL;
  MATCH_ONLY("module");
  MATCH_IF(true, n);
  MATCH_ONLY(":");
  *name = tok_seg(n);
  cell_free(n);
  const char *strname = seg_string(*name); // TODO remove redundant string allocation
  cell_t *m = parse_defs(&p, strname, err);
  UNUSED cell_t *old = module_set(modules, *name, m);
  assert_error(old == NULL); // TODO append modules?
  if(modules) modules->n = PERSISTENT;
  *c = p;
  return !*err;
fail:
  if(!*err) *err = p;
  return false;
}

TEST(parse_module) {
  cell_t *orig_modules = modules;
  modules = make_module();
  cell_t *p = lex("module a:\n"
                  "f1: the first word\n"
                  "f2: the\n"
                  "      second one\n"
                  "f3:\n"
                  " number three\n"
                  "module b:\n"
                  "f4: heres another\n", 0);
  cell_t *e = NULL;
  seg_t n;
  char *s = "Loaded modules (";
  while(parse_module(&p, &n, &e)) {
    printf("%s%.*s", s, (int)n.n, n.s);
    s = ", ";
  }
  printf(")\n");

  print_modules();
  free_modules();
  closure_free(modules);
  modules = orig_modules;
  return e ? -1 : 0;
}

bool is_uppercase(char c) {
  return c >= 'A' && c <= 'Z';
}

cell_t *array_to_list(cell_t **a, csize_t n) {
  cell_t *l = make_list(n);
  COUNTUP(i, n) {
    l->value.ptr[i] = a[--n];
  }
  return l;
}

cell_t *parse_expr(const cell_t **l, cell_t *module, tcell_t *entry) {
  return parse_subexpr(l, module, entry, 0);
}

#define MAX_ARGS 32
cell_t *push_arg(cell_t **stack, unsigned int *n, seg_t seg, cell_t *c) {
  assert_throw(*n < MAX_ARGS);
  c->src = seg;
  stack[(*n)++] = c;
  return c;
}

#define MAX_PARSE_DEPTH 32
cell_t *parse_subexpr(const cell_t **l, cell_t *module, tcell_t *entry, int depth) {
  cell_t *arg_stack[MAX_ARGS]; // TODO use allocated storage
  cell_t *ph = NULL;
  unsigned int n = 0;
  const cell_t *t;
  seg_t seg;

  assert_error(depth < MAX_PARSE_DEPTH, "too many nested quotes");

  while((t = *l)) {
    *l = t->tok_list.next;
    seg = tok_seg(t);

    switch(t->char_class) {

    case CC_NUMERIC:
    {
      push_arg(arg_stack, &n, seg, int_val(strtol(seg.s, NULL, 0)));
    } break;

    case CC_FLOAT:
    {
      char *end;
      double x = strtod(seg.s, &end);
      if(end != seg.s) {
        push_arg(arg_stack, &n, seg, float_val(x));
      }
    } break;

    case CC_STRING:
    {
      push_arg(arg_stack, &n, seg,
               make_unescaped_string((seg_t) { .s = seg.s + 1,
                                               .n = seg.n - 2 }));
    } break;

    case CC_SYMBOL:
    case CC_ALPHA:
    case CC_VAR:
    {
      if(is_uppercase(*seg.s)) {
        push_arg(arg_stack, &n, seg, string_symbol(seg));
      } else {
        cell_t *c = parse_word(seg, module, n, entry);
        if(!c) {
          LOG("parse failure: %.*s", seg.n, seg.s);
          log_fail(seg);
          goto fail;
        }
        bool f = !is_value(c);
        if(f) {
          while(n && !closure_is_ready(c)) {
            arg(c, arg_stack[--n]);
          }
          if(ph) {
            csize_t in = closure_in(ph);
            while(!closure_is_ready(c)) {
              ph = expand_deps(ph, 1);
              arg(c, ph->expr.arg[in] = dep(ref(ph)));
            }
          }
        }
        if(c->op == OP_placeholder) {
          drop(ph); // HACK probably should chain placeholders somehow
          ph = c;
          c->src = seg;
        } else {
          push_arg(arg_stack, &n, seg, c);
          if(f) {
            TRAVERSE(c, out) {
              push_arg(arg_stack, &n, seg, *p);
            }
          }
        }
      }
    } break;

    case CC_COMMA:
    case CC_BRACKET: // TODO .src for lists
    {
      if(seg.n == 1) {
        switch(*seg.s) {
        case ',':
        case ']':
          if(depth) {
            *l = t;
            goto done;
          } else {
            LOG("parse failure, mismatched closing bracket or comma");
            goto fail;
          }
        case '[':
        {
          cell_t *c;
          char ch;
          do {
            c = parse_subexpr(l, module, entry, depth + 1);
            if(!c) {
              LOG("parse failure");
              goto fail;
            }
            ch = *l ? seg_char(tok_seg(*l)) : '\0';
            *l = (*l)->tok_list.next;
            push_arg(arg_stack, &n, seg, c);
          } while(*l && ch == ',');
          if(ch != ']') {
            LOG("parse failure");
            goto fail;
          }
        } break;
        case '(':
        case ')':
          LOG("parse failure - vectors unsupported");
          goto fail;
          break;
        default:
          break;
        }
      }
    } break;

    default:
      break;
    }
  }

  if(depth) {
    LOG("parse failure, missing closing bracket");
    goto fail;
  }

done:
  if(ph) {
    assert_throw(n < MAX_ARGS);
    memmove(arg_stack + 1, arg_stack, n * sizeof(arg_stack[0]));
    arg_stack[0] = ph;
    n++;
  }
  return array_to_list(arg_stack, n);
fail:
  log_fail(seg);
  COUNTUP(i, n) {
    drop(arg_stack[i]);
  }
  drop(ph);
  return NULL;
}

uintptr_t intern(seg_t sym) {
  const char *s = seg_string(sym);
  assert_error(s);
  pair_t *x = string_map_find(symbols, s);
  uintptr_t v;
  if(x) {
    v = x->second;
    strings_drop();
  } else {
    v = *map_cnt(symbols);
    assert_throw(v < symbols_size, "`symbols` too small");
    pair_t p = {(uintptr_t)s, v};
    string_map_insert(symbols, p);
    symbol_index[v] = s;
  }
  return v;
}

cell_t *string_symbol(seg_t sym) {
  return symbol(intern(sym));
}

const char *symbol_string(val_t x) {
  if(x >= 0 && x < (val_t)*map_cnt(symbols)) {
    return symbol_index[x];
  } else {
    return NULL;
  }
}

FORMAT(symbol, 'Y') {
  const char *str = symbol_string(i);
  if(str) {
    printf("%s", str);
  } else {
    printf("UnknownSymbol");
  }
}

cell_t *parse_expr_string(const char *expr) {
  cell_t *p = lex(expr, 0);
  cell_t *m = parse_expr((const cell_t **)&p, NULL, NULL);
  free_toks(p);
  return m;
}

bool parse_numeric_args(cell_t *l, int *arg, int n) {
  while(n--) {
    if(l && l->char_class == CC_NUMERIC) {
      *arg++ = strtol(l->tok_list.location, NULL, 0);
      l = l->tok_list.next;
    } else {
      return false;
    }
  }
  return true;
}

#define MATCH_STRING(p, s, end)                 \
  (end - (p) >= (int)(LENGTH(s) - 1) &&         \
   strncmp((p), (s), LENGTH(s) - 1) == 0 &&     \
   ((p) += LENGTH(s) - 1, true))

void attribute_parser(const char *s, const char *e, bool after_newline, attr_t *attr_before, attr_t *attr_after) {
  while(s < e) {
    while(ONEOF(*s, ' ', '\t')) s++;
    if(MATCH_STRING(s, "{hide}", e)) {
      if(after_newline) {
        *attr_after |= ATTR_HIDE;
      } else {
        *attr_before |= ATTR_HIDE;
      }
    } else {
      s++;
    }
  }
}

attr_t scan_attributes(const cell_t *l) {
  attr_t attr = 0;
  COUNTUP(i, list_size(l)) {
    const cell_t *p = l->value.ptr[i];
    while(p) {
      attr |= p->tok_list.attributes;
      p = p->tok_list.next;
    }
  }
  return attr;
}
