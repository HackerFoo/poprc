/* Copyright 2012-2016 Dustin DeWeese
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
#include <assert.h>
#include <stdarg.h>

#include "gen/cells.h"
#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/parse.h"
#include "gen/word_table.h"
#include "gen/byte_compile.h"
#include "gen/lex.h"
#include "gen/module.h"
#include "gen/user_func.h"

#define FUNC_AP 1
#define MAX_SYMBOLS 64
#define ENTRY(name) [SYM_##name] = #name
static const char *symbol_index[MAX_SYMBOLS] = {
  ENTRY(False),
  ENTRY(True),
  ENTRY(IO),
  ENTRY(Dict)
};
#undef ENTRY

#define ENTRY(name) {(uintptr_t)#name, SYM_##name}
static pair_t symbols[MAX_SYMBOLS+1] = {
  {MAX_SYMBOLS, 4},
  ENTRY(Dict),
  ENTRY(False),
  ENTRY(IO),
  ENTRY(True)
};
#undef ENTRY

static char strings[1<<14];
static char *strings_top;

void print_symbols() {
  print_string_map(symbols);
}

const char *seg_string(seg_t s) {
  if((uintptr_t)((char *)(&strings + 1) - strings_top) < s.n + 1) return NULL;
  char *str = strings_top;
  memcpy(str, s.s, s.n);
  strings_top += s.n;
  *strings_top++ = '\0';
  return str;
}

const char *string_vprintf(const char *format, va_list ap) {
  size_t remaining = ((char *)(&strings + 1) - strings_top);
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
  assert(strings_top > strings);
  do {
    strings_top--;
  } while(strings_top > strings && strings_top[-1]);
}

int test_strings() {
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

cell_t *parse_word(seg_t w, cell_t *module, unsigned int n) {
  cell_t *c;
  cell_t *data = NULL;
  csize_t in = 0, out = 1;
  if(w.s[0] == '?' && w.n == 1) {
    c = var(T_ANY, NULL);
#if FUNC_AP
  } else if(w.n == 4 &&
            w.s[0] == 'a' && w.s[1] == 'p' &&
            char_class(w.s[2]) == CC_NUMERIC &&
            char_class(w.s[3]) == CC_NUMERIC) {
    in = w.s[2] - '0' + 1;
    out = w.s[3] - '0' + 1;
    c = func(func_ap, in, out);
#endif
  } else {
    cell_t *e = lookup_word(w);
    if(!e) e = module_lookup_compiled(w, &module);
    if(e) {
      if(e->entry.flags & ENTRY_PRIMITIVE) {
        if(e->func == func_placeholder) {
          c = func(func_placeholder, n + 1, 1);
          cell_t *tc = trace_alloc(n + 2);
          data = var_create(T_FUNCTION, tc, 0, 0);
        } else {
          c = func(e->func, e->entry.in, e->entry.out);
        }
      } else {
        c = func(e->func, e->entry.in + 1, e->entry.out);
        data = e;
      }
      in = e->entry.in;
      out = e->entry.out;
    } else {
      return NULL;
    }
  }
  COUNTUP(i, out-1) {
    cell_t *d = dep(ref(c));
    arg(c, d);
  }
  if(data) {
    arg(c, data);
  }
  return c;
}

void reverse_vector(cell_t *c) {
  csize_t n = val_size(c);
  COUNTUP(i, n/2) {
    val_t tmp = c->value.integer[i];
    c->value.integer[i] = c->value.integer[--n];
    c->value.integer[n] = tmp;
  }
}

cell_t *parse_vector(const cell_t **l) {
  const tok_list_t *tl;
  const char *s;
  const cell_t *t = *l;
  cell_t *c = vector(0);
  c->value.type.exclusive = T_INT; // for now
  while(t) {
    tl = &t->tok_list;
    s = tl->location;
    if(s[0] == ')') {
      t = tl->next;
      break;
    } else if(is_num(s)) {
      c = pushl_val(atoi(s), c);
      t = tl->next;
    } else {
      drop(c);
      return NULL;
    }
  }

  *l = t;
  reverse_vector(c);
  return c;
}

val_t fill_args(cell_t *r) {
  csize_t n = list_size(r);
  if(n < 1) return 0;
  cell_t *l = r->value.ptr[n-1];
  val_t i = 0;
  while(!closure_is_ready(l)) {
    cell_t *v = var(T_ANY, NULL);
    trace(v, v, tt_update);
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

#define MATCH_IF_0(p, label, cond, var, ...) \
  do {                                       \
    if(!(p) || !(cond)) goto label;          \
    var = p;                                 \
    p = p->tok_list.next;                    \
  } while(0)
#define MATCH_IF_1(p, label, cond, ...) \
  do {                                  \
    if(!(p) || !(cond)) goto label;     \
    cell_t *tmp = p;                    \
    p = p->tok_list.next;               \
    cell_free(tmp);                     \
  } while(0)

#define MATCH_IF(cond, ...) DISPATCH(MATCH_IF, 4, p, fail, cond , ##__VA_ARGS__)
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
  cell_t *p = *c;
  MATCH_IF(!is_reserved(tok_seg(p)), *name);
  MATCH_ONLY(":");
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
    cell_free(n);
    l->module_name = module_name;
    cell_t *old = module_set(m, name, l);
    assert(old == NULL); // TODO append defs?
    if((*err = check_def(l))) break;
  }
  *c = p;
  return m;
}

int test_parse_def() {
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
  cell_t *old = module_set(modules, *name, m);
  assert(old == NULL); // TODO append modules?
  if(modules) modules->n = PERSISTENT;
  *c = p;
  return !*err;
fail:
  if(!*err) *err = p;
  return false;
}

int test_parse_module() {
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
  if(n == 0) return &nil_cell;
  cell_t *l = make_list(n);
  COUNTUP(i, n) {
    l->value.ptr[i] = a[--n];
  }
  return l;
}

#define MAX_ARGS 64
cell_t *parse_expr(const cell_t **l, cell_t *module) {
  cell_t *arg_stack[MAX_ARGS]; // TODO use allocated storage
  cell_t *ph = NULL;
  unsigned int n = 0;
  const cell_t *t;

  while((t = *l)) {
    *l = t->tok_list.next;
    seg_t seg = tok_seg(t);

    switch(t->char_class) {

    case CC_NUMERIC:
      assert_throw(n < MAX_ARGS);
      arg_stack[n++] = int_val(atoi(seg.s));
      break;
    case CC_FLOAT:
    {
      char *end;
      double x = strtod(seg.s, &end);
      if(end != seg.s) {
        assert_throw(n < MAX_ARGS);
        arg_stack[n++] = float_val(x);
      }
      break;
    }

    case CC_SYMBOL:
      if(seg.n == 1 && *seg.s == ',') {
        if(n == 1) {
          if(closure_is_ready(arg_stack[0])) {
            arg_stack[1] = arg_stack[0];
            arg_stack[0] = &nil_cell;
            n = 2;
          } else {
            arg(arg_stack[0], &nil_cell);
          }
        }

        if(ph) { // TODO move this into array_to_list()
          assert_throw(n < MAX_ARGS);
          memmove(arg_stack + 1, arg_stack, n * sizeof(arg_stack[0]));
          arg_stack[0] = ph;
          ph = NULL;
          n++;
        }
        arg_stack[0] = array_to_list(arg_stack, n);
        n = 1;
        break;
      }
      // otherwise continue below
    case CC_ALPHA:
    case CC_VAR:
    {
      if(is_uppercase(*seg.s)) {
        assert_throw(n < MAX_ARGS);
        arg_stack[n++] = string_symbol(seg);
      } else {
        cell_t *c = parse_word(seg, module, n);
        if(!c) goto fail;
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
        assert_throw(n < MAX_ARGS);
        if(clear_ptr(c->func) == (void *)func_placeholder) {
          drop(ph); // HACK probably should chain placeholders somehow
          ph = c;
        } else {
          arg_stack[n++] = c;
          if(f) {
            csize_t in = closure_in(c);
            csize_t out = closure_out(c);
            COUNTUP(i, out) {
              assert_throw(n < MAX_ARGS);
              arg_stack[n++] = c->expr.arg[in+i];
            }
          }
        }
      }
    }

    case CC_BRACKET:
    if(seg.n == 1) {
      switch(*seg.s) {
      case ']':
        goto done;
      case '[':
      {
        cell_t *c = parse_expr(l, module);
        if(c) {
          assert_throw(n < MAX_ARGS);
          arg_stack[n++] = c;
        } else {
          goto fail;
        }
        break;
      }
      case '(':
      {
        cell_t *v = parse_vector(l);
        if(v) {
          assert_throw(n < MAX_ARGS);
          arg_stack[n++] = v;
        } else {
          goto fail;
        }
        break;
      }
      default:
        break;
      }
    }

    default:
      break;
    }
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
  COUNTUP(i, n) {
    drop(arg_stack[i]);
  }
  drop(ph);
  return NULL;
}

uintptr_t intern(seg_t sym) {
  const char *s = seg_string(sym);
  assert(s);
  pair_t *x = string_map_find(symbols, s);
  uintptr_t v;
  if(x) {
    v = x->second;
    strings_drop();
  } else {
    v = *map_cnt(symbols);
    assert_throw(v < MAX_SYMBOLS);
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

seg_t string_seg(const char *str) {
  seg_t seg = {str, strlen(str)};
  return seg;
}
