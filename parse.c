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
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "gen/rt.h"
#include "gen/primitive.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/parse.h"

word_entry_t user_word_table[64] = {{"", NULL, 0, 0, NULL}};
unsigned int const user_word_table_length = LENGTH(user_word_table);
word_entry_t *new_user_word_entry = user_word_table;

#define MAX_SYMBOLS 64
static uint32_t symbol_index[MAX_SYMBOLS] = {0, 6, 11, 14};
static char symbol_strings[4096] = "false\0true\0IO\0Dict";
#define ENTRY(i, name) {(uintptr_t)&symbol_strings[(i)], SYM_##name}
static pair_t symbols[MAX_SYMBOLS+1] = {
  {MAX_SYMBOLS, 4},
  ENTRY(14, DICT),
  ENTRY(11, IO),
  ENTRY(0,  FALSE),
  ENTRY(6,  TRUE)
};
#undef ENTRY
uint32_t symbol_strings_n = 19;

// for when index declaration is missing for some reason e.g. clang on Android
char *index(const char *s, int c);

bool is_num(char const *str) {
  return char_class(str[0]) == CC_NUMERIC ||
    (str[0] == '-' && char_class(str[1]) == CC_NUMERIC);
}

word_entry_t *lookup_word(seg_t w) {
  word_entry_t *res =
    lookup(word_table,
           WIDTH(word_table),
           word_table_length,
           w);
  if(!res) res =
    lookup_linear(user_word_table,
                  WIDTH(user_word_table),
                  user_word_table_length,
                  w);
  return res;
}
/*
cell_t *word(char const *w) {
  csize_t in, out;
  return parse_word(w, &in, &out);
}
*/
cell_t *parse_word(seg_t w,
                   csize_t *in,
                   csize_t *out,
                   cell_t **data) {
  cell_t *c;
  *data = NULL;
  if(is_num(w.s)) {
    c = val(atoi(w.s));
    *in = 0;
    *out = 1;
  } else if(w.s[0] == '?') {
    c = var(T_ANY);
    *in = 0;
    *out = 1;
  } else if(w.n == 4 &&
            w.s[0] == 'a' && w.s[1] == 'p' &&
            char_class(w.s[2]) == CC_NUMERIC &&
            char_class(w.s[3]) == CC_NUMERIC) {
    *in = w.s[2] - '0' + 1;
    *out = w.s[3] - '0' + 1;
    c = func(func_ap, *in, *out);
  } else {
    word_entry_t *e = lookup_word(w);
    /* disallow partial matches */
    if(!e ||
       w.n != strnlen(e->name, sizeof_field(word_entry_t, name))) {
      // trace the name ***
      c = func(func_placeholder, 0, 1);
      *in = 0;
      *out = 1;
    } else {
      if(e->data) {
        c = func(e->func, e->in + 1, e->out);
        *data = e->data;
      } else {
        c = func(e->func, e->in, e->out);
      }
      *in = e->in;
      *out = e->out;
    }
  }
  return c;
}

char_class_t char_class(char c) {
  if(c <= ' ' || c > '~')
    return CC_NONE;
  if(c >= '0' && c <= '9')
    return CC_NUMERIC;
  if((c >= 'a' && c <= 'z') ||
     (c >= 'A' && c <= 'Z'))
    return CC_ALPHA;
  if(c == '?') return CC_VAR;
  if(c == '_') return CC_COMMENT;
  if(index("[](){}", c))
    return CC_BRACKET;
  return CC_SYMBOL;
}

// starts at comment
const char *skip_comment(const char *s) {
  int level = 0;
  char_class_t before = CC_NONE;
  const char *ptr = s;

  for(;;) {
    // move cursor past comment character, and record character class after it
    ptr++;
    char_class_t after;
    unsigned int length = 1;
    while((after = char_class(*ptr)) == CC_COMMENT) {
      length++;
      ptr++;
    }

    if(before == CC_NONE) {
      if(after != CC_NONE) {
        level++;
      } else if(level == 0) {
        if(length > 1) {
          // line comment
          while(*ptr && *ptr != '\n') ptr++;
          return ptr;
        } else {
          // just a lone comment char
          return s;
        }
      }
    } else if(after == CC_NONE) {
      level--;
      if(!*ptr || level <= 0) return ptr;
    }

    // move cursor to next comment character, tracking character class before it
    ptr++;
    char_class_t cur = after;
    do {
      before = cur;
      ptr++;
      if(!*ptr) return ptr;
      cur = char_class(*ptr);
    } while(cur != CC_COMMENT);
  }
}

void mark_comments(char c, char *str) {
  char *ptr = str;
  while(*ptr) {
    char_class_t cc = char_class(*ptr);
    switch(cc) {
    case CC_NONE:
      ptr++;
      break;
    case CC_COMMENT: {
      char *start = ptr;
      ptr = (char *)skip_comment(ptr);
      if(ptr == start) {
        ptr++;
      } else {
        memset(start, c, ptr - start);
      }
      break;
    }
    default:
      while(char_class(*++ptr) != CC_NONE);
    }
  }
}

int test_comments(UNUSED char *name) {
  char str[] =
    "[1] :one def\n"
    "[2] :t_w_o def\n"
    "{ _an inline _n_e_s_t_ed_ comment_\n"
    "  [one t_w_o +] :three def\n"
    "  three *\n"
    "} :m def __ a line comment\n"
    "__ stack is: 6\n"
    "m:three\n"
    "__ stack is: 6 3\n";
  mark_comments('#', str);
  printf("%s", str);
  return 0;
}
static TEST(test_comments);

seg_t tok(const char *s) {
  seg_t seg = {NULL, 0};
  char_class_t cc = char_class(*s);

  /* skip spaces & comments */
  while(cc == CC_NONE || cc == CC_COMMENT) {
    if(!*s) {
      return seg;
    } else if(cc == CC_COMMENT) {
      const char *n = skip_comment(s);
      if(s == n) break;
      s = n;
    } else {
      s++;
    }
    cc = char_class(*s);
  }

  /* at start of token */
  seg.s = s;

  /* allow adjacent brackets to be separately tokenized */
  if(cc == CC_BRACKET ||
     cc == CC_COMMENT) {
    seg.n = 1;
    return seg;
  }

  while(*++s) {
    char_class_t ncc = char_class(*s);
    if(cc == ncc) continue;

    // exceptions
    switch(ncc) {
    case CC_NUMERIC: // negative numbers
      if(s[-1] == '-') {
        cc = ncc;
        continue;
      } else if(cc == CC_ALPHA) { // allow numeric after alpha
        continue;
      }
      break;
    case CC_ALPHA: // allow symbols
      if(s[-1] == ':') {
        cc = ncc;
        continue;
      }
      break;
    case CC_COMMENT: // comment char inside token
      if(cc == CC_ALPHA ||
         cc == CC_SYMBOL) {
        continue;
      }
      break;
    default:
      break;
    }
    break;
  }
  seg.n = s - seg.s;
  return seg;
}

void reverse_vector(cell_t *c) {
  csize_t n = val_size(c);
  COUNTUP(i, n/2) {
    val_t tmp = c->value.integer[i];
    c->value.integer[i] = c->value.integer[--n];
    c->value.integer[n] = tmp;
  }
}

cell_t *parse_vector(const char **s) {
  seg_t t;
  cell_t *c = vector(0);
  while((t = tok(*s), t.s) &&
        *t.s != ')') {
    assert(is_num(t.s));
    c = pushl_val(atoi(t.s), c);
    *s = seg_end(t);
  }
  reverse_vector(c);
  *s = seg_end(t);
  return c;
}

void argf_noop(UNUSED cell_t *c, UNUSED val_t i) {}
val_t fill_args(cell_t *r, void (*argf)(cell_t *, val_t)) {
  if(!argf) argf = argf_noop;
  csize_t n = list_size(r);
  if(n < 1) return 0;
  cell_t **l = &r->value.ptr[n-1];
  val_t i = 0;
  close_placeholders(*l);
  while(!closure_is_ready(*l)) {
    cell_t *v = var(T_ANY);
    v->value.integer[0] = i;
    arg_noexpand(l, v);
    argf(v, i);
    ++i;
  }
  return i;
}

#define MAX_ARGS 16
cell_t *_build(const char **s) {
  cell_t *arg_stack[MAX_ARGS]; // TODO use allocated storage
  unsigned int n = 0;
  seg_t t;

  while(t = tok(*s), t.s) {
    csize_t in = 0, out = 1;
    cell_t *data = NULL;
    *s = seg_end(t);
    if(t.n == 1) {
      switch(*t.s) {
      case ']':
        goto make_list;
      case '[':
        arg_stack[n++] = _build(s);
        continue;
      case '(':
        arg_stack[n++] = parse_vector(s);
        continue;
      default:
        break;
      }
    }

    if(*t.s == ':' && t.n > 1) {
      seg_t sym = {t.s + 1, t.n - 1};
      arg_stack[n++] = symbol(sym);
      continue;
    }

    cell_t *c = parse_word(t, &in, &out, &data);
    COUNTUP(i, out-1) {
      cell_t *d = dep(ref(c));
      arg(&c, d);
    }
    if(data) {
      arg_stack[n++] = data;
      in++;
    }
    COUNTDOWN(i, min(n, in)) {
      arg(&c, arg_stack[--n]);
    }
    arg_stack[n++] = c;
    COUNTUP(i, out-1) {
      arg_stack[n++] = c->expr.arg[in+i];
    }
  }

make_list: { // build list from stack and return
    cell_t *l = make_list(n);
    COUNTUP(i, n) {
      l->value.ptr[i] = arg_stack[--n];
    }
    return l;
  }
}

cell_t *build(const char *s) {
  return _build(&s);
}

uintptr_t intern(seg_t sym) {
  assert(LENGTH(symbol_strings) > symbol_strings_n + sym.n);
  char *s = &symbol_strings[symbol_strings_n];
  memcpy(s, sym.s, sym.n);
  s[sym.n] = 0;
  pair_t *x = string_map_find(symbols, s);
  uintptr_t v;
  if(x) {
    v = x->second;
  } else {
    v = *map_cnt(symbols);
    assert(v < MAX_SYMBOLS);
    pair_t p = {(uintptr_t)s, v};
    string_map_insert(symbols, p);
    symbol_index[v] = symbol_strings_n;
    symbol_strings_n += sym.n + 1;
  }
  return v;
}

cell_t *symbol(seg_t sym) {
  cell_t *c = closure_alloc(2);
  c->func = func_value;
  c->value.type = T_SYMBOL;
  c->value.integer[0] = intern(sym);
  return c;
}

char *symbol_string(val_t x) {
  if(x >= 0 && x < (val_t)*map_cnt(symbols)) {
    return symbol_strings + symbol_index[x];
  } else {
    return NULL;
  }
}

