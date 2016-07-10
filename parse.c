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
#include <strings.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

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

pair_t primitive_module[] = WORDS;

cell_t *modules = NULL;

#define MAX_SYMBOLS 64
static const char *symbol_index[MAX_SYMBOLS] = {
  [SYM_FALSE] = "False",
  [SYM_TRUE] = "True",
  [SYM_IO] = "IO",
  [SYM_DICT] = "Dict"
};

#define ENTRY(str, name) {(uintptr_t)str, SYM_##name}
static pair_t symbols[MAX_SYMBOLS+1] = {
  {MAX_SYMBOLS, 4},
  ENTRY("Dict", DICT),
  ENTRY("False",  FALSE),
  ENTRY("IO", IO),
  ENTRY("True",  TRUE)
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

// for when index declaration is missing for some reason e.g. clang on Android
char *index(const char *s, int c);

void parse_init() {
  strings_top = strings;
  modules = NULL;

  primitive_module[0] = (pair_t) {LENGTH(primitive_module) - 1,
                                  LENGTH(primitive_module) - 1};
}
/*
word_entry_t *alloc_user_word(seg_t w) {
  pair_t *p = seg_map_find(word_index, w);
  word_entry_t *e;
  const char *name;
  if(p) {
    e = (word_entry_t *)p->second;
    if(e >= word_table && e < word_table + 1) return NULL; // primitive
    else return e;
  } else {
    if(new_user_word_entry >= user_word_table + 1) return NULL; // user table is full
    if(!(name = seg_string(w))) return NULL; // strings full
    e = new_user_word_entry++;
    e->name = name;
    e->func = func_self;
    e->in = 0;
    e->out = 1;
    e->data = NULL;
    pair_t x = {(uintptr_t)e->name, (uintptr_t)e};
    string_map_insert(word_index, x);
    return e;
  }
}
*/
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

cell_t *parse_word(seg_t w, cell_t *module) {
  cell_t *c;
  cell_t *data = NULL;
  csize_t in = 0, out = 1;
  if(is_num(w.s)) {
    c = val(atoi(w.s));
  } else if(w.s[0] == '?') {
    c = var(T_ANY);
  } else if(w.n == 4 &&
            w.s[0] == 'a' && w.s[1] == 'p' &&
            char_class(w.s[2]) == CC_NUMERIC &&
            char_class(w.s[3]) == CC_NUMERIC) {
    in = w.s[2] - '0' + 1;
    out = w.s[3] - '0' + 1;
    c = func(func_ap, in, out);
  } else {
    cell_t *e = lookup_word(w);
    if(!e) e = module_lookup_compiled(w, &module);
    if(e) {
      c = func(e->func, e->entry.in + (e->entry.data[0] ? 1 : 0), e->entry.out);
      in = e->entry.in;
      out = e->entry.out;
      data = e->entry.data[0];
    } else {
      // trace the name ***
      c = func(func_placeholder, 0, 1);
    }
  }
  COUNTUP(i, out-1) {
    cell_t *d = dep(ref(c));
    arg(&c, d);
  }
  if(data) {
    arg(&c, data);
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
const char *skip_comment(const char *s, const char *e) {
  int level = 0;
  char_class_t before = CC_NONE;
  const char *ptr = s;

  for(;;) {
    // move cursor past comment character, and record character class after it
    ptr++;
    if(ptr >= e) return e;
    char_class_t after;
    unsigned int length = 1;
    while((after = char_class(*ptr)) == CC_COMMENT) {
      length++;
      ptr++;
      if(ptr >= e) return e;
    }

    if(before == CC_NONE) {
      if(after != CC_NONE) {
        level++;
      } else if(level == 0) {
        if(length > 1) {
          // line comment
          while(ptr < e && *ptr && *ptr != '\n') ptr++;
          return ptr;
        } else {
          // just a lone comment char
          return s;
        }
      }
    } else if(after == CC_NONE) {
      level--;
      if(ptr >= e || !*ptr || level <= 0) return ptr;
    }

    // move cursor to next comment character, tracking character class before it
    ptr++;
    char_class_t cur = after;
    do {
      before = cur;
      ptr++;
      if(ptr >= e || !*ptr) return ptr;
      cur = char_class(*ptr);
    } while(cur != CC_COMMENT);
  }
}

void mark_comments(char c, char *str) {
  char *ptr = str;
  char *e = str + strlen(str);
  while(*ptr) {
    char_class_t cc = char_class(*ptr);
    switch(cc) {
    case CC_NONE:
      ptr++;
      break;
    case CC_COMMENT: {
      char *start = ptr;
      ptr = (char *)skip_comment(ptr, e);
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

int test_comments() {
  char str[] =
    "[1] One def\n"
    "[2] T_w_o def\n"
    "{ _an inline _n_e_s_t_ed_ comment_\n"
    "  [one t_w_o +] :three def\n"
    "  three *\n"
    "} M def __ a line comment\n"
    "__ stack is: 6\n"
    "M:three\n"
    "__ stack is: 6 3\n";
  mark_comments('#', str);
  printf("%s", str);
  return 0;
}

seg_t tok(const char *s, const char* e) {
  seg_t seg = {NULL, 0};
  char_class_t cc = CC_NONE;

  /* skip spaces & comments */
  for(;;) {
    if(s >= e || !*s) return seg;
    cc = char_class(*s);
    if(cc == CC_COMMENT) {
      const char *n = skip_comment(s, e);
      if(s == n) break;
      s = n;
    } else if(cc == CC_NONE) {
      s++;
    } else break;
  }

  /* at start of token */
  seg.s = s;

  /* allow adjacent brackets to be separately tokenized */
  if(cc == CC_BRACKET ||
     cc == CC_COMMENT) {
    seg.n = 1;
    return seg;
  }

  while(++s < e && *s) {
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
    case CC_COMMENT: // comment char inside token
      if(cc == CC_ALPHA ||
         cc == CC_SYMBOL) {
        continue;
      }
      break;
    case CC_SYMBOL:
      if(cc == CC_ALPHA && s[0] == '.') continue; // allow dots in alpha identifiers
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

cell_t *parse_vector(const cell_t **l) {
  const tok_list_t *tl;
  const char *s;
  const cell_t *t = *l;
  cell_t *c = vector(0);
  while(t) {
    tl = &t->tok_list;
    s = tl->location;
    if(s[0] == ')') {
      t = tl->next;
      break;
    } else {
      assert(is_num(s));
      c = pushl_val(atoi(s), c);
      t = tl->next;
    }
  }

  *l = t;
  reverse_vector(c);
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

void update_line(const char *start, const char *end, const char **line) {
  const char *p = end - 1;
  while(p >= start) {
    if(*p == '\n') {
      *line = p + 1;
      break;
    } else {
      p--;
    }
  }
}

seg_t tok_seg(const cell_t *c) {
  seg_t s = {
    .s = c->tok_list.location,
    .n = c->tok_list.length
  };
  return s;
}

void tok_set_seg(cell_t *c, const seg_t s) {
  c->tok_list.location = s.s;
  c->tok_list.length = s.n;
}

cell_t *lex(const char* s, const char* e) {
  if(!e) e = s + strlen(s);
  seg_t t;
  const char *line = s;
  cell_t *ret = NULL, **prev = &ret;
  while(t = tok(s, e), t.s) {
    const char *next = seg_end(t);
    update_line(s, next, &line);
    s = next;
    cell_t *c = closure_alloc(1);
    c->func = func_fail; // HACK
    c->n = PERSISTENT;
    tok_set_seg(c, t);
    c->tok_list.line = line;
    *prev = c;
    prev = &c->tok_list.next;
  }
  return ret;
}

void free_toks(cell_t *t) {
  while(t) {
    cell_t *tmp = t;
    t = t->tok_list.next;
    cell_free(tmp);
  }
}

int test_lex() {
  cell_t *l = lex("testing\n[1 2+ 3]\n_ignore this_ 4\nDone", 0), *p = l;
  if(!l) return -1;
  const char *last_line = l->tok_list.line; 
  while(p) {
    if(p->tok_list.line != last_line) {
      printf("\n");
      last_line = p->tok_list.line;
    }
    printseg("", tok_seg(p), " ");
    p = p->tok_list.next;
  }
  printf("\n");
  free_toks(l);
  return 0;
}

const char *reserved_words[] = {
  "module",
  ":",
  ","
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

uintptr_t tok_indent(const cell_t *c) {
  assert(c->tok_list.location >= c->tok_list.line);
  return c->tok_list.location - c->tok_list.line;
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
      if(match(*it, ",")) {
        cell_t **tmp = it;
        it = &(*it)->tok_list.next;
        cell_free(*tmp);
        *tmp = NULL;
        if(*it) {
          r = list_insert(r, *it);
        }
      }
      it = &(*it)->tok_list.next;
    }
  } while(*it);
done:
  if(*c == *it) {
    return false;
  } else {
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

cell_t *parse_defs(cell_t **c, cell_t **e) {
  cell_t
    *l = NULL,
    *m = NULL,
    *n = NULL,
    *p = *c;
  while(parse_def(&p, &n, &l)) {
    const char *name = seg_string(tok_seg(n));
    cell_free(n);
    m = cmap_insert(m, name, (uintptr_t)l);
    if((*e = check_def(l))) break;
  }
  *c = p;
  if(m) m->n = PERSISTENT;
  return m;
}

void print_def(const cell_t *l) {
  if(!l) {
    printf("NULL\n");
    return;
  }
  csize_t n = list_size(l);
  COUNTUP(i, n) {
    cell_t *p = l->value.ptr[i];
    printf(" ");
    while(p) {
      printseg(" ", tok_seg(p), "");
      p = p->tok_list.next;
    }
    printf("\n");
  }
}

void free_def(cell_t *l) {
  if(!is_closure(l)) {
    return;
  }
  if(is_list(l)) {
    csize_t n = list_size(l);
    COUNTUP(i, n) {
      free_toks(l->value.ptr[i]);
    }
  }
  closure_free(l);
}

void print_defs(const cell_t *m) {
  if(!m) return;
  map_t map = (map_t)m->value.map;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    printf("%s:\n", (char *)map[i+1].first);
    print_def((cell_t *)map[i+1].second);
  }
}

void free_defs(cell_t *m) {
  if(!m || !is_closure(m)) return;
  map_t map = (map_t)m->value.map;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    free_def((cell_t *)map[i+1].second);
  }
  closure_free(m);
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
                  "and.more: stuff, listed on, one line\n"
                  "some.modules:\n"
                  " module one, module two\n"
                  " module three\n", 0);
  cell_t *e = NULL;
  cell_t *m = parse_defs(&p, &e);
  print_defs(m);
  free_defs(m);
  return e ? -1 : 0;
}

bool parse_module(cell_t **c, cell_t **e) {
  cell_t *p = *c, *n = NULL;
  MATCH_ONLY("module");
  MATCH_IF(true, n);
  MATCH_ONLY(":");
  const char *name = seg_string(tok_seg(n));
  cell_free(n);
  cell_t *m = parse_defs(&p, e);
  modules = cmap_insert(modules, name, (uintptr_t)m);
  if(modules) modules->n = PERSISTENT;
  *c = p;
  return !*e;
fail:
  if(!*e) *e = p;
  return false;
}

void print_modules() {
  if(!modules) return;
  map_t map = (map_t)modules->value.map;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    printf("module %s:\n", (char *)map[i+1].first);
    print_defs((cell_t *)map[i+1].second);
    printf("\n");
  }
}

void free_modules() {
  if(!modules) return;
  map_t map = (map_t)modules->value.map;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    free_defs((cell_t *)map[i+1].second);
  }
  closure_free(modules);
  modules = NULL;
}

int test_parse_module() {
  modules = NULL;
  cell_t *p = lex("module a:\n"
                  "f1: the first word\n"
                  "f2: the\n"
                  "      second one\n"
                  "f3:\n"
                  " number three\n"
                  "module b:\n"
                  "f4: heres another\n", 0);
  cell_t *e = NULL;
  while(parse_module(&p, &e));
  print_modules();
  free_modules();
  return e ? -1 : 0;
}

bool is_uppercase(char c) {
  return c >= 'A' && c <= 'Z';
}

#define MAX_ARGS 64
cell_t *parse_expr(const cell_t **l, cell_t *module) {
  cell_t *arg_stack[MAX_ARGS]; // TODO use allocated storage
  unsigned int n = 0;
  const cell_t *t;

  while((t = *l)) {
    *l = t->tok_list.next;
    if(t->tok_list.length == 1) {
      switch(*t->tok_list.location) {
      case ']':
        goto make_list;
      case '[':
        arg_stack[n++] = parse_expr(l, module);
        continue;
      case '(':
        arg_stack[n++] = parse_vector(l);
        continue;
      default:
        break;
      }
    }

    seg_t w = {t->tok_list.location, t->tok_list.length};

    if(is_uppercase(*t->tok_list.location)) {
      arg_stack[n++] = symbol(w);
    } else {
      cell_t *c = parse_word(w, module);
      bool f = !is_value(c);
      csize_t in = 0;
      if(f) {
        in = closure_in(c);
        COUNTDOWN(i, min(n, in)) {
          arg(&c, arg_stack[--n]);
        }
      }
      arg_stack[n++] = c;
      if(f) {
        csize_t out = closure_out(c);
        COUNTUP(i, out) {
          arg_stack[n++] = c->expr.arg[in+i];
        }
      }
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
    assert(v < MAX_SYMBOLS);
    pair_t p = {(uintptr_t)s, v};
    string_map_insert(symbols, p);
    symbol_index[v] = s;
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

const char *symbol_string(val_t x) {
  if(x >= 0 && x < (val_t)*map_cnt(symbols)) {
    return symbol_index[x];
  } else {
    return NULL;
  }
}

cell_t *expand_map(cell_t *c) {
  if(c == NULL) return make_map(1);
  map_t m = c->value.map;
  uintptr_t
    size = map_size(m),
    cnt = *map_cnt(m);
  if(cnt == size) {
    csize_t new_size = size * 2 + 1;
    cell_t *nc = make_map(new_size);
    memcpy(&nc->value.map[0].second, &c->value.map[0].second, sizeof_field(pair_t, second) + size * sizeof(pair_t));
    closure_free(c);
    return nc;
  } else {
    return c;
  }
}

cell_t *cmap_insert(cell_t *c, const char *key, uintptr_t val) {
  c = expand_map(c);
  map_t m = c->value.map;
  pair_t p = {(uintptr_t)key, val};
  string_map_insert(m, p);
  return c;
}

int test_expand_map() {
  cell_t *c = NULL;
  char *strings[] = {
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten"
  };
  FOREACH(i, strings) {
    c = cmap_insert(c, strings[i], i);
  }
  map_t m = c->value.map;
  print_string_map(m);
  FOREACH(i, strings) {
    pair_t *x = string_map_find(m, strings[i]);
    if(x->second != i) {
      return -1;
    }
  }
  closure_free(c);
  return 0;
}

seg_t string_seg(const char *str) {
  seg_t seg = {str, strlen(str)};
  return seg;
}

seg_t parse_split(char c, const char **start, const char *end) {
  const char *p = *start;
  seg_t s = {p, 0};
  while(p < end && *p++ != c) s.n++;
  *start = p;
  return s;
}

cell_t *get_module(seg_t s) {
  pair_t *x = seg_map_find(modules->value.map, s);
  return x ? (cell_t *)x->second : NULL;
}

cell_t *build_module(cell_t *c) {
  cell_t *m = NULL;
  csize_t n = list_size(c);
  uintptr_t ms = 0;

  // replace token pointers with pointers to modules
  COUNTUP(i, n) {
    cell_t **p = &c->value.ptr[i];
    cell_t *mod = get_module(tok_seg((*p)->tok_list.next));
    if(!mod) return NULL;
    ms += *map_cnt(mod->value.map);
    free_toks(*p);
    *p = mod;
  }

  if(n == 1) {
    m = c->value.ptr[0];
    closure_free(c);
    return m;
  }

  m = make_map(ms);
  COUNTUP(i, n) {
    string_map_union(m->value.map, c->value.ptr[i]->value.map);
  }
  closure_free(c);
  return m;
}

cell_t *get_submodule(cell_t *m, seg_t s) {
  pair_t *x = seg_map_find(m->value.map, s);
  if(!x) return NULL;
  cell_t *p = (cell_t *)x->second;

  // already built
  if(is_map(p)) return p;

  // check for module expression
  cell_t *l = p->value.ptr[0];
  if(segcmp("module", tok_seg(l)) != 0) return NULL;
  cell_t *n = build_module(p);
  if(n) x->second = (uintptr_t)n;
  return n;
}

cell_t **implicit_lookup(seg_t w, cell_t *m) {
  static const seg_t seg_import = SEG("imports");
  if(!m) return NULL;
  pair_t *x = seg_map_find(m->value.map, w);
  if(x) return (cell_t **)&x->second;
  cell_t *imports = get_submodule(m, seg_import);
  if(!imports) return NULL;
  x = seg_map_find(imports->value.map, w);
  if(x) return (cell_t **)&x->second;
  return NULL;
}

cell_t **module_lookup(seg_t path, cell_t **context) {
  if(!modules) return NULL;
  const char
    *start = path.s,
    *end = seg_end(path);
  seg_t s = parse_split('.', &start, end);
  if(start == end) return implicit_lookup(s, *context);
  cell_t *m = get_module(s);

  while(m) {
    s = parse_split('.', &start, end);
    if(start < end) {
      m = get_submodule(m, s);
    } else {
      pair_t *x = seg_map_find(m->value.map, s);
      *context = m;
      return x ? (cell_t **)&x->second : NULL;
    }
  }
  return NULL;
}

int test_module_lookup() {
  cell_t *orig_modules = modules;
  modules = NULL;
  cell_t *p = lex("module a:\n"
                  "imports: module e\n"
                  "b: module b\n"
                  "cd: module c, module d\n"
                  "f1: the first word\n"
                  "f2: the\n"
                  "      second one\n"
                  "f3:\n"
                  " number three\n"
                  "module b:\n"
                  "f4: heres another\n"
                  "module b:\n"
                  "a: module a\n"
                  "module c:\n"
                  "f5: 1 2 +\n"
                  "module d:\n"
                  "f6: 3 4 *\n"
                  "module e:\n"
                  "f7: 6 5 -\n", 0);
  cell_t *e = NULL;
  while(parse_module(&p, &e));
  cell_t *ma = get_module(string_seg("a"));
  cell_t *ctx = ma;
  cell_t **c = module_lookup(string_seg("a.b.a.f3"), &ctx);
  printf("a.b.a.f3:\n");
  print_def(*c);
  ctx = ma;
  c = module_lookup(string_seg("a.cd.f5"), &ctx);
  printf("a.cd.f5:\n");
  print_def(*c);
  ctx = ma;
  c = module_lookup(string_seg("a.cd.f6"), &ctx);
  printf("a.cd.f6:\n");
  print_def(*c);
  ctx = ma;
  c = module_lookup(string_seg("f7"), &ctx);
  printf("f7:\n");
  print_def(*c);
  free_modules();
  modules = orig_modules;
  return e ? -1 : 0;
}

cell_t *module_lookup_compiled(seg_t path, cell_t **context) {
  cell_t **p = module_lookup(path, context);
  if(!p || !*p) return NULL;
  if(!is_list(*p)) return *p;
  if(compile_word(p, *context)) {
    return *p;
  } else {
    return NULL;
  }
}
