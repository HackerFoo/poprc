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
#include <stdio.h>

#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/map.h"
#include "startle/test.h"

#include "cells.h"
#include "special.h"
#include "parse/parse.h"
#include "parse/lex.h"
#include "module.h"
#include "ir/compile.h"
#include "list.h"
#include "ir/trace.h"

#define WORD_ALIAS__ITEM(__file, __line, __name, __func, __in, __out, __builder, ...) \
  WORD__ITEM(__file, __line, __name, __func, __in, __out, ##__VA_ARGS__)

// count the number of words
#define WORD__ITEM(...) CONCAT(anon, __LINE__),
enum word_count {
  #include "word_list.h"
  WORD_COUNT
};
#undef WORD__ITEM

#define WORD__ITEM_FLAGS(__file, __line, __name, __func, __in, __out, __flags) \
  {                                                      \
    .first = (uintptr_t)__name,                          \
    .second = (uintptr_t)&(cell_t) {                     \
      .op = OP_##__func,                                 \
      .module_name = PRIMITIVE_MODULE_NAME,              \
      .word_name = __name "\0" #__func,                  \
      .entry = {                                         \
        .in = __in,                                      \
        .out = __out,                                    \
        .len = 0,                                        \
        .flags = (__flags)                               \
      }                                                  \
    }                                                    \
  },

#define WORD__ITEM_6(__file, __line, __name, __func, __in, __out) \
  WORD__ITEM_FLAGS(__file, __line, __name, __func, __in, __out, ENTRY_PRIMITIVE)
#define WORD__ITEM_7(__file, __line, __name, __func, __in, __out, f0)   \
  WORD__ITEM_FLAGS(__file, __line, __name, __func, __in, __out, ENTRY_PRIMITIVE | ENTRY_##f0)
#define WORD__ITEM_8(__file, __line, __name, __func, __in, __out, f0, f1)  \
  WORD__ITEM_FLAGS(__file, __line, __name, __func, __in, __out, ENTRY_PRIMITIVE | ENTRY_##f0 | ENTRY_##f1)
#define WORD__ITEM_9(__file, __line, __name, __func, __in, __out, f0, f1, f2) \
  WORD__ITEM_FLAGS(__file, __line, __name, __func, __in, __out, ENTRY_PRIMITIVE | ENTRY_##f0 | ENTRY_##f1 | ENTRY_##f2)
#define WORD__ITEM(...) DISPATCH(WORD__ITEM, __VA_ARGS__)

pair_t primitive_module[] = {
  { .first = WORD_COUNT, .second = WORD_COUNT },
  #include "word_list.h"
};

cell_t *modules = NULL;

cell_t *make_module() {
  cell_t *l = alloc_list(1);
  *l = (cell_t) {
    .size = l->size,
    .op = OP_value,
    .value.type = T_MODULE,
    .n = PERSISTENT
  };
  return l;
}

bool is_module(const cell_t *c) {
  return is_value(c) && c->value.type == T_MODULE;
}

cell_t **module_ref(cell_t *m) {
  return &m->value.ptr[0];
}

static
map_t value_map(cell_t *c) {
  assert_error(is_value(c), "%C", c);
  return c->value.map;
}

static
map_t module_map(cell_t *m) {
  assert_error(is_module(m), "%C", m);
  cell_t *c = *module_ref(m);
  return c ? value_map(c) : NULL;
}

void module_init() {
  modules = make_module();
}

cell_t *module_set(cell_t *m, seg_t key, cell_t *val) {
  assert_error(is_module(m));
  cell_t *c = *module_ref(m);
  map_t map;

  // check if already in the map
  if(c != NULL) {
    map = value_map(c);
    pair_t *x = seg_map_find(map, key);
    if(x) {
      cell_t *old = (cell_t *)x->second;
      x->second = (uintptr_t)val;
      return old;
    }
  }

  // add to map
  c = expand_map(c, 1);
  map = value_map(c);
  const char *s = seg_string(key);
  pair_t p = {(uintptr_t)s, (uintptr_t)val};
  string_map_insert(map, p);
  *module_ref(m) = persistent(c);
  return NULL;
}

cell_t *module_get(cell_t *m, seg_t key) {
  assert_error(is_module(m));
  cell_t *c = *module_ref(m);
  if(c == NULL) {
    return NULL;
  } else {
    pair_t *x = seg_map_find(value_map(c), key);
    return x ? (cell_t *)x->second : NULL;
  }
}

cell_t *module_get_or_create(cell_t *m, seg_t key) {
  assert_error(is_module(m));
  cell_t *r = module_get(m, key);
  if(r) return r;

  // add to map
  cell_t *c = *module_ref(m);
  c = expand_map(c, 1);
  map_t map = value_map(c);
  const char *s = seg_string(key);
  r = make_module();
  pair_t p = {(uintptr_t)s, (uintptr_t)r};
  string_map_insert(map, p);
  *module_ref(m) = persistent(c);

  return r;
}

void print_def(const cell_t *l) {
  if(!l) {
    printf("  NULL\n");
    return;
  } else if(is_map(l)) {
    printf("  submodule\n");
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
  if(!is_closure(l) ||
     is_persistent(l)) {
    return;
  }
  if(is_list(l)) {
    csize_t n = list_size(l);
    COUNTUP(i, n) {
      free_toks(l->value.ptr[i]);
    }
    closure_free(l);
  }
}

void print_defs(const cell_t *m) {
  assert_error(is_module(m));
  map_t map = module_map((cell_t *)m);
  if(!map) return;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    pair_t p = map[i+1];
    cell_t *l = (cell_t *)p.second;
    if(!is_list(l)) continue;
    printf("%s:\n", (char *)p.first);
    print_def(l);
  }
}

void free_defs(cell_t *m) {
  assert_error(is_module(m));
  if(!m || !is_closure(m)) return;
  map_t map = module_map(m);
  if(map) {
    csize_t n = *map_cnt(map);
    COUNTUP(i, n) {
      free_def((cell_t *)map[i+1].second);
    }
  }
  closure_free(*module_ref(m));
  closure_free(m);
}

void print_modules() {
  if(!modules) return;
  assert_error(is_module(modules));
  map_t map = module_map(modules);
  if(!map) return;
  csize_t n = *map_cnt(map);
  COUNTUP(i, n) {
    printf("module %s:\n", (char *)map[i+1].first);
    print_defs((cell_t *)map[i+1].second);
    printf("\n");
  }
}

void free_modules() {
  if(!modules) return;
  assert_error(is_module(modules));
  map_t map = module_map(modules);
  if(map) {
    csize_t n = *map_cnt(map);
    COUNTUP(i, n) {
      free_defs((cell_t *)map[i+1].second);
    }
  }
  cell_t **r = module_ref(modules);
  closure_free(*r);
  r = NULL;
}

cell_t *expand_map(cell_t *c, csize_t n) {
  if(c == NULL) return make_map(n);
  map_t c_map = value_map(c);
  uintptr_t
    size = map_size(c_map),
    cnt = *map_cnt(c_map);
  if(cnt + n > size) {
    cell_t *nc = make_map(size + n);
    map_t nc_map = value_map(nc);
    memcpy(&nc_map[0].second, &c_map[0].second, sizeof_field(pair_t, second) + size * sizeof(pair_t));
    closure_free(c);
    return nc;
  } else {
    return c;
  }
}

TEST(expand_map) {
  cell_t *m = make_module();
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
    module_set(m, string_seg(strings[i]), (cell_t *)i);
  }
  map_t map = module_map(m);
  print_string_map(map);
  FOREACH(i, strings) {
    pair_t *x = string_map_find(map, strings[i]);
    if(x->second != i) {
      return -1;
    }
  }
  closure_free(*module_ref(m));
  closure_free(m);
  return 0;
}

cell_t *get_module(seg_t s) {
  return module_get(modules, s);
}

cell_t *build_module(cell_t *c) {
  cell_t *m;
  csize_t n = list_size(c);
  uintptr_t ms = 0;

  // replace token pointers with pointers to modules
  COUNTUP(i, n) {
    cell_t **p = &c->value.ptr[i];
    cell_t *mod = get_module(tok_seg((*p)->tok_list.next));
    if(!mod) return NULL;
    map_t map = module_map(mod);
    if(map) ms += *map_cnt(map);
    free_toks(*p);
    *p = mod->value.ptr[0];
  }

  if(n == 1) {
    m = copy(c->value.ptr[0]);
  } else {
    m = make_map(ms);
    COUNTUP(i, n) {
      cell_t *p = c->value.ptr[i];
      if(p) {
        string_map_union(value_map(m), value_map(p));
      }
    }
  }

  closure_free(c);
  cell_t *r = make_module();
  *module_ref(r) = persistent(m);
  return r;
}

void merge_into_module(cell_t *ma, cell_t *mb) {
  if(!mb) return;
  cell_t
    *a = *module_ref(ma),
    *b = *module_ref(mb);
  if(!b) return;
  if(!a) {
    *module_ref(ma) = persistent(copy(b));
  } else {
    assert_error(is_map(a));
    a = expand_map(a, *map_cnt(value_map(b)));
    string_map_union(value_map(a), value_map(b));
    *module_ref(ma) = persistent(a);
  }
}

cell_t *get_submodule(cell_t *m, seg_t s) {
  cell_t *p = module_get(m, s);

  if(!p) return NULL;

  // already built
  if(is_module(p)) return p;

  // check for module expression
  cell_t *l = p->value.ptr[0];
  if(segcmp("module", tok_seg(l)) != 0) return NULL;
  cell_t *n = build_module(p);
  if(n) module_set(m, s, n);
  return n;
}

cell_t *implicit_lookup(seg_t w, cell_t *m) {
  static const seg_t seg_import = SEG("imports");
  if(!m) return NULL;
  assert_error(is_module(m));
  cell_t *r = module_get(m, w);
  if(r) return r;
  cell_t *imports = get_submodule(m, seg_import);
  if(!imports) return NULL;
  return module_get(imports, w);
}

seg_t parse_split(char c, const char **start, const char *end) {
  const char *p = *start;
  seg_t s = {p, 0};
  while(p < end && *p++ != c) s.n++;
  *start = p;
  return s;
}

seg_t path_name(seg_t path) {
  seg_t s = seg_after(path, '.');
  if(*s.s == '.' && s.n > 0) {
    s.s++;
    s.n--;
  }
  return s;
}

cell_t *module_lookup(seg_t path, cell_t **context) {
  if(!modules || !*module_ref(modules)) return NULL;
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
      cell_t *r = module_get(m, s);
      if(r) *context = m;
      return r;
    }
  }
  return NULL;
}

const char *module_name(cell_t *module) {
  if(!modules) return NULL;
  map_t map = module_map(modules);
  FORMAP(i, map) {
    pair_t *e = &map[i];
    if((cell_t *)e->second == module) return (char *)e->first;
  }
  return NULL;
}

const char *entry_name(cell_t *module, cell_t *entry) {
  if(!module || !entry) return NULL;
  map_t map = module_map(module);
  FORMAP(i, map) {
    pair_t *e = &map[i];
    if((cell_t *)e->second == entry) return (char *)e->first;
  }
  return NULL;
}

TEST(module_lookup) {
  cell_t *orig_modules = modules;
  modules = make_module();
  cell_t *p = lex("module a:\n"
                  "imports: module e\n"
                  "b: module b\n"
                  "cd: module c\n"
                  "    module d\n"
                  "f1: the first word\n"
                  "f2: the\n"
                  "      second one\n"
                  "f3:\n"
                  " number three\n"
                  "module b:\n"
                  "f4: heres another\n"
                  "a: module a\n"
                  "module c:\n"
                  "f5: 1 2 +\n"
                  "module d:\n"
                  "f6: 3 4 *\n"
                  "module e:\n"
                  "f7: 6 5 -\n", 0);
  cell_t *e = NULL;
  seg_t n;
  char *s = "Loaded modules (";
  while(parse_module(&p, &n, &e)) {
    printf("%s%.*s", s, (int)n.n, n.s);
    s = ", ";
  }
  printf(")\n");

  cell_t *ma = get_module(string_seg("a"));
  cell_t *ctx = ma;
  cell_t *c = module_lookup(string_seg("a.b.a.f3"), &ctx);
  printf("a.b.a.f3:\n");
  print_def(c);
  ctx = ma;
  c = module_lookup(string_seg("a.cd.f5"), &ctx);
  printf("a.cd.f5:\n");
  print_def(c);
  ctx = ma;
  c = module_lookup(string_seg("a.cd.f6"), &ctx);
  printf("a.cd.f6:\n");
  print_def(c);
  ctx = ma;
  c = module_lookup(string_seg("f7"), &ctx);
  printf("f7:\n");
  print_def(c);
  free_modules();
  closure_free(modules);
  modules = orig_modules;
  return e ? -1 : 0;
}

COMMAND(import, "import given module(s), or all") {
  cell_t *eval_module = module_get_or_create(modules, string_seg("eval"));
  cell_t *eval_imports = module_get_or_create(eval_module, string_seg("imports"));
  if(!rest) { // import all
    printf("Importing all modules (");
    map_t m = module_map(modules);
    char *sep = "";
    FORMAP(i, m) {
      if(strcmp("eval", (const char *)m[i].first) != 0) {
        printf("%s%s", sep, (const char *)m[i].first);
        merge_into_module(eval_imports, (cell_t *)m[i].second);
        sep = ", ";
      }
    }
    printf(")\n");
  } else {
    while(rest) {
      cell_t *import = get_module(tok_seg(rest));
      merge_into_module(eval_imports, import);
      rest = rest->tok_list.next;
    }
  }
}

void print_module_bytecode(cell_t *m) {
  error_t error;
  assert_error(is_module(m));
  if(!*module_ref(m)) return;
  cell_t *map_copy = persistent(copy(*module_ref(m)));
  map_t map = value_map(map_copy);
  string_map_sort_full(map);
  FORMAP(i, map) {
    char *name = (char *)map[i].first;
    if(strcmp("imports", name) == 0) continue;
    CATCH(&error) {
      printf(NOTE("ERROR") " ");
      print_last_log_msg();
      print_active_entries("  - while compiling ");
      printf("\n");
      trace_reset_active();
      log_soft_init();
      cleanup_cells();
      reset_counters();
    } else {
      cell_t *p = module_lookup(string_seg(name), &m);
      if(!(p->value.attributes & ATTR_HIDE)) {
        cell_t *e = compile_def(p, string_seg(name), &m);
        if(e) {
          print_bytecode(tcell_entry(e), false);
          putchar('\n');
        }
      }
    }
  }
  closure_free(map_copy);
}

void print_all_bytecode() {
  if(!modules) return;
  assert_error(is_module(modules));
  map_t map = module_map(modules);
  if(!map) return;
  string_map_sort_full(map);
  FORMAP(i, map) {
    print_module_bytecode((cell_t *)map[i].second);
  }
}
