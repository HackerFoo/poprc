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
#include <stdio.h>
#include <assert.h>

#include "gen/cells.h"
#include "gen/special.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/parse.h"
#include "gen/word_table.h"
#include "gen/lex.h"
#include "gen/module.h"

pair_t primitive_module[] = WORDS;

cell_t *modules = NULL;

void module_init() {
  modules = NULL;
}

cell_t *cmap_set(cell_t **cp, seg_t key, cell_t *val) {
  cell_t *c = *cp;
  map_t map;

  // check if already in the map
  if(c != NULL) {
    map = c->value.map;
    pair_t *x = seg_map_find(map, key);
    if(x) {
      cell_t *old = (cell_t *)x->second;
      x->second = (uintptr_t)val;
      return old;
    }
  }

  // add to map
  c = expand_map(c);
  map = c->value.map;
  const char *s = seg_string(key);
  pair_t p = {(uintptr_t)s, (uintptr_t)val};
  string_map_insert(map, p);
  *cp = c;
  return NULL;
}

cell_t **cmap_get(cell_t **cp, seg_t key) {
  cell_t *c = *cp;
  map_t map;

  // check if already in the map
  if(c != NULL) {
    map = c->value.map;
    pair_t *x = seg_map_find(map, key);
    if(x) return (cell_t **)&x->second;
  }

  // add to map
  c = expand_map(c);
  map = c->value.map;
  const char *s = seg_string(key);
  pair_t p = {(uintptr_t)s, (uintptr_t)NULL};
  string_map_insert(map, p);
  *cp = c;

  // find and return it
  return (cell_t **)&string_map_find(map, s)->second;
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
     l->n == PERSISTENT) {
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
    cmap_set(&c, string_seg(strings[i]), (cell_t *)i);
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
    goto done;
  }

  m = make_map(ms);
  COUNTUP(i, n) {
    string_map_union(m->value.map, c->value.ptr[i]->value.map);
  }

done:
  closure_free(c);
  m->n = PERSISTENT;
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

seg_t parse_split(char c, const char **start, const char *end) {
  const char *p = *start;
  seg_t s = {p, 0};
  while(p < end && *p++ != c) s.n++;
  *start = p;
  return s;
}

seg_t path_name(seg_t path) {
  seg_t s = seg_rindex(path, '.');
  if(*s.s == '.' && s.n > 0) {
    s.s++;
    s.n--;
  }
  return s;
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

const char *module_name(cell_t *module) {
  if(!modules) return NULL;
  map_t map = modules->value.map;
  FORMAP(i, map) {
    pair_t *e = &map[i];
    if((cell_t *)e->second == module) return (char *)e->first;
  }
  return NULL;
}

const char *entry_name(cell_t *module, cell_t *entry) {
  if(!module || !entry) return NULL;
  map_t map = module->value.map;
  FORMAP(i, map) {
    pair_t *e = &map[i];
    if((cell_t *)e->second == entry) return (char *)e->first;
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
