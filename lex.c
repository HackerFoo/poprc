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
#include <stdio.h>

#include "startle/error.h"
#include "startle/log.h"
#include "startle/test.h"
#include "startle/support.h"

#include "cells.h"
#include "special.h"
#include "tok.h"
#include "lex.h"

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
  char_class_t cc;
  while(t = tok(s, e, &cc), t.s) {
    const char *next = seg_end(t);
    update_line(s, next, &line);
    s = next;
    cell_t *c = closure_alloc(1);
    c->func = func_fail; // HACK
    c->n = PERSISTENT;
    tok_set_seg(c, t);
    c->tok_list.line = line;
    c->char_class = cc;
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

void print_toks(cell_t *p) {
  const char *last_line = p->tok_list.line;
  while(p) {
    if(p->tok_list.line != last_line) {
      printf("\n");
      last_line = p->tok_list.line;
    }
    printseg("", tok_seg(p), " ");
    p = p->tok_list.next;
  }
  printf("\n");
}

TEST(lex) {
  cell_t *l = lex("testing\n[1 2+ 3]\n_ignore this_ 4\nDone", 0);
  if(!l) return -1;
  print_toks(l);
  free_toks(l);
  return 0;
}

uintptr_t tok_indent(const cell_t *c) {
  assert_error(c->tok_list.location >= c->tok_list.line);
  return c->tok_list.location - c->tok_list.line;
}
