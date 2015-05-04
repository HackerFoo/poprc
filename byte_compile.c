#include "rt_types.h"
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>

#include "gen/rt.h"
#include "gen/eval.h"
#include "gen/primitive.h"
#include "gen/test.h"
#include "gen/support.h"
#include "gen/map.h"
#include "gen/byte_compile.h"

/*
void _make_index(cell_t *c, pair_t **index) {
  traverse(c, {
      if(*p && !((*p)->type & T_TRACED)) {
        pair_t *x = *index;
        x->first = (intptr_t)clear_ptr(*p, 3);
        (*p)->type |= T_TRACED;
        *index += 1;
        _make_index(*p, index);
      }
    }, ARGS | PTRS);
}

unsigned int make_index(cell_t *c, pair_t *index) {
  pair_t *p = index;
  _make_index(c, &p);
  unsigned int n = p - index;
  quicksort(index, n);
  for(unsigned int i = 0; i < n; i++) {
    ((cell_t *)p[i].first)->type &= ~T_TRACED;
    p[i].second = i;
  }
  return n;
}
*/
cell_t trace_cells[1 << 10];
cell_t *trace_ptr = &trace_cells[0];
static MAP(trace_index, 1 << 10);

void trace_index_add(cell_t *c, uintptr_t x) {
  pair_t p = {(uintptr_t)c, x};
  map_insert(trace_index, p);
}

void trace_index_assign(cell_t *new, cell_t *old) {
  pair_t *p = map_find(trace_index, (uintptr_t)old);
  if(p) {
    trace_index_add(new, p->second);
  }
}

cell_t *trace_store(cell_t *c) {
  cell_t *dest = trace_ptr;
  unsigned int size = closure_cells(c);
  trace_ptr += size;
  memcpy(dest, c, sizeof(cell_t) * size);
  trace_index_add(c, dest - trace_cells);
  return dest;
}

void trace_init() {
  trace_ptr = &trace_cells[0];
  map_clear(trace_index);
}

void trace_rewrite_ptrs() {

}

void bc_trace(cell_t *c, cell_t *r, trace_type_t tt) {
  switch(tt) {
  case tt_reduction: {
    if(is_reduced(c) || !is_var(r)) break;
    if(c->func == func_pushl ||
       c->func == func_pushr ||
       c->func == func_popr) break;
    if(is_dep(c)) break;
    int i, in = closure_in(c);
    for(i = 0; i < in; ++i) trace(c->arg[i], 0, tt_force);

    if(c->func == func_cut ||
       c->func == func_id) {
      trace_index_assign(c, c->arg[0]);
    } else if(c->func == func_dep) {
      // do nothing
    } else if(c->func == func_placeholder) {
      //fb->apply_list(c);
    } else if(c->func == func_self) {
      //fb->callSelf(c);
      trace_store(c);
    } else {
      trace_store(c);
    }
    r->type |= T_TRACED;
    break;
  }
  case tt_touched:
    if(!is_var(c)) break;
  case tt_force: {
    if(!is_reduced(c)) break;
    if(c->type & T_TRACED) break;
    if(is_any(c)) break;
    if(is_list(c) && is_placeholder(c->ptr[0])) {
      //fb->assign(c->ptr[0], c); // ***
      trace_index_assign(c, c->arg[0]);
    } else if(!is_var(c)) {
      //fb->val(c);
      trace_store(c);
    }
    c->type |= T_TRACED;
    break;
  }
  case tt_select: {
    //fb->select(c, r);
    break;
  }
  case tt_copy: {
    //fb->assign(c, r);
    trace_index_assign(c, c->arg[0]);
    break;
  }
  case tt_compose_placeholders: {
    /* to do *** */
    break;
  }
  }
}

cell_t const *bc_args[1 << 10] = {0};
void bc_arg(cell_t const *c, int x) {
  bc_args[x] = c;
}

reduce_t *byte_compile(cell_t *root, UNUSED int in, UNUSED int out) {
  trace_init();
  set_trace(bc_trace);
  fill_args(root, bc_arg);
  reduce_root(root);

  print_map(trace_index);

  return NULL;
}

void compact_expr(char const *name, char *str, unsigned int n) {
  word_entry_t *e =
    lookup_linear(user_word_table,
                  WIDTH(user_word_table),
                  user_word_table_length,
                  name);
  if(!e) e = new_user_word_entry++;
  strcpy(e->name, name);
  e->func = func_placeholder;
  e->in = 0;
  e->out = 1;
  char *s = malloc(n+1);
  memcpy(s, str, n+1);
  get_arity(s, n, &e->in, &e->out);
  free(s);
  e->func = func_self;
  cell_t *c = build(str, n);
  if(!c) {
    --new_user_word_entry;
    return;
  }
  e->func = byte_compile(c, e->in, e->out);
}
