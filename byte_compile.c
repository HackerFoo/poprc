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

void make_index(cell_t *c) {
  traverse(c, {
      if(*p && !((*p)->type & T_TRACED)) {
        trace_store(clear_ptr(*p, 3));
        (*p)->type |= T_TRACED;
        make_index(*p);
      }
    }, ARGS | PTRS);
}

cell_t trace_cells[1 << 10];
cell_t *trace_cur = &trace_cells[0];
cell_t *trace_ptr = &trace_cells[0];
size_t trace_cnt = 0;
static MAP(trace_index, 1 << 10);

void trace_index_add(const cell_t *c, uintptr_t x) {
  pair_t p = {(uintptr_t)c, x};
  map_insert(trace_index, p);
}

void trace_index_assign(cell_t *new, cell_t *old) {
  pair_t *p = map_find(trace_index, (uintptr_t)old);
  if(p) {
    trace_index_add(new, p->second);
  }
}

cell_t *trace_encode(uintptr_t index) {
  return (cell_t *)(-(index+1));
}

uintptr_t trace_decode(cell_t *c) {
  return -1 - (intptr_t)c;
}

cell_t *trace_store(const cell_t *c) {
  cell_t *dest = trace_ptr;
  unsigned int size = closure_cells(c);
  trace_ptr += size;
  trace_cnt++;
  memcpy(dest, c, sizeof(cell_t) * size);
  trace_index_add(c, dest - trace_cur);

  dest->n = -1;

  // rewrite pointers
  traverse(dest, {
      if(*p) {
        pair_t *e = map_find(trace_index, (uintptr_t)clear_ptr(*p, 3));
        if(e) {
          *p = trace_encode(e->second);
          trace_cur[e->second].n++;
        } else {
          *p = NULL;
        }
      }
    }, ARGS | PTRS);

  return dest;
}

cell_t *trace_select(const cell_t *c, const cell_t *a) {
  cell_t *dest = trace_ptr;
  unsigned int size = closure_cells(c);
  trace_ptr += size;
  trace_cnt++;

  pair_t *e = map_find(trace_index, (uintptr_t)clear_ptr(c, 3));
  assert(e != NULL);
  pair_t *e2 = map_find(trace_index, (uintptr_t)clear_ptr(a, 3));
  assert(e2 != NULL);

  memset(dest, 0, sizeof(cell_t));
  dest->func = func_select;
  dest->arg[0] = trace_encode(e->second);
  dest->arg[1] = trace_encode(e2->second);
  dest->size = 2;
  dest->n = -1;

  trace_cur[e->second].n++;
  trace_cur[e2->second].n++;

  e->second = dest - trace_cur;

  return dest;
}

cell_t *trace_update_type(const cell_t *c) {
  pair_t *p = map_find(trace_index, (uintptr_t)c);
  if(!p) return NULL;
  cell_t *t = &trace_cur[p->second];
  t->type = c->type;
  return t;
}

void trace_init() {
  trace_cur = trace_ptr;
  trace_cnt = 0;
  map_clear(trace_index);
}

void print_trace_cells() {
  for(cell_t *c = trace_cur; c < trace_ptr; c += closure_cells(c)) {
    printf("cell[%d]:", (int)(c-trace_cur));
    if(is_reduced(c)) {
      if(is_var(c)) {
        printf(" var");
      } else if(is_list(c)) {
        printf(" [");
        for(unsigned int i = 0; i < list_size(c); i++) {
          printf(" %ld", (long int)trace_decode(c->ptr[i]));
        }
        printf(" ]");
      } else {
        printf(" val %ld", (long int)c->val[0]);
      }
      printf(", type = %s", show_type_all_short(c->type));
    } else {
      printf(" %s", function_name(c->func));

      traverse(c, {
          printf(" %ld", (long int)trace_decode(*p));
        }, ARGS | PTRS);
    }
    printf(" x%d\n", c->n + 1);
  }
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
    //if(is_any(c)) break; // why?
    if(is_list(c) && is_placeholder(c->ptr[0])) {
      trace_index_assign(c, c->arg[0]);
    } else if(is_var(c)) {
      trace_update_type(c);
    } else {
      trace_store(c);
    }
    c->type |= T_TRACED;
    break;
  }
  case tt_select: {
    trace_select(c, r);
    break;
  }
  case tt_copy: {
    trace_index_assign(c, c->arg[0]);
    break;
  }
  case tt_compose_placeholders: {
    /* to do *** */
    break;
  }
  }
}

void bc_arg(cell_t const *c, UNUSED int x) {
  trace_store(c);
}

cell_t *byte_compile(cell_t *root, UNUSED int in, UNUSED int out) {
  trace_init();
  cell_t *header = trace_ptr++;
  header->func = func_id; // to pass asserts
  trace_cur = trace_ptr;
  set_trace(bc_trace);
  fill_args(root, bc_arg);
  reduce_root(root);
  trace_store(root)->n++;

  /*
  // make index readable for debugging
  for(size_t i = 1; i <= *map_cnt(trace_index); i++) {
    trace_index[i].first = (cell_t *)trace_index[i].first - cells;
  }

  print_map(trace_index);
  */
  print_trace_cells();
  header->val[0] = trace_cnt;
  return header;
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
  e->func = func_exec;
  e->data = byte_compile(c, e->in, e->out);
}

bool func_exec(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  assert(is_closure(c));

  size_t data = closure_in(c) - 1;
  cell_t *header = c->arg[data];
  cell_t *code = header + 1;
  size_t count = header->val[0];
  cell_t *map[count];
  size_t map_idx = 0;
  cell_t *last, *res;

  c->arg[data] = 0;
  memset(map, 0, sizeof(map[0]) * count);

  size_t i = data;
  while(i--) {
    assert(is_var(code));
    map[map_idx++] = c->arg[i];
    refn(c->arg[i], code->n + 1);
    code++;
  }
  count -= data;

  while(count--) {
    size_t s = closure_cells(code);
    cell_t *nc = closure_alloc_cells(s);
    memcpy(nc, code, s * sizeof(cell_t));
    code += s;

    map[map_idx++] = nc;
    traverse(nc, {
        if(*p) {
          unsigned int x = trace_decode(*p);
          if(x < map_idx) {
            *p = map[x];
          } else {
            *p = NULL;
          }
        }
      }, ARGS | PTRS);
    last = nc;
  }

  size_t
    out_n = list_size(last),
    in_n = c->size - out_n;
  res = ref(last->ptr[0]);
  for(i = 1; i < out_n; i++) {
    cell_t *d = c->arg[in_n + i];
    d->func = func_id;
    d->arg[0] = ref(last->ptr[i]);
    d->arg[1] = 0;
    drop(c);
  }

  drop(last);

  bool ret = reduce(&res, t);
  store_reduced(cp, res);

  return ret;
}
