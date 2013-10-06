/* Copyright 2012-2013 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <string.h>
#include <assert.h>
#include "rt_types.h"
#include "gen/rt.h"
#include "gen/primitive.h"

// make sure &cells > 255
void *data_start;
cell_t cells[1<<16];
cell_t *cells_ptr;
uint8_t alt_cnt = 0;
cell_t fail_cell = {
  .func = func_reduced,
  .type = T_FAIL
};
#define BM_SIZE (sizeof(intptr_t) * 4)
#define ALT_SET_IDS BM_SIZE
uintptr_t alt_live[sizeof(intptr_t) * 4];

measure_t measure, saved_measure;

void trace_noop(cell_t *c, cell_t *r, trace_type_t tt) {};
void (*trace)(cell_t *, cell_t *, trace_type_t) = trace_noop;

void set_trace(void (*t)(cell_t *, cell_t *, trace_type_t)) {
  trace = t ? t : trace_noop;
}

// #define CHECK_CYCLE

bool is_data(void *p) {
  return p >= data_start;
}

bool is_cell(void *p) {
  return p >= (void *)&cells && p < (void *)(&cells+1);
}

bool is_closure(void *p) {
  return is_data(p) && ((cell_t *)p)->func;
}

bool closure_is_ready(cell_t *c) {
  assert(is_closure(c));
  return !is_marked(c->func, 1);
}

void closure_set_ready(cell_t *c, bool r) {
  assert(is_closure(c));
  c->func = mark_ptr(clear_ptr(c->func, 1), r ? 0 : 1);
}

/* propagate alternatives down to root of expression tree */
/* [TODO] distribute splitting into args */
/* or combine reduce and split, so each arg is reduced */
/* just before split, and alts are stored then zeroed */
cell_t *closure_split(cell_t *c, unsigned int s) {
  assert(is_closure(c) && closure_is_ready(c));
  unsigned int i, j, n = 0;
  unsigned int alt_mask = 0;

  cell_t *cn, *p = c->alt;

  /* calculate a mask for args with alts */
  i = s;
  alt_mask = 0;
  while(i--) {
    alt_mask <<= 1;
    if(is_marked(c->arg[i], 1)) {
      /* clear marks as we go */
      c->arg[i] = clear_ptr(c->arg[i], 1);
    } else if(c->arg[i] &&
	      c->arg[i]->alt) {
      alt_mask |= 1;
      n++;
    }
  }

  if(n == 0) return p;

  p = c->alt;

  for(i = alt_mask;
      i != 0;
      i = (i - 1) & alt_mask) {
    cn = closure_alloc(s);
    cn->func = c->func;
    for(j = 0; j < s; j++) {
      cn->arg[j] = (1<<j) & i ?
	c->arg[j]->alt :
	mark_ptr(c->arg[j], 1);
    }
    cn->alt = p;
    p = cn;
  }

  unsigned int nref_alt_alt = 1 << (n-1);
  unsigned int nref_alt = nref_alt_alt - 1;
  unsigned int nref_noalt = (1 << n) - 1;
  for(i = 0; i < s; i++) {
    if((1<<i) & alt_mask) {
      c->arg[i]->alt->n += nref_alt_alt;
      c->arg[i]->n += nref_alt;
    } else {
      c->arg[i]->n += nref_noalt;
    }
  }

  return p;
}

cell_t *closure_split1(cell_t *c, int n) {
  int i;
  if(!c->arg[n]->alt) return c->alt;
  cell_t *a = copy(c);
  a->arg[n] = 0;
  traverse_ref(a, ARGS_IN);
  for(i = c->size - c->out; i < c->size; ++i) {
    a->arg[i] = dep(a);
  }
  a->arg[n] = ref(c->arg[n]->alt);
  a->n = c->out;
  a->alt = c->alt;
  return a;
}

bool reduce(cell_t **cp, type_rep_t t) {
  cell_t *c;
  while((c = clear_ptr(*cp, 1))) {
    if(!closure_is_ready(c)) close_placeholders(c);
    assert(is_closure(c));
    if(!closure_is_ready(c)) {
      fail(cp);
      continue;
    }
    measure.reduce_cnt++;
    if(c->func(cp, t)) return true;
  }
  *cp = &fail_cell;
  return false;
}

bool reduce_partial(cell_t **cp) {
  cell_t *c;
  c = clear_ptr(*cp, 1);
  if(!closure_is_ready(c)) close_placeholders(c);
  if(!c || !closure_is_ready(c)) {
    fail(cp);
    return false;
  }
  assert(is_closure(c) &&
	 closure_is_ready(c));
  measure.reduce_cnt++;
  return c->func(cp, T_ANY);
}

cell_t *cells_next() {
  cell_t *p = cells_ptr;
  assert(is_cell(p) && !is_closure(p) && is_cell(cells_ptr->next));
  cells_ptr = cells_ptr->next;
  return p;
}

#ifdef CHECK_CYCLE
bool check_cycle() {
  int i = 0;
  cell_t *start = cells_ptr, *ptr = start;
  while(ptr->next != start) {
    if(i > LENGTH(cells)) return false;
    i++;
    assert(is_cell(ptr->next->next));
    ptr = ptr->next;
  }
  return true;
}
#else
bool check_cycle() {
  return true;
}
#endif

void cells_init() {
  data_start = (void *)cells;
  if((void *)&fail_cell < data_start) data_start = &fail_cell;
  int i;
  const unsigned int n = LENGTH(cells)-1;

  // zero the cells
  memset(&cells, 0, sizeof(cells));
  memset(&alt_live, 0, sizeof(alt_live));

  // set up doubly-linked pointer ring
  for(i = 0; i < n; i++) {
    cells[i].prev = &cells[i-1];
    cells[i].next = &cells[i+1];
  }
  cells[0].prev = &cells[n-1];
  cells[n-1].next = &cells[0];

  cells_ptr = &cells[0];
  assert(check_cycle());
  alt_cnt = 0;
}

void cell_alloc(cell_t *c) {
  assert(is_cell(c) && !is_closure(c));
  cell_t *prev = c->prev;
  assert(is_cell(prev) && !is_closure(prev));
  cell_t *next = c->next;
  assert(is_cell(next) && !is_closure(next));
  if(cells_ptr == c) cells_next();
  prev->next = next;
  next->prev = prev;
  measure.alloc_cnt++;
  if(++measure.current_alloc_cnt > measure.max_alloc_cnt)
    measure.max_alloc_cnt = measure.current_alloc_cnt;
  assert(check_cycle());
}

cell_t *closure_alloc(int args) {
  cell_t *c = closure_alloc_cells(calculate_cells(args));
  c->size = args;
  return c;
}

cell_t *closure_alloc_cells(int size) {
  cell_t *ptr = cells_next(), *c = ptr;
  cell_t *mark = ptr;
  int cnt = 0;
  (void)mark;

  // search for contiguous chunk
  while(cnt < size) {
    if(is_cell(ptr) && !is_closure(ptr)) {
      cnt++;
      ptr++;
    } else {
      cnt = 0;
      c = ptr = cells_next();
      assert(c != mark);
    }
  }

  // remove the found chunk
  int i;
  for(i = 0; i < size; i++) {
    cell_alloc(&c[i]);
  }

  memset(c, 0, sizeof(cell_t)*size);
  assert(check_cycle());
  return c;
}

#define calc_size(f, n)				\
  ((sizeof(cell_t)				\
    + ((intptr_t)&(((cell_t *)0)->f[n]) - 1))	\
   / sizeof(cell_t))

int calculate_cells(int n) {
  return calc_size(arg, n);
}

int calculate_list_size(int n) {
  return calc_size(ptr, n);
}

int calculate_val_size(int n) {
  return calc_size(val, n);
}

int closure_cells(cell_t *c) {
  return calculate_cells(closure_args(c));
}

void cell_free(cell_t *c) {
  c->func = 0;
  c->next = cells_ptr;
  c->prev = cells_ptr->prev;
  cells_ptr->prev = c;
  c->prev->next = c;
}

void closure_shrink(cell_t *c, int s) {
  if(!is_cell(c)) return;
  int i, size = closure_cells(c);
  if(size > s) {
    assert(is_closure(c));
    for(i = s; i < size; i++) {
      c[i].func = 0;
      c[i].prev = &c[i-1];
      c[i].next = &c[i+1];
    }
    c[s].prev = cells_ptr->prev;
    cells_ptr->prev->next = &c[s];
    c[size-1].next = cells_ptr;
    cells_ptr->prev = &c[size-1];
    assert(check_cycle());
    measure.current_alloc_cnt -= size - s;
  }
}

void closure_free(cell_t *c) {
  closure_shrink(c, 0);
}

bool type_match(type_t t, cell_t *c) {
  type_t ta = t & T_EXCLUSIVE;
  type_t tb = c->type & T_EXCLUSIVE;
  return ta == T_ANY || tb == T_ANY || ta == tb;
}

bool func_reduced(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  assert(is_closure(c));
  measure.reduce_cnt--;
  if(c->type != T_FAIL &&
     type_match(t, c)) {
    if(is_any(c)) {
      /* create placeholder */
      if((t & T_EXCLUSIVE) == T_LIST) {
	c->ptr[0] = func(func_placeholder, 0, 1);
	c->size = 2;
      }
      c->type |= t;
      trace(c, 0, tt_touched);
    }
    return true;
  }
  else {
    fail(cp);
    return false;
  }
}

cell_t *val(intptr_t x) {
  cell_t *c = closure_alloc(2);
  c->func = func_reduced;
  c->type = T_INT;
  c->val[0] = x;
  return c;
}

cell_t *var(type_t t) {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_VAR | t;
  return c;
}

cell_t *vector(uint32_t n) {
  cell_t *c = closure_alloc(n+1);
  c->func = func_reduced;
  c->type = T_INT;
  return c;
}

cell_t *quote(cell_t *x) {
  cell_t *c = closure_alloc(2);
  c->func = func_reduced;
  c->type = T_LIST;
  c->ptr[0] = x;
  return c;
}

cell_t *empty_list() {
  cell_t *c = closure_alloc(1);
  c->func = func_reduced;
  c->type = T_LIST;
  return c;
}

cell_t *make_list(unsigned int n) {
  cell_t *c = closure_alloc(n + 1);
  c->func = func_reduced;
  c->type = T_LIST;
  return c;
}

cell_t *append(cell_t *a, cell_t *b) {
  int n = list_size(b);
  int n_a = list_size(a);
  cell_t *e = expand(a, n);
  while(n--) e->ptr[n + n_a] = b->ptr[n];
  return e;
}

cell_t *expand(cell_t *c, unsigned int s) {
  if(!c) return 0;
  int n = closure_args(c);
  int cn_p = calculate_cells(n);
  int cn = calculate_cells(n + s);
  if(!c->n && cn == cn_p) {
    //drop(c->alt); //***
    //c->alt = 0;
    c->size += s;
    return c;
  } else {
    /* copy */
    cell_t *new = closure_alloc(n + s);
    memcpy(new, c, cn_p * sizeof(cell_t));
    if(is_placeholder(c)) trace(new, c, tt_copy);
    new->n = 0;
    traverse_ref(new, ARGS_IN | PTRS | ALT);
    new->size = n + s;
    if(is_reduced(c)) alt_set_ref(c->alt_set);
    drop(c);
    return new;
  }
}

cell_t *expand_inplace(cell_t *c, unsigned int s) {
  uintptr_t n = c->n;
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->arg[s], &c->arg[0], (c->size - 1) * sizeof(cell_t *));
  unsigned int i;
  for(i = c->size - c->out; i < c->size; ++i) {
    if(c->arg[i]) c->arg[i]->arg[0] = c;
  }
  return c;
}

cell_t *expand_inplace_dep(cell_t *c, unsigned int s) {
  uintptr_t n = c->n;
  unsigned int in = closure_in(c);
  c->n = 0;
  c = expand(c, s);
  c->n = n;

  // shift and update deps
  memmove(&c->arg[in+s], &c->arg[in], c->out * sizeof(cell_t *));
  c->out += s;
  unsigned int i;
  for(i = c->size - c->out + s; i < c->size; ++i) {
    if(c->arg[i]) c->arg[i]->arg[0] = c;
  }
  return c;
}

cell_t *compose_placeholders(cell_t *a, cell_t *b) {
  unsigned int i;
  unsigned int b_in = closure_in(b);
  unsigned int b_out = closure_out(b);
  cell_t *c = expand(b, a->size);
  memmove(c->arg + a->size + b_in,
	  c->arg + b_in,
	  b_out * sizeof(cell_t *));
  for(i = 0; i < closure_in(a); ++i)
    c->arg[b_in + i] = ref(a->arg[i]);
  for(; i < a->size; ++i)
    c->arg[b_in + i] = a->arg[i];
  drop(a);
  cell_t *ab[] = {a, b};
  trace(c, (cell_t *)ab, tt_compose_placeholders);
  return c;
}

cell_t *compose_nd(cell_t *a, cell_t *b) {
  int n = list_size(b);
  int n_a = list_size(a);
  int i = 0;
  if(n && n_a) {
    cell_t *l;
    while(!closure_is_ready(l = b->ptr[n-1]) && i < n_a) {
      cell_t *x = a->ptr[i];
      if(is_placeholder(x)) {
	if(is_placeholder(l)) {
	  b->ptr[n-1] = compose_placeholders(x, l);
	  ++i;
	  break;
	}
	a->ptr[i] = x = expand_inplace_dep(x, 1);
	b = arg_nd(l, x->arg[closure_in(x)] = dep(ref(x)), b);
      } else {
	b = arg_nd(l, ref(x), b);
	++i;
      }
    }
  }
  cell_t *e = expand(b, n_a - i);
  int j;
  for(j = n; i < n_a; ++i, ++j) {
    e->ptr[j] = ref(a->ptr[i]);
  }
  drop(a);
  return e;
}

bool is_reduced(cell_t *c) {
  return c && c->func == func_reduced;
}

// max offset is 255
bool is_offset(cell_t *c) {
  return !((intptr_t)c & ~0xff);
}

cell_t *func(reduce_t *f, unsigned int in, unsigned int out) {
  assert(out > 0);
  unsigned int args = in + out - 1;
  cell_t *c = closure_alloc(args);
  c->out = out - 1;
  c->func = f;
  if(args) c->arg[0] = (cell_t *)(intptr_t)(args - 1);
  closure_set_ready(c, false /*!args*/);
  return c;
}

#define cell_offset(f) ((cell_t **)&(((cell_t *)0)->f))

int list_size(cell_t *c) {
  return c->size ? c->size - 1 : 0;
}

int val_size(cell_t *c) {
  return c->size ? c->size - 1 : 0;
}

int closure_args(cell_t *c) {
  assert(is_closure(c));
  return c->size;
}

int closure_in(cell_t *c) {
  assert(is_closure(c) && !is_reduced(c));
  return c->size - c->out;
}

int closure_out(cell_t *c) {
  assert(is_closure(c) && !is_reduced(c));
  return c->out;
}

int closure_next_child(cell_t *c) {
  assert(is_closure(c));
  return is_offset(c->arg[0]) ? (intptr_t)c->arg[0] : 0;
}

/* arg is destructive to c */
void arg(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  int i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->arg[0]))) {
    c = expand_inplace(c, 1);
    c->arg[0] = a;
  } else if(!is_data(c->arg[i])) {
    c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0 && !is_placeholder(c)) closure_set_ready(c, closure_is_ready(a));
  } else {
    arg(&c->arg[i], a);
    if(!is_placeholder(c) &&
       closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void arg_noexpand(cell_t **cp, cell_t *a) {
  cell_t *c = *cp;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  int i = closure_next_child(c);
  if(!is_data(c->arg[i])) {
    c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    c->arg[i] = a;
    if(i == 0) closure_set_ready(c, closure_is_ready(a));
  } else {
    arg_noexpand(&c->arg[i], a);
    if(closure_is_ready(c->arg[i])) {
      if(i == 0) closure_set_ready(c, true);
      else --*(intptr_t *)&c->arg[0]; // decrement offset
    }
  }
  *cp = c;
}

void close_placeholders(cell_t *c) {
  if(!is_closure(c) ||
     closure_is_ready(c)) return;
  if(is_placeholder(c)) {
    closure_set_ready(c, closure_in(c) == 0 ? true : closure_is_ready(c->arg[0]));
  } else if(is_data(c->arg[0])) {
    close_placeholders(c->arg[0]);
  }
}

bool is_fail(cell_t *c) {
  return (c->type & T_FAIL) != 0;
}

bool is_any(cell_t *c) {
  return (c->type & T_EXCLUSIVE) == T_ANY;
}

#define traverse(r, action, flags) 			\
  do {							\
    cell_t **p;						\
    if(is_reduced(r)) {					\
      if(((flags) & PTRS) &&				\
		is_list(r)) {				\
	int i, n = list_size(r);			\
	for(i = 0; i < n; ++i) {			\
	  p = (r)->ptr + i;				\
	  action					\
	}						\
      }							\
    } else if((flags) & (ARGS | ARGS_IN)) {		\
      int i, n = ((flags) & ARGS_IN) ?			\
	closure_in(r) :					\
	closure_args(r);				\
      for(i = closure_next_child(r); i < n; ++i) {	\
	p = (r)->arg + i;				\
	if(*p) {action}					\
      }							\
    }							\
    if((flags) & ALT) {					\
      p = &(r)->alt;					\
      action						\
    }							\
  } while(0)

void traverse_mark_alt(cell_t *c) {
  traverse(c, {
      if(*p && !is_marked((*p)->alt, 1)) {
	(*p)->alt = mark_ptr((*p)->alt, 1);
	traverse_mark_alt(*p);
      }
    }, ARGS | PTRS);
}

void traverse_clear_alt(cell_t *c) {
  traverse(c, {
      if(*p && is_marked((*p)->alt, 1)) {
	(*p)->alt = clear_ptr((*p)->alt, 1);
	traverse_clear_alt(*p);
      }
    }, ARGS | PTRS);
}

cell_t *arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *t = _arg_nd(c, a, r);
  zero_tmps(r);
  //check_tmps();
  return t;
}

cell_t *_arg_nd(cell_t *c, cell_t *a, cell_t *r) {
  cell_t *t;
  assert(is_closure(c) && is_closure(a));
  assert(!closure_is_ready(c));
  int i = closure_next_child(c);
  // *** shift args if placeholder
  if(is_placeholder(c) &&
     (closure_in(c) == 0 ||
      closure_is_ready(c->arg[0]))) {
    t = modify_copy(c, r);
    cell_t *_c = clear_ptr(c->tmp, 3) ? clear_ptr(c->tmp, 3) : c;
    _c = expand_inplace(_c, 1); // ***
    _c->arg[0] = a;
    cell_t *rt = clear_ptr(r->tmp, 3);
    rt->ptr[list_size(rt)-1] = c->tmp = _c; // ***
  } else if(!is_data(c->arg[i])) {
    t = modify_copy(c, r);
    cell_t *_c = clear_ptr(c->tmp, 3) ? clear_ptr(c->tmp, 3) : c;
    _c->arg[0] = (cell_t *)(intptr_t)(i - (closure_is_ready(a) ? 1 : 0));
    _c->arg[i] = a;
    if(i == 0 && !is_placeholder(c)) closure_set_ready(_c, closure_is_ready(a));
  } else {
    t = _arg_nd(c->arg[i], a, r);
    cell_t *_c = clear_ptr(c->tmp, 3) ? clear_ptr(c->tmp, 3) : c;
    if(!is_placeholder(c) &&
       closure_is_ready(_c->arg[i])) {
      if(i == 0) closure_set_ready(_c, true);
      else --*(intptr_t *)&_c->arg[0]; // decrement offset
    }
  }
  return t;
}

cell_t *copy(cell_t *c) {
  int size = closure_cells(c);
  cell_t *new = closure_alloc_cells(size);
  memcpy(new, c, size * sizeof(cell_t));
  return new;
}

cell_t *traverse_ref(cell_t *c, uint8_t flags) {
  traverse(c, {
      if(is_closure(*p)) *p = ref(*p);
    }, flags);
  return c;
}

void store_fail(cell_t *c, cell_t *alt) {
  closure_shrink(c, 1);
  memset(c->arg, 0, sizeof(c->arg));
  c->func = func_reduced;
  c->type = T_FAIL;
  c->alt = alt;
}

void store_var(cell_t *c, type_rep_t t) {
  closure_shrink(c, 1);
  c->func = func_reduced;
  c->type = T_VAR | t;
  c->size = 0;
}

void fail(cell_t **cp) {
  cell_t *c = clear_ptr(*cp, 3);
  cell_t *alt = ref(c->alt);
  drop(c);
  if(c->func) {
    traverse(c, {
	cell_t *x = clear_ptr(*p, 3);
	drop(x);
      }, ARGS_IN);
    closure_shrink(c, 1);
    memset(c->arg, 0, sizeof(c->arg));
    c->func = func_reduced;
    c->type = T_FAIL;
  }
  *cp = alt;
}

void store_reduced(cell_t **cp, cell_t *r) {
  cell_t *c = clear_ptr(*cp, 3);
  int n = c->n;
  r->func = func_reduced;
  trace(c, r, tt_reduction);
  drop_multi(c->arg, closure_in(c));
  alt_set_ref(r->alt_set);
  int size = is_closure(r) ? closure_cells(r) : 0;
  if(size <= closure_cells(c)) {
    closure_shrink(c, size);
    memcpy(c, r, sizeof(cell_t) * size);
    if(is_cell(r)) {
      traverse_ref(r, ALT | PTRS);
      drop(r);
    }
    c->n = n;
   } else { /* TODO: must copy if not cell */
    if(!is_cell(r)) {
      cell_t *t = closure_alloc_cells(size);
      memcpy(t, r, sizeof(cell_t) * size);
      r = t;
    }
    store_lazy(cp, c, r);
  }
}

cell_t *ref(cell_t *c) {
  return(refn(c, 1));
}

cell_t *refn(cell_t *c, unsigned int n) {
  if(c) {
    assert(is_closure(c));
    c->n += n;
  }
  return c;
}

bool is_nil(cell_t *c) {
  return !c;
}

bool is_list(cell_t *c) {
  return c && is_reduced(c) && (c->type & T_EXCLUSIVE) == T_LIST;
}

bool is_var(cell_t *c) {
  return c && is_reduced(c) && (c->type & T_VAR) != 0;
}

bool is_weak(cell_t *p, cell_t *c) {
  return c && is_dep(c) && (!c->arg[0] || c->arg[0] == p);
}

void drop(cell_t *c) {
  if(!is_cell(c) || !is_closure(c)) return;
  if(!c->n) {
    cell_t *p;
    traverse(c, {
	cell_t *x = clear_ptr(*p, 3);
	/* !is_marked condition needed */
	/* during _modify_copy2 */
	if(!is_marked(*p, 2)) {
	  drop(x);
	}
      }, ALT | ARGS_IN | PTRS);
    if(is_dep(c) && !is_reduced(p = c->arg[0]) && is_closure(p)) {
      /* mark dep arg as gone */
      int n = closure_args(p);
      while(n--) {
	if(p->arg[n] == c) {
	  p->arg[n] = 0;
	  break;
	}
      }
    }
    if(is_reduced(c)) alt_set_drop(c->alt_set);
    if(c->func == func_id) alt_set_drop((alt_set_t)c->arg[1]);
    closure_free(c);
  } else {
    --c->n;
  }
}

cell_t *conc_alt(cell_t *a, cell_t *b) {
  if(!a) return b;
  if(!b) return a;
  cell_t *p = a, *r;
  while((r = p->alt)) p = r;
  p->alt = b;
  return a;
}

cell_t *dep(cell_t *c) {
  cell_t *n = closure_alloc(1);
  n->func = func_dep;
  n->arg[0] = c;
  return n;
}

bool func_dep(cell_t **cp, type_rep_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  /* rely on another cell for reduction */
  /* don't need to drop arg, handled by other function */
  cell_t *p = ref(c->arg[0]);
  reduce_partial(&p);
  if(p) drop(p);
  else fail(cp);
  return false;
}

bool is_dep(cell_t *c) {
  return c->func == func_dep;
}

// *** fix arg()'s
cell_t *compose_expand(cell_t *a, unsigned int n, cell_t *b) {
  assert(is_closure(a) &&
	 is_closure(b) && is_list(b));
  assert(n);
  bool ph_a = is_placeholder(a);
  int i, bs = list_size(b);
  if(bs) {
    cell_t **l = &b->ptr[bs-1];
    cell_t *d = 0;
    int nd = 0;
    if(ph_a && is_placeholder(*l)) {
      *l = compose_placeholders(a, *l);
      return b;
    }
    while((ph_a || n > 1) &&
	  !closure_is_ready(*l)) {
      d = dep(NULL);
      arg(l, d);
      arg(&a, d);
      d->arg[0] = ref(a);
      ++nd;
      if(ph_a) ++a->out;
      else --n;
    }
    if(!closure_is_ready(*l)) {
      arg(l, a);
      // *** messy
      for(i = closure_in(*l); i < closure_args(*l); ++i) {
	drop((*l)->arg[i]->arg[0]);
	(*l)->arg[i]->arg[0] = ref(*l);
      }
      return b;
    }
  }

  b = expand(b, n);

  for(i = 0; i < n-1; ++i) {
    cell_t *d = dep(ref(a));
    b->ptr[bs+i] = d;
    arg(&a, d);
  }
  b->ptr[bs+n-1] = a;
  return b;
}

cell_t *pushl_val(intptr_t x, cell_t *c) {
  int n = val_size(c);
  c = expand(c, 1);
  c->val[n] = x;
  return c;
}

/* b is a list, a is a closure */
cell_t *pushl_nd(cell_t *a, cell_t *b) {
  assert(is_closure(a) &&
	 is_closure(b) && is_list(b));

  int n = list_size(b);
  if(n) {
    cell_t *l = b->ptr[n-1];
    if(!closure_is_ready(l)) {
      cell_t *_b = arg_nd(l, a, b);
      if(is_placeholder(l)) trace(_b->ptr[n-1], l, tt_copy);
      return _b;
    }
  }

  cell_t *e = expand(b, 1);
  e->ptr[n] = a;
  return e;
}

intptr_t bm(int k, int v) {
  assert(k < BM_SIZE);
  return ((intptr_t)1 << (k + BM_SIZE)) |
    (((intptr_t)v & 1) << k);
}

alt_set_t bm_intersect(alt_set_t a, alt_set_t b) {
  return a & b;
}

alt_set_t bm_union(alt_set_t a, alt_set_t b) {
  return a | b;
}

alt_set_t bm_conflict(alt_set_t a, alt_set_t b) {
  return ((a & b) >> BM_SIZE) &
    ((a ^ b) & (((intptr_t)1<<BM_SIZE)-1));
}

alt_set_t bm_overlap(alt_set_t a, alt_set_t b) {
  return a | (b & (a >> BM_SIZE));
}

void set_bit(uint8_t *m, unsigned int x) {
  m[x >> 3] |= 1 << (x & 7);
}

void clear_bit(uint8_t *m, unsigned int x) {
  m[x >> 3] &= ~(1 << (x & 7));
}

bool check_bit(uint8_t *m, unsigned int x) {
  return m[x >> 3] & (1 << (x & 7));
}

//cell_t *collect;

/* reassemble a fragmented cell */
/*
bool func_collect(cell_t *c) {
  collect->alt = c->alt;
  collect->n = c->n;
  collect->func = (reduce_t *)c->arg[0];
  cell_t **dest = collect->arg;
  cell_t **src = c->arg+1;
  cell_t *prev = 0;
  int n = sizeof(c->arg)-1;
  do {
    while(--n > 0) {
      *dest++ = *src++;
    }
    src = (cell_t **)*src;
    cell_free(prev);
    prev = (cell_t *)src;
    n = sizeof(cell_t)-1;
    src++;
  } while(src);
  bool b = reduce(collect);
  return store_reduced(c, collect, b);
}
*/

void *lookup(void *table, unsigned int width, unsigned int rows, const char *key) {
  unsigned int low = 0, high = rows, pivot;
  int c;
  void *entry, *ret = 0;
  int key_length = strnlen(key, width);
  while(high > low) {
    pivot = low + ((high - low) >> 1);
    entry = table + width * pivot;
    c = strncmp(key, entry, key_length);
    if(c == 0) {
      /* keep looking for a lower key */
      ret = entry;
      high = pivot;
    } else if(c < 0) high = pivot;
    else low = pivot + 1;
  }
  return ret;
}

void *lookup_linear(void *table, unsigned int width, unsigned int rows, const char *key) {
  void *entry = table;
  unsigned int rows_left = rows;
  int key_length = strnlen(key, width);
  while(rows_left-- && *(uint8_t *)entry) {
    if(!strncmp(key, entry, key_length)) return entry;
    entry += width;
  }
  return NULL;
}

cell_t *modify_copy(cell_t *c, cell_t *r) {
  cell_t *new = _modify_copy1(c, r, true);
  if(new && new != r) {
    ref(new);
    drop(r);
  }
  if(new) {
    _modify_copy2(new);
    return new;
  } else return r;
}

void check_tmps() {
  unsigned int i = 0;
  cell_t *p;
  while(i < LENGTH(cells)) {
    p = &cells[i];
    if(is_closure(p)) {
      /*
      if(p->tmp) {
	printf("<<%d %d>>\n", i, (int)p->tmp);
	drop(clear_ptr(p->tmp, 3));
	p->tmp = 0;
      }
      */
      assert(!p->tmp);
      i += closure_cells(p);
    } else ++i;
  }
}

void zero_tmps(cell_t *r) {
  if(!r || !r->tmp) return;

  cell_t *t = clear_ptr(r->tmp, 3);
  r->tmp = 0;
  zero_tmps(t);
  drop(t);

  traverse(r, {
      zero_tmps(clear_ptr(*p, 3));
    }, ARGS | PTRS | ALT);
}

/* ref count not from deps */
int nondep_n(cell_t *c) {
  if(!is_closure(c)) return 0;
  int nd = c->n;
  if(is_dep(c)) return nd;
  else if(!is_reduced(c)) {
    int n = closure_args(c);
    while(n-- &&
	  is_closure(c->arg[n]) &&
	  is_dep(c->arg[n]) &&
	  c->arg[n]->arg[0] == c) --nd;
  }
  return nd;
}

void _modify_new(cell_t *r, bool u) {
  cell_t *n;
  if(clear_ptr(r->tmp, 3)) return;
  if(u) {
    n = ref(r);
  } else {
    n = copy(r);
    n->tmp = (cell_t *)3;
    n->n = 0;
  }
  r->tmp = mark_ptr(n, 3);
}

/* first sweep of modify_copy */
/* c is the cell to be modified, or if 0, */
/* alts will be modified */
cell_t *_modify_copy1(cell_t *c, cell_t *r, bool up) {
  if(!is_closure(r)) return 0;

  r = clear_ptr(r, 3);
  int nd = nondep_n(r);

  /* is r unique (okay to replace)? */
  bool u = up && !nd;

  if(r->tmp) {
    assert(is_marked(r->tmp, 3));
    /* already been replaced */
    return clear_ptr(r->tmp, 3);
  } else r->tmp = (cell_t *)3;
  if(c == r) _modify_new(r, u);
  traverse(r, {
      if(_modify_copy1(c, *p, u))
	_modify_new(r, u);
    }, ARGS | PTRS | ALT);
  return clear_ptr(r->tmp, 3);
}

cell_t *get_mod(cell_t *r) {
  if(!r) return 0;
  cell_t *a = r->tmp;
  if(is_marked(a, 2)) return clear_ptr(a, 3);
  else return 0;
}

/* second sweep of modify copy */
void _modify_copy2(cell_t *r) {

  /* r is modified in place */
  bool s = r == clear_ptr(r->tmp, 3);

  if(!is_closure(r)) return;
  /* alread been here */
  if(!is_marked(r->tmp, 1)) return;
  r->tmp = clear_ptr(r->tmp, 1);
  traverse(r, {
      cell_t *u = clear_ptr(*p, 3);
      cell_t *t = get_mod(u);
      if(t) {
	if(!(s && t == u)) {
	  *p = ref(t);
	  if(s) drop(u);
	}
	_modify_copy2(t);
      } else if(!s) ref(u);
      if((!s || t != u) && is_weak(r, *p)) {
	--(*p)->n;
      }
    }, ARGS | PTRS | ALT);
}

cell_t *mod_alt(cell_t *c, cell_t *alt, alt_set_t alt_set) {
  cell_t *n;
  if(c->alt == alt &&
     c->alt_set == alt_set) return c;
  alt_set_ref(alt_set);
  if(!c->n) {
    n = c;
    drop(c->alt);
    alt_set_drop(c->alt_set);
  } else {
    --c->n;
    n = copy(c);
    traverse_ref(n, ARGS | PTRS);
    n->n = 0;
  }
  n->alt = alt;
  n->alt_set = alt_set;
  return n;
}

alt_set_t alt_set_ref(alt_set_t alt_set) {
  /*
  uintptr_t bit = (uintptr_t)1 << (sizeof(uintptr_t) * 4);
  uintptr_t *live = alt_live;
  while(bit) {
    if(alt_set & bit) ++*live;
    ++live;
    bit <<= 1;
  }
  */
  return alt_set;
}

alt_set_t alt_set_drop(alt_set_t alt_set) {
  /*
  uintptr_t bit = (uintptr_t)1 << (sizeof(uintptr_t) * 4);
  uintptr_t *live = alt_live;
  while(bit) {
    if(alt_set & bit) {
      assert(*live);
      --*live;
    }
    ++live;
    bit <<= 1;
  }
  */
  return alt_set;
}

uint8_t new_alt_id(uintptr_t n) {
  /*
  uint8_t r = 0;
  while(r < ALT_SET_IDS) {
    if(alt_live[r]) ++r;
    else {
      alt_live[r] = n;
      return r;
    }
  }
  assert(false);
  return -1;
  */
  return alt_cnt++;
}

bool reduce_and_check(cell_t *c, type_t *const types, int n, alt_set_t *alt_set) {

  /* reduce all args until failure */
  int i = n;
  cell_t **p = c->arg;
  while(i) {
    if(!reduce(p++, types[i])) return false; //***
    --i;
  }

  /* generate alts */
  c->alt = closure_split(c, n);

  /* check for conflicting args */
  i = n;
  p = c->arg;
  while(i--) {
    alt_set_t p_as = (*p++)->alt_set;
    if(bm_conflict(*alt_set, p_as))
      return false;
    *alt_set |= p_as;
  }

  return true;
}

void get_args(cell_t **dest, cell_t *const *src, int n) {
  int i;
  for(i = 0; i < n; i++) {
    *dest++ = *src++;
  }
}

bool handle_types(type_t *const types, cell_t **arg, cell_t **res, int n) {
  int i;
  cell_t **p = arg;
  type_t *t = types + 1;
  bool contains_var = false;
  for(i = 0; i < n; i++) {
    cell_t *a = *p++;
    type_t type = *t++;
    if(!type_match(type, a)) return false;
    if(is_var(a)) contains_var = true;
  }
  if(contains_var) {
    *res = var(types[0]);
  }
  return true;
}

void drop_multi(cell_t **a, int n) {
  int i;
  for(i = 0; i < n; i++) drop(*a++);
}

bool function_preamble(cell_t *c,
		       alt_set_t *alt_set,
		       cell_t **arg,
		       type_t *const types,
		       cell_t **res,
		       int n) {
  return reduce_and_check(c, types, n, alt_set) &&
    (get_args(arg, c->arg, n),
     handle_types(types, arg, res, n));
}

void function_epilogue(cell_t **cp,
		       alt_set_t alt_set,
		       cell_t *res,
		       int n) {
  res->alt_set = alt_set_ref(alt_set);
  res->alt = (*cp)->alt;
  store_reduced(cp, res);
}

void store_lazy(cell_t **cp, cell_t *c, cell_t *r) {
  if(c->n) {
    --c->n;
    closure_shrink(c, 1);
    c->func = func_id;
    c->size = 1;
    c->out = 0;
    c->arg[0] = ref(r);
    c->arg[1] = 0;
  } else closure_free(c);
  *cp = r;
}

void store_lazy_dep(cell_t *c, cell_t *d, cell_t *r) {
  if(d) {
    --c->n;
    d->func = func_id;
    d->size = 1;
    d->out = 0;
    d->arg[0] = r;
    d->arg[1] = 0;
  } else drop(r);
}

/*
type_rep_t tr_next(type_rep_t t) {
  type_rep_t p = t;
  int level = 0;
  do {
    if(!*p) return NULL;
    level += *p++ == '.' ? -1 : 1;
  } while(level > 0);
  return p;
}

type_rep_t tr_arg(type_rep_t t, unsigned int n) {
  if(!t[0] || t[0] == '.') return NULL;
  type_rep_t p = t + 1;
  while(n--) p = tr_next(p);
  return p;
}
*/

bool func_placeholder(cell_t **cp, type_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  int i, in = closure_in(c), n = closure_args(c);
  for(i = 0; i < in; ++i) {
    if(!reduce(&c->arg[i], T_ANY)) goto fail;
    trace(c->arg[i], 0, tt_force);
  }
  for(i = in; i < n; ++i) {
    cell_t *d = c->arg[i];
    if(!d) continue;
    drop(d->arg[0]);
    d->func = func_reduced;
    d->size = 1;
    d->alt_set = 0;
    d->type = T_VAR;
  }
  store_reduced(cp, var(T_ROW));
  return true;

 fail:
  fail(cp);
  return false;
}

bool func_self(cell_t **cp, type_t t) {
  cell_t *c = clear_ptr(*cp, 3);
  int i, in = closure_in(c), n = closure_args(c);
  for(i = 0; i < in; ++i) {
    if(!reduce(&c->arg[i], T_ANY)) goto fail;
    trace(c->arg[i], 0, tt_force);
  }
  for(i = in; i < n; ++i) {
    cell_t *d = c->arg[i];
    if(!d) continue;
    drop(d->arg[0]);
    d->func = func_reduced;
    d->size = 1;
    d->alt_set = 0;
    d->type = T_VAR;
  }
  store_reduced(cp, var(T_ANY));
  return true;

 fail:
  fail(cp);
  return false;
}

bool is_placeholder(cell_t *c) {
  return c && clear_ptr(c->func, 3) == func_placeholder;
}
