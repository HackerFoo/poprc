#include "rt_types.h"
#include "gen/rt.h"
#include "gen/primitive.h"


#define FUNC_OP2(__op__)					\
  do {								\
    cell_t res = { .type = T_INT };				\
    bool s = reduce(c->arg[0]) &&				\
      reduce(c->arg[1]);					\
    res.alt = closure_split(c, 2);				\
    s &= !bm_conflict(c->arg[0]->alt_set,			\
		      c->arg[1]->alt_set);			\
    res.alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;	\
    res.val = s ? c->arg[0]->val __op__ c->arg[1]->val : 0;	\
    deref(c->arg[0]);						\
    deref(c->arg[1]);						\
    return to_ref(c, &res, s);					\
  } while(0)

bool func_add(cell_t *c) { FUNC_OP2(+); }
bool func_mul(cell_t *c) { FUNC_OP2(*); }
bool func_sub(cell_t *c) { FUNC_OP2(-); }


bool func_append(cell_t *c) {
  cell_t res = { .type = T_PTR };
  bool s = reduce(c->arg[0]) && reduce(c->arg[1]);
  res.alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  res.alt_set = c->arg[0]->alt_set | c->arg[1]->alt_set;
  if(s) {
    if(!c->arg[0]->ptr)
      res.ptr = ref(c->arg[1]->ptr);
    else if(!c->arg[1]->ptr)
      res.ptr = ref(c->arg[0]->ptr);
    else {
      res.ptr = closure_alloc(2);
      res.ptr->func = func_concat;
      res.ptr->arg[0] = ref(c->arg[0]->ptr);
      res.ptr->arg[1] = ref(c->arg[1]->ptr);
      res.ptr->arg[2] = (cell_t *)res.alt_set;
    }
  }
  deref(c->arg[0]);
  deref(c->arg[1]);
  return to_ref(c, &res, s);
}

bool func_pushl(cell_t *c) {
  cell_t res = { .type = T_PTR };
  cell_t *p = c->arg[1];
  bool s = reduce(p);
  res.alt = closure_split1(c, 1);
  res.alt_set = p->alt_set;
  res.ptr = s ? compose1(c->arg[0], ref(p->ptr), res.alt_set) : NULL;
  /* TODO: need to enforce alt_set when p->ptr == 0 */
  deref(p);
  return to_ref(c, &res, s);
}

bool func_pushr(cell_t *c) {
  cell_t res = { .type = T_PTR };
  bool s = reduce(c->arg[0]) && reduce(c->arg[1]);
  res.alt = closure_split(c, 2);
  cell_t *p = c->arg[0];
  cell_t *q = c->arg[1];
  s &= !bm_conflict(p->alt_set, q->alt_set);
  res.alt_set = p->alt_set | q->alt_set;
  if(s) {
    res.ptr = compose1(ref(p->ptr), q, res.alt_set);
  } else deref(q);
  deref(p);
  return to_ref(c, &res, s);
}

bool func_quote(cell_t *c) {
  cell_t res = { .type = T_PTR,
		 .ptr = c->arg[0] };
  res.alt = closure_split1(c, 0);
  return to_ref(c, &res, true);
}


bool func_popr(cell_t *c) {
  cell_t res = { .type = T_PTR };
  cell_t res_tail = { .val = 0 };
  bool s = true;
  cell_t *head = c->arg[0];
  cell_t *tail = c->arg[1];
  if(!reduce(head) || !reduce(head->ptr)) {
    s = false;
  } else {
    copy_val(&res_tail, head->ptr);
    res_tail.alt_set = res.alt_set =
      head->alt_set | head->ptr->alt_set;
    s &= !bm_conflict(head->alt_set,
		      head->ptr->alt_set);
    if(head->ptr->alt) {
      cell_t *q = quote(ref(head->ptr->alt));
      head->alt = conc_alt(q, head->alt);
      q->alt_set = head->alt_set;
    }
    if(s) res.ptr = ref(head->ptr->next);
  }
  if(head->alt) {
    cell_t *alt;
    alt = closure_alloc(2);
    alt->func = func_popr;
    alt->arg[0] = ref(head->alt);
    alt->arg[1] = dep(alt);
    res.alt = ref(alt);
    res_tail.alt = ref(alt->arg[1]);
  }
  deref(head);
  deref(tail);
  deref(c); /* dump ref from tail to c */
  to_ref(tail, &res_tail, s);
  return to_ref(c, &res, s);
}

bool is_alt(cell_t *c) {
  return c->func == func_alt;
}

cell_t *alt() {
  cell_t *c = func(func_alt, 2);
  c->arg[2] = (cell_t *)(intptr_t)alt_cnt++;
  return c;
}

bool func_alt(cell_t *c) {
  cell_t res = { .val = 0 };
  cell_t *p = c->arg[0];
  bool s = reduce(p);
  uint8_t id = (intptr_t)c->arg[2];
  res.alt_set = p->alt_set | bm(id, c->arg[1] ? 0 : 1);
  if(p->alt) {
    res.alt = closure_alloc(2);
    res.alt->func = func_alt;
    res.alt->arg[0] = ref(p->alt);
    res.alt->arg[1] = c->arg[1];
    res.alt->arg[2] = c->arg[2];
  } else if (c->arg[1]) {
    res.alt = closure_alloc(2);
    res.alt->func = func_alt;
    res.alt->arg[0] = c->arg[1];
    res.alt->arg[1] = 0;
    res.alt->arg[2] = c->arg[2];
  }
  copy_val(&res, p);
  deref(p);
  return to_ref(c, &res, s);
}

bool func_concat(cell_t *c) {
  cell_t res = { .val = 0 };
  cell_t *p = c->arg[0], *q = c->arg[1];
  bool s = reduce(q);
  res.alt_set = (intptr_t)c->arg[2];
  s &= !bm_conflict(res.alt_set,
		    q->alt_set);
  res.alt_set |= q->alt_set;
  res.alt = closure_split1(c, 1);
  if(s) {
    if(has_next(q)) {
      res.next = closure_alloc(3);
      res.next->func = func_concat;
      res.next->arg[0] = p;
      res.next->arg[1] = ref(q->next);
      res.next->arg[2] = (cell_t *)res.alt_set;
    } else if(p) {
      if(res.alt_set) {
	res.next = closure_alloc(3);
	res.next->func = func_concat;
	res.next->arg[0] = 0;
	res.next->arg[1] = p;
	res.next->arg[2] = (cell_t *)res.alt_set;
      } else {
	res.next = p;
      }
    }
    copy_val(&res, q);
  } else deref(p);
  deref(q);
  return to_ref(c, &res, s);
}

bool func_assert(cell_t *c) {
  bool s = reduce(c->arg[0]);
  cell_t res = { .val = c->arg[0]->val,
		 .type = c->arg[0]->type };
  res.alt = closure_split1(c, 0);
  deref(c->arg[0]);
  return to_ref(c, &res, s && res.val);
}

bool func_id(cell_t *c) {
  cell_t *p = c->arg[0];
  if(reduce(p)) {
    cell_t *a = closure_split1(c, 0);
    c->func = func_reduced;
    c->alt_set = p->alt_set;
    copy_val(c, p);
    c->next = p->next;
    c->alt = a;
    deref(p);
    return true;
  } else {
    deref(p);
    return false;
  }
}

cell_t *id(cell_t *c) {
  cell_t *i = func(func_id, 1);
  arg(i, c);
  return i;
}

/*
bool func_compose(cell_t *c) {
  cell_t res = { .val = 0 };
  bool s = reduce(c->arg[0]) &&
    reduce(c->arg[1]);
  res.alt = closure_split(c, 2);
  s &= !bm_conflict(c->arg[0]->alt_set,
		    c->arg[1]->alt_set);
  res.alt_set = c->arg[0]->alt_set |
    c->arg[1]->alt_set;
  
  (void)res;

  // ???

  return true;
}
*/
