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
#include "gen/byte_compile.h"


void _make_index(cell_t *c, pair_t **index) {
  traverse(c, {
      if(*p && !(*p)->type & T_TRACED) {
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

cell_t trace_arr[1 << 10];
cell_t *trace_ptr = &trace_arr[0];

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
      //fb->assign(c, c->arg[0]);
    } else if(c->func == func_dep) {
      // do nothing
    } else if(c->func == func_placeholder) {
      //fb->apply_list(c);
    } else if(c->func == func_self) {
      //fb->callSelf(c);
    } else {
      //fb->call(c);
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
    } else if(!is_var(c)) {
      //fb->val(c);
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
    break;
  }
  case tt_compose_placeholders: {
    /* to do *** */
    break;
  }
  }
}

cell_t const *bc_args[1 << 10] = {0};
cell_t const **bc_args_ptr = &bc_args[0];
void bc_arg(cell_t const *c, UNUSED int x) {
  *bc_args_ptr++ = c;
}

reduce_t *byte_compile(cell_t *root, int in, int out) {
  pair_t index[1 << 10] = {0};
  set_trace(bc_trace);
  /* fb = this; */

  /* Function *f = get_cell_func(name, in, out); */
  fill_args(root, bc_arg);
  /* self = declare_wrap_func(f); */
  /* block = BasicBlock::Create(ctx, "entry", f, 0); */

  /* auto j = args.begin(); */
  /* for(auto i = f->arg_begin(); */
  /*     i != f->arg_end() && j != args.end(); */
  /*     ++i, ++j) { */
  /*   printf("%d <- *\n", *j); */
  /*   regs[*j] = i; */
  /*   cnt[*j] = 1; */
  /* } */

  reduce_root(root);

  unsigned int n = make_index(root, index);
  for(unsigned int i = 0; i < n; i++) {
    printf("index[%d] = &cells[%d]\n", i, (int)((cell_t *)index[i].first - cells));
  }

  /* Value *ret; */
  /* if(out > 1) { */
  /*   Value *agg = UndefValue::get(f->getReturnType()); */
  /*   for(unsigned int i = 0; i < out; ++i) { */
  /*     build_tree(root->ptr[out - 1 - i]); */
  /*     agg = InsertValueInst::Create(agg, wrap_alts(root->ptr[out - 1 - i]), {i}, "", block); */
  /*   } */
  /*   ret = agg; */
  /* } else { */
  /*   build_tree(root->ptr[0]); */
  /*   ret = wrap_alts(root->ptr[0]); */
  /* } */

  /* for(unsigned int i = 0; i < out; ++i) { */
  /*   cell_t *p = root->ptr[i]; */
  /*   while(p) { */
  /*     --cnt[p - cells]; */
  /*     p = p->alt; */
  /*   } */
  /* } */

  // adjust reference counts
  /* for(auto i = cnt.begin(); i != cnt.end(); ++i) { */
  /*   if(i->second < 0) { */
  /*     CallInst::Create(ext::refn(module), */
  /*       	       std::vector<Value *> { */
  /*       		 reg(i->first), */
  /*       		 ConstantInt::get(ctx, APInt(32, -i->second)) */
  /*       	       }, "", block); */
  /*   } else if(i->second > 0) { */
  /*     CallInst::Create(ext::drop(module), ArrayRef<Value *>(reg(i->first)), "", block); */
  /*   } */
  /* } */

  /* ReturnInst::Create(ctx, ret, block); */
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
