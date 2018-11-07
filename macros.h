/* Copyright 2012-2018 Dustin DeWeese
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

#ifndef __MACROS__
#define __MACROS__

// chunky list iterators
#define FORLIST_3(x, l, r) for(list_iterator_t __it = list_begin(l); (x = list_next(&__it, (r))) ; )
#define FORLIST_2(x, l) FORLIST_3(x, l, false)
#define FORLIST(...) DISPATCH(FORLIST, ##__VA_ARGS__)

#define WHILELIST_3(x, it, r) while((x = list_next(&it, (r))))
#define WHILELIST_2(x, it) WHILELIST_3(x, it, false)
#define WHILELIST(...) DISPATCH(WHILELIST, ##__VA_ARGS__)

#define TRAVERSE_alt_args_ptrs(c)                                       \
  for(cell_t **p = &c->alt,                                             \
        **_n = is_value(c) ? c->value.ptr : closure_next_arg(c),        \
        **_end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : _n) : &c->expr.arg[closure_args(c)]; \
      p < _end;                                                         \
      p = _n++)
#define TRAVERSE_alt_ptrs_args(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_args_alt_ptrs(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_args_ptrs_alt(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_ptrs_alt_args(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_ptrs_args_alt(c) TRAVERSE_alt_args_ptrs(c)

#define TRAVERSE_alt_in_ptrs(c)                                         \
  for(cell_t **p = &c->alt,                                             \
        **_n = is_value(c) ? c->value.ptr : closure_next_arg(c),        \
        **_end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : _n) : &c->expr.arg[closure_in(c)]; \
      p < _end;                                                         \
      p = _n++)
#define TRAVERSE_alt_ptrs_in(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_in_alt_ptrs(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_in_ptrs_alt(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_ptrs_alt_in(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_ptrs_in_alt(c) TRAVERSE_alt_in_ptrs(c)

#define TRAVERSE_alt_in(c)                                              \
  for(cell_t **p = &c->alt,                                             \
        **_n = closure_next_arg(c),                                     \
        **_end = is_value(c) ? _n : &c->expr.arg[closure_in(c)];        \
      p < _end;                                                         \
      p = _n++)
#define TRAVERSE_in_alt(c) TRAVERSE_alt_in(c)

#define TRAVERSE_alt_ptrs(c)                                    \
  for(cell_t **p = &c->alt,                                     \
        **_n = c->value.ptr,                                    \
        **_end = is_list(c) ? &c->value.ptr[list_size(c)] : _n; \
      p < _end;                                                 \
      p = _n++)
#define TRAVERSE_ptrs_alt(c) TRAVERSE_alt_ptrs(c)

#define TRAVERSE_args_ptrs(c)                                           \
  for(cell_t **p = is_value(c) ? c->value.ptr : closure_next_arg(c),    \
        **_end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : p) : &c->expr.arg[closure_args(c)]; \
      p < _end;                                                         \
      p++)
#define TRAVERSE_ptrs_args(c) TRAVERSE_args_ptrs(c)

#define TRAVERSE_in_ptrs(c)                                             \
  for(cell_t **p = is_value(c) ? c->value.ptr : closure_next_arg(c),    \
        **_end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : p) : &c->expr.arg[closure_in(c)]; \
      p < _end;                                                         \
      p++)
#define TRAVERSE_ptrs_in(c) TRAVERSE_in_ptrs(c)

#define TRAVERSE_args(c)                                \
  if(!is_value(c))                                      \
    for(cell_t **p = closure_next_arg(c),               \
          **_end = &c->expr.arg[closure_args(c)];       \
        p < _end;                                       \
        p++)

#define TRAVERSE_in(c)                          \
  if(!is_value(c))                              \
    for(cell_t **p = closure_next_arg(c),       \
          **_end = &c->expr.arg[closure_in(c)]; \
        p < _end;                               \
        p++)

#define TRAVERSE_const_in(c)                            \
  if(!is_value(c))                                      \
    for(cell_t *const *p = closure_next_arg_const(c),   \
          *const *_end = &c->expr.arg[closure_in(c)];   \
        p < _end;                                       \
        p++)

#define TRAVERSE_out(c)                                                 \
  if(!is_value(c))                                                      \
    for(cell_t **p = &c->expr.arg[closure_args(c) - closure_out(c)],    \
          **_end = &c->expr.arg[closure_args(c)];                       \
        p < _end;                                                       \
        p++)

#define TRAVERSE_ptrs(c)                        \
  if(is_list(c))                                \
    for(cell_t **p = c->value.ptr,              \
          **_end = &c->value.ptr[list_size(c)]; \
        p < _end;                               \
        p++)

#define CONCAT_ARGS_1(w)          w
#define CONCAT_ARGS_2(w, x)       CONCAT_ARGS_1(CONCAT_UNDERSCORE(w, x))
#define CONCAT_ARGS_3(w, x, y)    CONCAT_ARGS_2(CONCAT_UNDERSCORE(w, x), y)
#define CONCAT_ARGS_4(w, x, y, z) CONCAT_ARGS_3(CONCAT_UNDERSCORE(w, x), y, z)
#define CONCAT_ARGS(...) DISPATCH(CONCAT_ARGS, ##__VA_ARGS__)

#define TRAVERSE(c, ...) CONCAT_ARGS(TRAVERSE, __VA_ARGS__)(c)

#define TRAVERSE_REF(c, ...)                    \
  TRAVERSE(c, __VA_ARGS__) {                    \
    if(is_closure(*p)) *p = ref(*p);            \
  }

#define COPY_REF(c, ...)                        \
  ({                                            \
    cell_t *nc = copy(c);                       \
    TRAVERSE_REF(nc, __VA_ARGS__);              \
    nc;                                         \
  })

// embedded linked list
#define FOLLOW_3(p, l, next) for(__typeof__(l) p = (l); p != NULL; p = p->next)

// embedded zig-zag (alternating) linked list
#define FOLLOW_4(p, q, l, next)                      \
  for(__typeof__(l) p = (l), q = p ? p->next : NULL; \
      q != NULL;                                     \
      p = q->next, q = p ? p->next : NULL)

#define FOLLOW(...) DISPATCH(FOLLOW, ##__VA_ARGS__)


// unions of alt_sets
#define AS_UNION_2(c, x)               \
  (c->expr.arg[x]->value.alt_set)
#define AS_UNION_3(c, x, y)            \
  (c->expr.arg[x]->value.alt_set |     \
   c->expr.arg[y]->value.alt_set)
#define AS_UNION_4(c, x, y, z)         \
  (c->expr.arg[x]->value.alt_set |     \
   c->expr.arg[y]->value.alt_set |     \
   c->expr.arg[z]->value.alt_set)
#define AS_UNION(c, ...) DISPATCH(AS_UNION, c, ##__VA_ARGS__)

// encode small integers as pointers
#define FLIP_PTR(p) ((void *)~(uintptr_t)(p))

// marking
#ifdef EMSCRIPTEN
#define MARK_BIT (1<<31)
#else
#define MARK_BIT 2
#endif

#define is_marked(p) (((uintptr_t)(p) & (MARK_BIT)) != 0)
#define mark_ptr(p) ((void *)((uintptr_t)(p) | (MARK_BIT)))
#define clear_ptr(p) ((void *)((uintptr_t)(p) & ~(MARK_BIT)))

#define CELL_INDEX(x) (int)((x) - cells)
#define TRACE_INDEX(x) (int)((x) - trace_cells)
#define TRACE_VAR_INDEX(x) (int)((x) - trace_cur)

#define PRE_NO_CONTEXT(func)                    \
  UNUSED response rsp = SUCCESS;                \
  cell_t *c = ctx->src = *cp;                   \
  ctx->alt_set = 0;                             \
  assert_error(!is_marked(c));                  \
  WATCH(c, #func)

#define PRE_1(func)                             \
  CONTEXT(#func ": %C", *cp);                   \
  PRE_NO_CONTEXT(func)

#define PRE_2(func, fmt)                        \
  CONTEXT(#func ": %C " fmt, *cp);              \
  PRE_NO_CONTEXT(func)

#define PRE_3(func, fmt, x)                     \
  CONTEXT(#func ": %C " fmt, *cp, x);           \
  PRE_NO_CONTEXT(func)

#define PRE_4(func, fmt, x, y)                  \
  CONTEXT(#func ": %C " fmt, *cp, x, y);        \
  PRE_NO_CONTEXT(func)

#define PRE_5(func, fmt, x, y, z)               \
  CONTEXT(#func ": %C " fmt, *cp, x, y, z);     \
  PRE_NO_CONTEXT(func)

#define PRE(...) DISPATCH2(PRE, ##__VA_ARGS__)

#define WATCH_2(c, msg)                                                 \
  do {                                                                  \
    int i = get_watch(c);                                               \
    if unlikely(i) {                                                    \
        if(i == -1) {                                                   \
          LOG_NOBREAK(NOTE("WATCH OP") " %O " msg " %C", c->op, c);     \
        } else {                                                        \
          LOG_NOBREAK(NOTE("WATCH") " %d " msg " %C", i, c);            \
        }                                                               \
        breakpoint();                                                   \
      }                                                                 \
  } while(0)

#define WATCH_4(c, msg, fmt, x)                                         \
  do {                                                                  \
    int i = get_watch(c);                                               \
    if unlikely(i) {                                                    \
        if(i == -1) {                                                   \
          LOG_NOBREAK(NOTE("WATCH OP") " %O " msg " %C " fmt,           \
                      c->op, c, (x));                                   \
        } else {                                                        \
          LOG_NOBREAK(NOTE("WATCH") " %d " msg " %C " fmt, i, c, (x));  \
        }                                                               \
        breakpoint();                                                   \
      }                                                                 \
  } while(0)

#define WATCH(...) DISPATCH3(WATCH, ##__VA_ARGS__)

#define CUT(c, field)                           \
    ({                                          \
      cell_t *tmp = ref((c)->field);            \
      drop(c);                                  \
      tmp;                                      \
    })

// define away WORD annotations
#define WORD(...)
#define WORD_ALIAS(...)
#define OP(name) response func_##name(cell_t **cp, UNUSED context_t *ctx)

#define AND0_1(a) (a)
#define AND0_2(a, b)                            \
  ({                                            \
    __typeof__(a) __a = (a);                    \
    __a ? __a : (b);                            \
  })
#define AND0_3(a, b, c) AND0_2(a, AND0_2(b, c))
#define AND0_4(a, b, c, d) AND0_3(a, b, AND0_2(c, d))
#define AND0_5(a, b, c, d, e) AND0_4(a, b, c, AND0_2(d, e))
#define AND0(...) DISPATCH(AND0, __VA_ARGS__)

#define CHECK(x)                                \
  do {                                          \
    response __rsp = (x);                       \
    if(__rsp > rsp) rsp = __rsp;                \
    if(__rsp > DELAY) goto abort;               \
  } while(0)

#define CHECK_IF(x, v)                          \
  do {                                          \
    if(x) {                                     \
      rsp = (v);                                \
      goto abort;                               \
    }                                           \
  } while(0)

#define CHECK_DELAY()                           \
  do {                                          \
    if(rsp) goto abort;                         \
  } while(0)

#define ABORT(x)                                \
  do {                                          \
    rsp = (x);                                  \
    goto abort;                                 \
  } while(0)

#define NOT_NULL(x)                                     \
  ({                                                    \
    __typeof(x) __ptr = (x);                            \
    assert_error(__ptr != NULL, "null pointer");        \
    __ptr;                                              \
  })

#define strfield(s, f) maybe_get(s, f, "null")

#define WITH(p, f, v)                           \
  ({                                            \
    LET(__p, (p));                              \
    __p->f = (v);                               \
    __p;                                        \
  })

#define CHECK_PRIORITY(p)                                       \
  do {                                                          \
    if(should_delay(ctx, PRIORITY_##p)) {                       \
      rsp = DELAY;                                              \
      if(c->op != OP_value) FLAG_SET(c->expr, EXPR_DELAYED);    \
      LOG("delay (priority %d) %C", ctx->priority, c);          \
      goto abort;                                               \
    }                                                           \
  } while(0)

#define RULE(name)                              \
  do {                                          \
    if(rule_##name(ctx)) return RETRY;          \
  } while(0)

#endif
