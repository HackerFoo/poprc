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
        **n = is_value(c) ? c->value.ptr : closure_next_arg(c),         \
        **end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : n) : &c->expr.arg[closure_args(c)]; \
      p < end;                                                          \
      p = n++)
#define TRAVERSE_alt_ptrs_args(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_args_alt_ptrs(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_args_ptrs_alt(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_ptrs_alt_args(c) TRAVERSE_alt_args_ptrs(c)
#define TRAVERSE_ptrs_args_alt(c) TRAVERSE_alt_args_ptrs(c)

#define TRAVERSE_alt_in_ptrs(c)                                         \
  for(cell_t **p = &c->alt,                                             \
        **n = is_value(c) ? c->value.ptr : closure_next_arg(c),         \
        **end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : n) : &c->expr.arg[closure_in(c)]; \
      p < end;                                                          \
      p = n++)
#define TRAVERSE_alt_ptrs_in(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_in_alt_ptrs(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_in_ptrs_alt(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_ptrs_alt_in(c) TRAVERSE_alt_in_ptrs(c)
#define TRAVERSE_ptrs_in_alt(c) TRAVERSE_alt_in_ptrs(c)

#define TRAVERSE_alt_in(c)                                      \
  for(cell_t **p = &c->alt,                                     \
        **n = closure_next_arg(c),                              \
        **end = is_value(c) ? n : &c->expr.arg[closure_in(c)];  \
      p < end;                                                  \
      p = n++)
#define TRAVERSE_in_alt(c) TRAVERSE_alt_in(c)

#define TRAVERSE_alt_ptrs(c)                                    \
  for(cell_t **p = &c->alt,                                     \
        **n = c->value.ptr,                                     \
        **end = is_list(c) ? &c->value.ptr[list_size(c)] : n;   \
      p < end;                                                  \
      p = n++)
#define TRAVERSE_ptrs_alt(c) TRAVERSE_alt_ptrs(c)

#define TRAVERSE_args_ptrs(c)                                           \
  for(cell_t **p = is_value(c) ? c->value.ptr : closure_next_arg(c),    \
        **end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : p) : &c->expr.arg[closure_args(c)]; \
      p < end;                                                          \
      p++)
#define TRAVERSE_ptrs_args(c) TRAVERSE_args_ptrs(c)

#define TRAVERSE_in_ptrs(c)                                             \
  for(cell_t **p = is_value(c) ? c->value.ptr : closure_next_arg(c),    \
        **end = is_value(c) ? (is_list(c) ? &c->value.ptr[list_size(c)] : p) : &c->expr.arg[closure_in(c)]; \
      p < end;                                                          \
      p++)
#define TRAVERSE_ptrs_in(c) TRAVERSE_in_ptrs(c)

#define TRAVERSE_args(c)                                \
  if(!is_value(c))                                      \
    for(cell_t **p = closure_next_arg(c),               \
          **end = &c->expr.arg[closure_args(c)];        \
        p < end;                                        \
        p++)

#define TRAVERSE_in(c)                          \
  if(!is_value(c))                              \
    for(cell_t **p = closure_next_arg(c),       \
          **end = &c->expr.arg[closure_in(c)];  \
        p < end;                                \
        p++)

#define TRAVERSE_out(c)                                                 \
  if(!is_value(c))                                                      \
    for(cell_t **p = &c->expr.arg[closure_args(c) - closure_out(c)],    \
          **end = &c->expr.arg[closure_args(c)];                        \
        p < end;                                                        \
        p++)

#define TRAVERSE_ptrs(c)                        \
  if(is_list(c))                                \
    for(cell_t **p = c->value.ptr,              \
          **end = &c->value.ptr[list_size(c)];  \
        p < end;                                \
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

#define PRE_NO_CONTEXT(c, func)                 \
  assert_error(!is_marked(c));                  \
  WATCH(c, #func)

#define PRE_2(c, func)                          \
  CONTEXT(#func ": %C", c);                     \
  PRE_NO_CONTEXT(c, func)

#define PRE_3(c, func, fmt)                     \
  CONTEXT(#func ": %C" fmt, c);                 \
  PRE_NO_CONTEXT(c, func)

#define PRE_4(c, func, fmt, x)                  \
  CONTEXT(#func ": %C" fmt, c, x);              \
  PRE_NO_CONTEXT(c, func)

#define PRE_5(c, func, fmt, x, y)               \
  CONTEXT(#func ": %C" fmt, c, x, y);           \
  PRE_NO_CONTEXT(c, func)

#define PRE_6(c, func, fmt, x, y, z)            \
  CONTEXT(#func ": %C" fmt, c, x, y, z);        \
  PRE_NO_CONTEXT(c, func)

#define PRE(...) DISPATCH2(PRE, ##__VA_ARGS__)

#define WATCH_2(c, msg)                                         \
  do {                                                          \
    int i = get_watch(c);                                       \
    if unlikely(i) {                                            \
      LOG_NOBREAK(NOTE("WATCH") " %d " msg " %C", i, c);        \
      breakpoint();                                             \
    }                                                           \
  } while(0)

#define WATCH_4(c, msg, fmt, x)                                         \
  do {                                                                  \
    int i = get_watch(c);                                               \
    if unlikely(i) {                                                    \
      LOG_NOBREAK(NOTE("WATCH") " %d " msg " %C " fmt, i, c, (x));      \
      breakpoint();                                                     \
    }                                                                   \
  } while(0)

#define WATCH(...) DISPATCH3(WATCH, ##__VA_ARGS__)

#define CUT(c, field)                           \
    ({                                          \
      cell_t *tmp = ref(c->field);              \
      drop(c);                                  \
      tmp;                                      \
    })

// define away WORD annotations
#define WORD(...)
#define OP(name) response func_##name(cell_t **cp, UNUSED type_request_t treq)

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

#define CHECK_1(x)                              \
  do {                                          \
    rsp = (x);                                  \
    if(rsp) goto abort;                         \
  } while(0)
#define CHECK_2(x, v)                           \
  do {                                          \
    if(x) {                                     \
      rsp = (v);                                \
      goto abort;                               \
    }                                           \
  } while(0)
#define CHECK(...) DISPATCH(CHECK, __VA_ARGS__)

#define ABORT(x)                                \
  do {                                          \
    rsp = (x);                                  \
    goto abort;                                 \
  } while(0)

#endif
