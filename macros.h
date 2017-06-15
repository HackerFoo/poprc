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

// MAKE COMPILERS HAPPY ________________________________________

#ifdef __clang__
#pragma clang diagnostic ignored "-Wgnu-statement-expression"
#endif



// MACRO UTILITIES ________________________________________

// concatenate tokens
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)

#define _CONCAT_UNDERSCORE(x, y) x##_##y
#define CONCAT_UNDERSCORE(x, y) _CONCAT_UNDERSCORE(x, y)

// make a unique identifier
#define UNIQUE CONCAT(__unique_, __LINE__)

// convert a token to a string
#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)



// SIZES & OFFSETS ________________________________________

// size of a field
#define sizeof_field(s, f) sizeof(((s *)0)->f)

// offset of a field
#if !defined(offsetof)
#define offsetof(s, f) ((uintptr_t)&(((s *)0)->f))
#endif

#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))



// ITERATION MACROS ________________________________________

// up and down ranges
#define RANGEUP(i, lower, upper) for(size_t i = (lower), __upper = (upper); i < __upper; i++)
#define RANGEDOWN(i, lower, upper) for(size_t i = (upper), __lower = (lower); i-- > __lower; )
#define COUNTDOWN(i, n) RANGEDOWN(i, 0, n)
#define COUNTUP(i, n) RANGEUP(i, 0, n)
#define LOOP(n) COUNTDOWN(UNIQUE, n)

// iterate over a container
#define FOREACH(i, a) COUNTUP(i, LENGTH(a))
#define FORMAP(i, m) for(size_t i = 1; i <= *map_cnt(m); i++)

// chunky list iterators
#define FORLIST_0(x, l, r, ...) for(list_iterator_t __it = list_begin(l); (x = list_next(&__it, (r))) ; )
#define FORLIST_1(x, l, ...) FORLIST_0(x, l, false)
#define FORLIST(...) DISPATCH(FORLIST, 3, ##__VA_ARGS__)

#define WHILELIST_0(x, it, r, ...) while((x = list_next(&it, (r))))
#define WHILELIST_1(x, it, ...) WHILELIST_0(x, it, false)
#define WHILELIST(...) DISPATCH(WHILELIST, 3, ##__VA_ARGS__)

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

#define CONCAT_ARGS_0(w, x, y, z, ...) CONCAT_ARGS_1(CONCAT_UNDERSCORE(w, x), y, z)
#define CONCAT_ARGS_1(w, x, y, ...)    CONCAT_ARGS_2(CONCAT_UNDERSCORE(w, x), y)
#define CONCAT_ARGS_2(w, x, ...)       CONCAT_ARGS_3(CONCAT_UNDERSCORE(w, x))
#define CONCAT_ARGS_3(w, ...)          w
#define CONCAT_ARGS(...) DISPATCH(CONCAT_ARGS, 4, ##__VA_ARGS__)

#define TRAVERSE(c, ...) CONCAT_ARGS(TRAVERSE, __VA_ARGS__)(c)

#define TRAVERSE_REF(c, ...)                    \
  TRAVERSE(c, __VA_ARGS__) {                    \
    if(is_closure(*p)) *p = ref(*p);            \
  }


// embedded linked list
#define FOLLOW_1(p, l, next, ...) for(__typeof__(l) p = (l); p != NULL; p = p->next)

// embedded zig-zag (alternating) linked list
#define FOLLOW_0(p, q, l, next, ...)                 \
  for(__typeof__(l) p = (l), q = p ? p->next : NULL; \
      q != NULL;                                     \
      p = q->next, q = p ? p->next : NULL)

#define FOLLOW(...) DISPATCH(FOLLOW, 4, ##__VA_ARGS__)


// CODE GENERATION ________________________________________

#define WORD_COUNT(n) { .first = (n), .second = (n) }
#define WORD(__name, __func, __in, __out)                \
  {                                                      \
    .first = (uintptr_t)__name,                          \
    .second = (uintptr_t)&(cell_t) {                     \
      .func = func_##__func,                             \
      .module_name = PRIMITIVE_MODULE_NAME,              \
      .word_name = __name "\0" #__func,                  \
      .entry = {                                         \
        .in = __in,                                      \
        .out = __out,                                    \
        .len = 0,                                        \
        .flags = ENTRY_PRIMITIVE                         \
      }                                                  \
    }                                                    \
  }

#define TEST(name)                                       \
  {                                                      \
    .first = (uintptr_t)#name,                           \
    .second = (uintptr_t)&test_##name                    \
  }



// DATA STRUCTURES ________________________________________

#define BITSET(name, size) uint8_t name[((size)+7)/8] = {0}
#define BITSET_INDEX(name, array) BITSET(name, LENGTH(array))

#if !defined(static_assert)
#define static_assert(expr, msg) _Static_assert(expr, msg)
#endif

// string segment initializer: seg_t s = SEG("Hello");
#define SEG(x) {(x), sizeof(x) - 1}

// printf that prepends a string segment
#define printseg(pre, seg, fmt, ...)                                    \
  do {                                                                  \
    seg_t __seg = seg;                                                  \
    printf(pre "%.*s" fmt, (int)__seg.n, __seg.s , ##__VA_ARGS__);      \
  } while(0)

// building embedded lists

// insert at l = tail
#define LIST_ADD(f, l, v)                       \
  ({                                            \
    __typeof__(v) __v = (v);                    \
    *(l) = __v;                                 \
    (l) = &__v->f;                              \
  })

// insert at l = head
#define CONS(f, l, v)                           \
  ({                                            \
    __typeof__(l) __l = (l);                    \
    __typeof__(v) __v = (v);                    \
    __v->f = *__l;                              \
    *__l = __v;                                 \
  })



// DISPATCH ________________________________________________________________________________

// macro to allow handling optional macro arguments
// DISPATCH(MAC, n, x_0 .. x_m) will reduce to MAC_i(x_0 .. x_m), where i = n-m, so i is the number of missing arguments
#define DISPATCH(m, n, ...) CONCAT(DISPATCH, n)(m, __VA_ARGS__, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9)
#define DISPATCH0(m, argc, ...) CONCAT(m, argc)()
#define DISPATCH1(m, x0, argc, ...) CONCAT(m, argc)(x0)
#define DISPATCH2(m, x0, x1, argc, ...) CONCAT(m, argc)(x0, x1)
#define DISPATCH3(m, x0, x1, x2, argc, ...) CONCAT(m, argc)(x0, x1, x2)
#define DISPATCH4(m, x0, x1, x2, x3, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3)
#define DISPATCH5(m, x0, x1, x2, x3, x4, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4)
#define DISPATCH6(m, x0, x1, x2, x3, x4, x5, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5)
#define DISPATCH7(m, x0, x1, x2, x3, x4, x5, x6, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6)
#define DISPATCH8(m, x0, x1, x2, x3, x4, x5, x6, x7, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7)
#define DISPATCH9(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8)



// MATH ________________________________________

#define min(a, b)                               \
  ({                                            \
    __typeof__(a) __a = (a);                    \
    __typeof__(b) __b = (b);                    \
    __a <= __b ? __a : __b;                     \
  })

#define max(a, b)                               \
  ({                                            \
    __typeof__(a) __a = (a);                    \
    __typeof__(b) __b = (b);                    \
    __a >= __b ? __a : __b;                     \
  })

// non-negative saturating subtraction
#define csub(a, b)                              \
  ({                                            \
    __typeof__(a) _a = (a);                     \
    __typeof__(b) _b = (b);                     \
    _b > _a ? 0 : _a - _b;                      \
  })


// UM... OTHER STUFF ________________________________________

// zero a thing
#define zero(a) memset((a), 0, sizeof(a))

// simple debug printf
#define show(x) printf(#x " = %d\n", (int)(x))

// unions of alt_sets
#define AS_UNION_0(c, x, y, z, ...)    \
  (c->expr.arg[x]->value.alt_set |     \
   c->expr.arg[y]->value.alt_set |     \
   c->expr.arg[z]->value.alt_set)
#define AS_UNION_1(c, x, y, ...)       \
  (c->expr.arg[x]->value.alt_set |     \
   c->expr.arg[y]->value.alt_set)
#define AS_UNION_2(c, x, ...)          \
  (c->expr.arg[x]->value.alt_set)
#define AS_UNION(c, ...) DISPATCH(AS_UNION, 4, c, ##__VA_ARGS__)

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

#define FLAG_SET(var, flag) ((var) |= (flag))
#define FLAG_CLEAR(var, flag) ((var) &= ~(flag))
#define FLAG_SET_TO(var, flag, val) ((val) ? FLAG_SET(var, flag) : FLAG_CLEAR(var, flag))

#define CELL_INDEX(x) (int)((x) - cells)
#define TRACE_INDEX(x) (int)((x) - trace_cells)
#define TRACE_VAR_INDEX(x) (int)((x) - trace_cur)

#define ARRAY_SHIFTR(elem, x, n) memmove(&(elem) + (x), &(elem), (n) * sizeof(elem))
#define ARRAY_SHIFTL(elem, x, n) memmove(&(elem), &(elem) + (x), (n) * sizeof(elem))
#define ARRAY_COPY(dst, src, n) memcpy(&(dst), &(src), (n) * sizeof(dst))

#endif
