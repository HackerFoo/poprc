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

#ifndef __MACROS__
#define __MACROS__

#define sizeof_field(s, f) sizeof(((s *)0)->f)
#define offset(s, f) ((uintptr_t)&(((s *)0)->f))

#define zero(a) memset((a), 0, sizeof(a))

#define show(x) printf(#x " = %d\n", (int)(x))
#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))
#define COUNTDOWN(i, n) if(n) for(size_t i = (n); i--; )
#define COUNTUP(i, n) for(size_t i = 0, __n = (n); i < __n; i++)
#define FOREACH(i, a) COUNTUP(i, LENGTH(a))
#define FORMAP(i, m) for(size_t i = 1; i <= *map_cnt(m); i++)
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)
#define UNIQUE CONCAT(__unique_, __LINE__)
#define LOOP(n) COUNTDOWN(UNIQUE, n)
#define sizeof_field(s, f) sizeof(((s *)0)->f)

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#ifdef EMSCRIPTEN
#define MARK_BIT (1<<31)
#else
#define MARK_BIT 2
#endif

#define is_marked(p) (((uintptr_t)(p) & (MARK_BIT)) != 0)
#define mark_ptr(p) ((void *)((uintptr_t)(p) | (MARK_BIT)))
#define clear_ptr(p) ((void *)((uintptr_t)(p) & ~(MARK_BIT)))

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

#define COMMAND(name)                                    \
  {                                                      \
    .first = (uintptr_t)#name,                           \
    .second = (uintptr_t)&command_##name                 \
  }

#define BITSET(name, size) uint8_t name[((size)+7)/8] = {0}
#define BITSET_INDEX(name, array) BITSET(name, LENGTH(array))

#if !defined(static_assert)
#define static_assert(expr, msg) _Static_assert(expr, msg)
#endif

#define SEG(x) {(x), sizeof(x) - 1}

#define printseg(pre, seg, fmt, ...)                                    \
  do {                                                                  \
    seg_t __seg = seg;                                                  \
    printf(pre "%.*s" fmt, (int)__seg.n, __seg.s , ##__VA_ARGS__);      \
  } while(0)

// macro to allow handling optional macro arguments
// DISPATCH(MAC, n, x_0 .. x_m) will reduce to MACi(x_0 .. x_m), where i = n-m, so i is the number of missing arguments
#define DISPATCH(m, n, ...) CONCAT(DISPATCH, n)(m, __VA_ARGS__, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9)
#define DISPATCH0(m, argc, ...) CONCAT(m, argc)()
#define DISPATCH1(m, x0, argc, ...) CONCAT(m, argc)(x0)
#define DISPATCH2(m, x0, x1, argc, ...) CONCAT(m, argc)(x0, x1)
#define DISPATCH3(m, x0, x1, x2, argc, ...) CONCAT(m, argc)(x0, x1, x2)
#define DISPATCH4(m, x0, x1, x2, x3, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3)
#define DISPATCH5(m, x0, x1, x2, x3, x4, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4)
#define DISPATCH6(m, x0, x1, x2, x3, x4, x5, atgc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5)
#define DISPATCH7(m, x0, x1, x2, x3, x4, x5, x6, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6)
#define DISPATCH8(m, x0, x1, x2, x3, x4, x5, x6, x7, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7)
#define DISPATCH9(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, argc, ...) CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8)

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

#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)

#define LIST_ADD(f, l, v) (*(l) = (v), (l) = &(v)->f) // insert at l = tail
#define CONS(f, l, v) ((v)->f = *(l), *(l) = (v)) // insert at l = head

#define FLIP_PTR(p) ((void *)~(uintptr_t)(p))

#define FORLIST(x, l) for(list_iterator_t it = list_begin(l); (x = list_next(&it)) ; )
#define WHILELIST(x, it) while((x = list_next(&it)))

#endif
