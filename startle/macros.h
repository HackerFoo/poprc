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

#ifndef __STARTLE_MACROS__
#define __STARTLE_MACROS__

#include "dispatch.h"

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

#define FLAG(s, flag) (((s).flags & (flag)) != 0)
#define NOT_FLAG(s, flag) (((s).flags & (flag)) == 0)
#define FLAG_SET(s, flag) ((s).flags |= (flag))
#define FLAG_CLEAR(s, flag) ((s).flags &= ~(flag))
#define FLAG_SET_TO(s, flag, val) ((val) ? FLAG_SET(s, flag) : FLAG_CLEAR(s, flag))

#define ARRAY_SHIFTR(elem, x, n) memmove(&(elem) + (x), &(elem), (n) * sizeof(elem))
#define ARRAY_SHIFTL(elem, x, n) memmove(&(elem), &(elem) + (x), (n) * sizeof(elem))
#define ARRAY_COPY(dst, src, n) memcpy(&(dst), &(src), (n) * sizeof(dst))

#define COLOR(x) CONCAT(COLOR_, x)
#if !defined(EMSCRIPTEN)
#define COLOR_red "\x1b[37;41m"
#define COLOR_blue "\x1b[37;44m"
#define COLOR_gray "\x1b[38;5;8m"
#define COLOR_normal "\x1b[0m"
#else
#define COLOR_red "[[;#fff;#f00;]"
#define COLOR_blue "[[;#fff;#00f;]"
#define COLOR_gray "[[;#999;#000;]"
#define COLOR_normal "]"
#endif

#define MARK(x) COLOR(red) x COLOR(normal)
#define NOTE(x) COLOR(blue) x COLOR(normal)
#define FADE(x) COLOR(gray) x COLOR(normal)
#define TODO MARK("TODO")
#define HACK MARK("HACK")

#define DISABLE(...)                            \
  do {                                          \
    LOG(MARK("DISABLED") " %s", __func__);      \
    return __VA_ARGS__;                         \
  } while(0)                                    \

#define if_unlikely(c) if(__builtin_expect((c), 0))
#define if_likely(c) if(__builtin_expect((c), 1))

#define TEST(name) int test_##name()
#define FORMAT(name, c) void format_##name(intptr_t i)

#endif
