/* Copyright 2012-2018 Dustin M. DeWeese

   This file is part of the Startle library.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

#ifndef __STARTLE_MACROS__
#define __STARTLE_MACROS__

#include "startle/dispatch.h"

/** @file
 *  @brief Generally useful macros
 */

// MAKE COMPILERS HAPPY ________________________________________

#ifdef __clang__
#pragma clang diagnostic ignored "-Wgnu-statement-expression"
#endif



// MACRO UTILITIES ________________________________________

/** concatenate tokens. */
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)

#define _CONCAT_UNDERSCORE(x, y) x##_##y
#define CONCAT_UNDERSCORE(x, y) _CONCAT_UNDERSCORE(x, y)

/** Make a unique identifier. */
#define UNIQUE CONCAT(__unique_, __LINE__)

/** Convert a token to a string. */
#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)



// SIZES & OFFSETS ________________________________________

/** Size of a field. */
#define sizeof_field(s, f) sizeof(((s *)0)->f)

/** Offset of a field. */
#if !defined(offsetof)
#define offsetof(s, f) ((uintptr_t)&(((s *)0)->f))
#endif

/** Number of bytes per array element. */
#define WIDTH(a) (sizeof((a)[0]))

/** Number of array elements. */
#define LENGTH(a) (sizeof(a) / WIDTH(a))



// ITERATION MACROS ________________________________________

/** Iterate `i` from `lower` up to `upper-1`. */
#define RANGEUP(i, lower, upper) for(size_t i = (lower), __lower = (lower), __upper = ((void) __lower, (upper)); i < __upper; i++)

/** Iterate `i` from `upper-1` down to `lower`. */
#define RANGEDOWN(i, lower, upper) for(size_t i = (upper), __upper = (upper), __lower = ((void) __upper, (lower)); i-- > __lower; )

/** Iterator over the same range in reverse order. */
#define REVI(i) (__upper - 1 - (i) + __lower)

/** Iterate `i` from `n-1` to `0`.
 * @snippet test.c loops
 */
#define COUNTDOWN(i, n) RANGEDOWN(i, 0, n)

/** Iterate `i` from `0` to `n-1`. */
#define COUNTUP(i, n) RANGEUP(i, 0, n)

/** Iterate `n` times. */
#define LOOP(n) COUNTDOWN(UNIQUE, n)

/** Iterate `i` over each index of the array `a`. */
#define FOREACH(i, a) COUNTUP(i, LENGTH(a))

/** Iterate `i` over each index of map `m`. */
#define FORMAP(i, m) for(size_t i = 1; i <= *map_cnt(m); i++)

#define FORMASK(i, j, mask)                                     \
  for(uintptr_t i = 0, j = 0, __z = 0, __mask = (mask); __mask; \
      __mask >>= 1,                                             \
        __z = ctz(__mask), j += __z + 1, i++, __mask >>= __z)

// DATA STRUCTURES ________________________________________

/** Declare a bit set named `name` to store `size` bits. */
#define BITSET(name, size) uint8_t name[((size)+7)/8] = {0}

/** Declare a bit set to hold a bit for each element of the array. */
#define BITSET_INDEX(name, array) BITSET(name, LENGTH(array))

#if !defined(static_assert)
#define static_assert(expr, msg) _Static_assert(expr, msg)
#endif

/** String segment initializer.
 * Example: seg_t s = SEG("Hello");
 */
#define SEG(x) {(x), sizeof(x) - 1}

/** printf that prepends a string segment
 * @param pre print this string first
 * @param seg string segment to print next
 * @param fmt format string for remaining arguments
 */
#define printseg(pre, seg, fmt, ...)                                    \
  do {                                                                  \
    seg_t __seg = seg;                                                  \
    printf(pre "%.*s" fmt, (int)__seg.n, __seg.s , ##__VA_ARGS__);      \
  } while(0)

// building embedded lists

/** Add to tail of an intrusive list.
 * @param f field to store link.
 * @param l pointer to list tail pointer to update.
 * @param v struct to add.
 */
#define LIST_ADD(f, l, v)                       \
  ({                                            \
    __typeof__(v) __v = (v);                    \
    *(l) = __v;                                 \
    (l) = &__v->f;                              \
  })

/** Add to head of an intrusive list.
 * @param f field to store link.
 * @param l pointer to list head pointer to update.
 * @param v struct to add.
 */
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

/** Non-negative saturating subtraction. */
#define csub(a, b)                              \
  ({                                            \
    __typeof__(a) _a = (a);                     \
    __typeof__(b) _b = (b);                     \
    _b > _a ? 0 : _a - _b;                      \
  })

/** Integer division rounding up. */
#define DIV_UP(n, d)                            \
  ({                                            \
    __typeof__(d) _d = (d);                     \
    (_d - 1 + (n)) / _d;                        \
  })

#define INRANGE_3(x, lo0, hi0)                  \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x >= (lo0) && _x <= (hi0);                 \
  })

#define INRANGE_5(x, lo0, hi0, lo1, hi1)        \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    (_x >= (lo0) && _x <= (hi0))                \
      || (_x >= (lo1) && _x <= (hi1));          \
  })

#define INRANGE_7(x, lo0, hi0, lo1, hi1, lo2, hi2)      \
  ({                                                    \
    __typeof__(x) _x = (x);                             \
    (_x >= (lo0) && _x <= (hi0))                        \
      || (_x >= (lo1) && _x <= (hi1))                   \
      || (_x >= (lo2) && _x <= (hi2));                  \
  })

#define INRANGE(...) DISPATCH(INRANGE, __VA_ARGS__)

#define ONEOF_2(x, y0)                          \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0);                                 \
  })

#define ONEOF_3(x, y0, y1)                      \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0)                                  \
      || _x == (y1);                            \
  })

#define ONEOF_4(x, y0, y1, y2)                  \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0)                                  \
      || _x == (y1)                             \
      || _x == (y2);                            \
  })

#define ONEOF_5(x, y0, y1, y2, y3)              \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0)                                  \
      || _x == (y1)                             \
      || _x == (y2)                             \
      || _x == (y3);                            \
  })

#define ONEOF_6(x, y0, y1, y2, y3, y4)          \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0)                                  \
      || _x == (y1)                             \
      || _x == (y2)                             \
      || _x == (y3)                             \
      || _x == (y4);                            \
  })

#define ONEOF_7(x, y0, y1, y2, y3, y4, y5)      \
  ({                                            \
    __typeof__(x) _x = (x);                     \
    _x == (y0)                                  \
      || _x == (y1)                             \
      || _x == (y2)                             \
      || _x == (y3)                             \
      || _x == (y4)                             \
      || _x == (y5);                            \
  })

#define ONEOF(...) DISPATCH(ONEOF, __VA_ARGS__)

// UM... OTHER STUFF ________________________________________

/** Zero an array or struct. */
#define zero(a) memset(&(a), 0, sizeof(a))

/** Trace an integer variable.
 * `show(name)` will print `name = <value>`
 */
#define show(x) printf(#x " = %d\n", (int)(x))

/** Return `true` if the flag is set. */
#define FLAG(s, flag) (((s).flags & (flag)) != 0)

/** Return `true` if the flag is NOT set. */
#define NOT_FLAG(s, flag) (((s).flags & (flag)) == 0)

#define FLAG_SET(s, flag) ((s).flags |= (flag))
#define FLAG_CLEAR(s, flag) ((s).flags &= ~(flag))
#define FLAG_SET_TO(s, flag, val) ((val) ? FLAG_SET(s, flag) : FLAG_CLEAR(s, flag))

/** Shift elements in the array to the right. */
#define ARRAY_SHIFTR(array, offset, length) memmove(&(array) + (offset), &(array), (length) * sizeof(array))

/** Shift elements in the array to the left. */
#define ARRAY_SHIFTL(array, offset, length) memmove(&(array), &(array) + (offset), (length) * sizeof(array))

#define ARRAY_COPY(dst, src, length) memcpy(&(dst), &(src), (length) * sizeof(dst))

#if !defined(EMSCRIPTEN)
#define COLOR(c, str) "\x1b[" CONCAT(COLOR_, c) "m" str "\x1b[0m"
#define COLORs(str) "\x1b[%sm" str "\x1b[0m"
#define COLOR_red "37;41"
#define COLOR_blue "37;44"
#define COLOR_gray "38;5;8"
#define COLOR_normal "0"
#else
#define COLOR(c, str) "[[;" CONCAT(COLOR_, c) ";]" str "]"
#define COLORs(str) "[[;%s;]" str "]"
#define COLOR_red "#fff;#f00"
#define COLOR_blue "#fff;#00f"
#define COLOR_gray "#999;#000"
#define COLOR_normal ";"
#endif

#define MARK(x) COLOR(red, x)
#define NOTE(x) COLOR(blue, x)
#define FADE(x) COLOR(gray, x)
#define TODO MARK("TODO")
#define HACK MARK("HACK")

#define DISABLE(...)                            \
  do {                                          \
    LOG(MARK("DISABLED") " %s", __func__);      \
    return __VA_ARGS__;                         \
  } while(0)                                    \

/** Hint that the condition is unlikely.
 * if unlikely(...) { ...
 */
#define unlikely(c) (__builtin_expect((c), 0))

/** Hint that the condition is likely.
 * if likely(...) { ...
 */
#define likely(c) (__builtin_expect((c), 1))

/** Define a test. */
#define TEST(name) int test_##name()

/** Define a new format string specifier (for logging). */
#define FORMAT(name, c) void format_##name(intptr_t i)

/** Call `f` for each of the following arguments. */
#define EACH_2(f, x0) f(x0)
#define EACH_3(f, x0, x1) f(x0); f(x1)
#define EACH_4(f, x0, x1, x2) f(x0); f(x1); f(x2)
#define EACH_5(f, x0, x1, x2) f(x0); f(x1); f(x2); f(x3)
#define EACH(...) \
  do {                                          \
    DISPATCH(EACH, __VA_ARGS__);                \
  } while(0)

/** Reassign a variable for the duration of the following block. */
#define SHADOW(var, val)                        \
  for(const __typeof(var) __tmp = (var),        \
        *__tmpp = (((var) = (val)), &__tmp);    \
      __tmpp;                                   \
      ((var) = __tmp), __tmpp = NULL)

#endif
