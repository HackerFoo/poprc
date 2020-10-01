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

#define LET(x, y) __typeof__(y) x = (y)


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

#define FORMASK(i, j, mask)                                             \
  for(uintptr_t __mask0 = (mask), __z = ctz(__mask0), __mask = __mask0 >> __z, \
        i = 0, j = __z; __mask;                                         \
      __mask >>= 1,                                                     \
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
#define SEG(x) ((seg_t) {(x), sizeof(x) - 1})

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
    LET(__v, v);                                \
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
    LET(__l, l);                                \
    LET(__v, v);                                \
    __v->f = *__l;                              \
    *__l = __v;                                 \
  })



// MATH ________________________________________

#define min(a, b)                               \
  ({                                            \
    LET(__a, a);                                \
    LET(__b, b);                                \
    __a <= __b ? __a : __b;                     \
  })

#define max(a, b)                               \
  ({                                            \
    LET(__a, a);                                \
    LET(__b, b);                                \
    __a >= __b ? __a : __b;                     \
  })

#define clamp(lo, hi, x)                        \
  ({                                            \
    LET(__x, x);                                \
    LET(__lo, lo);                              \
    LET(__hi, hi);                              \
    __x > __hi ? __hi :                         \
      __x < __lo ? __lo :                       \
      __x;                                      \
  })

/** Non-negative saturating subtraction. */
#define csub(a, b)                              \
  ({                                            \
    LET(__a, a);                                \
    LET(__b, b);                                \
    __b > __a ? 0 : __a - __b;                  \
  })

/** Integer division rounding up. */
#define DIV_UP(n, d)                            \
  ({                                            \
    LET(__d, d);                                \
    (__d - 1 + (n)) / __d;                      \
  })

#define INRANGE_3(x, lo0, hi0)                  \
  ({                                            \
    LET(__x, x);                                \
    __x >= (lo0) && __x <= (hi0);               \
  })

#define INRANGE_5(x, lo0, hi0, lo1, hi1)        \
  ({                                            \
    LET(__x, x);                                \
    (__x >= (lo0) && __x <= (hi0))              \
      || (__x >= (lo1) && __x <= (hi1));        \
  })

#define INRANGE_7(x, lo0, hi0, lo1, hi1, lo2, hi2)      \
  ({                                                    \
    LET(__x, x);                                        \
    (__x >= (lo0) && __x <= (hi0))                      \
      || (__x >= (lo1) && __x <= (hi1))                 \
      || (__x >= (lo2) && __x <= (hi2));                \
  })

#define INRANGE(...) DISPATCH(INRANGE, __VA_ARGS__)

#define ONEOF_2(x, y0)                          \
    ((x) == (y0))

#define ONEOF_3(x, y0, y1)                      \
  ({                                            \
    LET(__x, x);                                \
    __x == (y0)                                 \
      || __x == (y1);                           \
  })

#define ONEOF_4(x, y0, y1, y2)                  \
  ({                                            \
    LET(__x, x);                                \
    __x == (y0)                                 \
      || __x == (y1)                            \
      || __x == (y2);                           \
  })

#define ONEOF_5(x, y0, y1, y2, y3)              \
  ({                                            \
    LET(__x, x);                                \
    __x == (y0)                                 \
      || __x == (y1)                            \
      || __x == (y2)                            \
      || __x == (y3);                           \
  })

#define ONEOF_6(x, y0, y1, y2, y3, y4)          \
  ({                                            \
    LET(__x, x);                                \
    __x == (y0)                                 \
      || __x == (y1)                            \
      || __x == (y2)                            \
      || __x == (y3)                            \
      || __x == (y4);                           \
  })

#define ONEOF_7(x, y0, y1, y2, y3, y4, y5)      \
  ({                                            \
    LET(__x, x);                                \
    __x == (y0)                                 \
      || __x == (y1)                            \
      || __x == (y2)                            \
      || __x == (y3)                            \
      || __x == (y4)                            \
      || __x == (y5);                           \
  })

#define ONEOF(...) DISPATCH(ONEOF, __VA_ARGS__)

#define SNAP_UP(x, d)                           \
  ({                                            \
    LET(__x, x);                                \
    LET(__d, d);                                \
    LET(__m, __x % __d);                        \
    __m ? __x + __d - __m : __x;                \
  })



// UM... OTHER STUFF ________________________________________

/** Zero an array or struct. */
#define zero(a) memset(&(a), 0, sizeof(a))
#define static_zero(a) memset(a, 0, static_sizeof(a))

/** Trace an integer variable.
 * `show(name)` will print `name = <value>`
 */
#define show(x) printf(#x " = %d\n", (int)(x))

#define FLAG_(x, flag) (((x) & (flag)) != 0)
#define FLAG_FIELD(s, t) ((s).GET(0, CONCAT(FLAG_, t)) GET(1, CONCAT(FLAG_, t)) flags)
#define FLAG_BIT(flag, t) CONCAT(CONCAT(GET(2, CONCAT(FLAG_, t)), _), flag)

/** Return `true` if the flag is set. */
#define FLAG(s, t, flag) FLAG_(FLAG_FIELD(s, t), FLAG_BIT(flag, t))

/** Return `true` if the flag is NOT set. */
#define NOT_FLAG(s, t, flag) (!FLAG(s, t, flag))

#define FLAG_SET_(x, flag) ((x) |= (flag))
#define FLAG_SET(s, t, flag) FLAG_SET_(FLAG_FIELD(s, t), FLAG_BIT(flag, t))

#define FLAG_CLEAR_(x, flag) ((x) &= ~(flag))
#define FLAG_CLEAR(s, t, flag) FLAG_CLEAR_(FLAG_FIELD(s, t), FLAG_BIT(flag, t))

#define FLAG_SET_TO_(x, flag, val) ((val) ? FLAG_SET_(x, flag) : FLAG_CLEAR_(x, flag))
#define FLAG_SET_TO(s, t, flag, val) FLAG_SET_TO_(FLAG_FIELD(s, t), FLAG_BIT(flag, t), val)

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

#if !defined(EMSCRIPTEN)
#define UNDERLINE_START "\x1b[4m"
#define UNDERLINE_END "\x1b[0m"
#else
#define UNDERLINE_START "[[u;;]"
#define UNDERLINE_END "]"
#endif
#define UNDERLINE(x) UNDERLINE_START x UNDERLINE_END

#define IRC_COLOR_red "04"
#define IRC_COLOR_blue "02"
#define IRC_COLOR_gray "14"
#define IRC_COLOR(c, str) "\x03" CONCAT(IRC_COLOR_, c) str "\x03" "99"
#define IRC_UNDERLINE_CODE "\x1f"
#define IRC_UNDERLINE(x) IRC_UNDERLINE_CODE x IRC_UNDERLINE_CODE
#define IRC_MARK(x) IRC_COLOR(red, x)
#define IRC_NOTE(x) IRC_COLOR(blue, x)

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
#define EACH(...) DISPATCH(EACH, __VA_ARGS__)

#define ANY_2(f, x0) f(x0)
#define ANY_3(f, x0, x1) (ANY_2(f, x0) || f(x1))
#define ANY_4(f, x0, x1, x2) (ANY_3(f, x0, x1) || f(x2))
#define ANY_5(f, x0, x1, x2, x3) (ANY_4(f, x0, x1, x2) || f(x3))
#define ANY_6(f, x0, x1, x2, x3, x4) (ANY_5(f, x0, x1, x2, x3) || f(x4))
#define ANY(...) DISPATCH(ANY, __VA_ARGS__)

/** Reassign a variable for the duration of the following block. */
#define SHADOW_2(var, val)                      \
  for(const __typeof__(var) __tmp = (var),      \
        *__tmpp = (((var) = (val)), &__tmp);    \
      __tmpp;                                   \
      ((var) = __tmp), __tmpp = NULL)

#define SHADOW_1(var)                           \
  for(const __typeof__(var) __tmp = (var),      \
        *__tmpp = &__tmp;                       \
      __tmpp;                                   \
      ((var) = __tmp), __tmpp = NULL)

#define SHADOW(...) DISPATCH(SHADOW, __VA_ARGS__)

#define maybe_get(s, f, d)                      \
  ({                                            \
    LET(__s, s);                                \
    __s && __s->f ? __s->f : (d);               \
  })

#endif

#define FOR_MASK(i, mask)                       \
  for(__typeof__(mask)                          \
        __mask = (mask),                        \
        __inc = ~__mask + 1,                    \
        i = 0, __done = __mask + 1;             \
      __done != __mask;                         \
      __done = i, i = (i + __inc) & __mask)

#define STR_IF(cond, str) ((cond) ? (str) : "")

#define STATIC_ALLOC(name, type, ...)               \
  extern type *name;                                \
  extern size_t name##_size;                        \
  extern size_t name##_size_init
#define STATIC_ALLOC_ALIGNED(...) STATIC_ALLOC(__VA_ARGS__)
#define STATIC_ALLOC_DEPENDENT(name, type, ...)     \
  extern type *name;                                \
  extern size_t name##_size
#define STATIC_FOREACH(i, a) COUNTUP(i, a##_size)

#define PAIR(x, y) ((pair_t) {(uintptr_t)(x), (uintptr_t)(y)})

#define MAP_GET(map, key, val)                                  \
  ({                                                            \
    pair_t *p = map_find(switch_rev_map, (uintptr_t)(key));     \
    p ? (__typeof__(val))p->second : val;                       \
  })

#define sizeof_bits(x) (sizeof(x) * 8)
