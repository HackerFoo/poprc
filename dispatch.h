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

#ifndef __DISPATCH__
#define __DISPATCH__

/** @file
 *  @brief Function dispatch for variable arguments
 */

// GET ________________________________________________________________________________

/** Accessor macro for simulated tuples.
 * GET(N, (x0, x1, ..., xN)) returns xN
 */
#define GET(n, t) CONCAT(GET, n) t
#define GET0(x0, ...) x0
#define GET1(x0, x1, ...) x1
#define GET2(x0, x1, x2, ...) x2
#define GET3(x0, x1, x2, x3, ...) x3
#define GET4(x0, x1, x2, x3, x4, ...) x4
#define GET5(x0, x1, x2, x3, x4, x5, ...) x5
#define GET6(x0, x1, x2, x3, x4, x5, x6, ...) x6
#define GET7(x0, x1, x2, x3, x4, x5, x6, x7, ...) x7
#define GET8(x0, x1, x2, x3, x4, x5, x6, x7, x8, ...) x8
#define GET20(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, ...) x20

// DISPATCH ________________________________________________________________________________

/** Return the number of arguments */
#define ARG_COUNT(...) GET20(_X, ##__VA_ARGS__, _19, _18, _17, _16, _15, _14, _13, _12, _11, _10, _9, _8, _7, _6, _5, _4, _3, _2, _1, _0)

/** macro to allow handling optional macro arguments
 * DISPATCH(MACRO_NAME, ...) calls MACRO_NAME_N(...) where N is the number of arguments other than MACRO_NAME.
 * The typical use is:
 * #define MACRO_NAME(...) DISPATCH(MACRO_NAME, __VA_ARGS__)
 * to define an overloaded macro that takes a variable number of arguments
 * @snippet test.c macro_dispatch
 */
#define DISPATCH(m, ...) CONCAT(m, ARG_COUNT(__VA_ARGS__))(__VA_ARGS__)

// FORARG ________________________________________________________________________________

#define FORARG_2(name, x0)                              \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _only)(GET(0, CONCAT(name, _args)), x0)  \
  CONCAT(name, _post)
#define FORARG_3(name, x0, x1)                          \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(1, CONCAT(name, _args)), x0) \
  CONCAT(name, _last)(x1)                               \
  CONCAT(name, _post)
#define FORARG_4(name, x0, x1, x2)                      \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(2, CONCAT(name, _args)), x0) \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _last)(x2)                               \
  CONCAT(name, _post)
#define FORARG_5(name, x0, x1, x2, x3)                  \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(3, CONCAT(name, _args)), x0) \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _last)(x3)                               \
  CONCAT(name, _post)
#define FORARG_6(name, x0, x1, x2, x3, x4)              \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(4, CONCAT(name, _args)), x0) \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _middle)(x3)                             \
  CONCAT(name, _last)(x4)                               \
  CONCAT(name, _post)
#define FORARG_7(name, x0, x1, x2, x3, x4, x5)          \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(5, CONCAT(name, _args)), x0) \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _middle)(x3)                             \
  CONCAT(name, _middle)(x4)                             \
  CONCAT(name, _last)(x5)                               \
  CONCAT(name, _post)
#define FORARG_8(name, x0, x1, x2, x3, x4, x5, x6)      \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(GET(6, CONCAT(name, _args)), x0) \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _middle)(x3)                             \
  CONCAT(name, _middle)(x4)                             \
  CONCAT(name, _middle)(x5)                             \
  CONCAT(name, _last)(x6)                               \
  CONCAT(name, _post)
#define FORARG_9(name, x0, x1, x2, x3, x4, x5, x6, x7)          \
  CONCAT(name, _pre)                                            \
  CONCAT(name, _first)(GET(7, CONCAT(name, _args)), x0)         \
  CONCAT(name, _middle)(x1)                                     \
  CONCAT(name, _middle)(x2)                                     \
  CONCAT(name, _middle)(x3)                                     \
  CONCAT(name, _middle)(x4)                                     \
  CONCAT(name, _middle)(x5)                                     \
  CONCAT(name, _middle)(x6)                                     \
  CONCAT(name, _last)(x7)                                       \
  CONCAT(name, _post)
#define FORARG_10(name, x0, x1, x2, x3, x4, x5, x6, x7, x8)     \
  CONCAT(name, _pre)                                            \
  CONCAT(name, _first)(GET(8, CONCAT(name, _args)), x0)         \
  CONCAT(name, _middle)(x1)                                     \
  CONCAT(name, _middle)(x2)                                     \
  CONCAT(name, _middle)(x3)                                     \
  CONCAT(name, _middle)(x4)                                     \
  CONCAT(name, _middle)(x5)                                     \
  CONCAT(name, _middle)(x6)                                     \
  CONCAT(name, _middle)(x7)                                     \
  CONCAT(name, _last)(x8)                                       \
  CONCAT(name, _post)

/** Create a macro that iterates over its arguments.
 * FORARG is a flexible iteration macro that can expand its
 * arguments into repeated actions.
 *
 * FORARG(NAME, ...) where
 * #define NAME_pre
 *   expanded before FORARG(NAME, ...)
 * #define NAME_first(s, x)
 *   s is selected from NAME_args based on the number of arguments
 *   x is the first argument
 * #define NAME_middle(x)
 *   x is an argument that is neither first nor last
 * #define NAME_last(x)
 *   x is the last argument
 * #define NAME_only(x)
 *   x is the only argument
 * #define NAME_post
 *   expanded after FORARG(NAME, ...)
 * #define NAME_args (x0, x1, ..., xN)
 *   used in NAME_first for `s`
 */
#define FORARG(name, ...) DISPATCH(FORARG, name, __VA_ARGS__)

#define DUMMY_args (x, x, x, x, x, x, x, x, x)

// drops the first argument, useful with __VA_ARGS__
#define DROP_pre
#define DROP_first(s, x)
#define DROP_middle(x) ,x
#define DROP_last(x) ,x
#define DROP_only(s, x)
#define DROP_post
#define DROP_args DUMMY_args
#define DROP(...) FORARG(DROP, __VA_ARGS__)

#endif
