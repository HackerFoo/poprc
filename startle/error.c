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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/static_alloc.h"

static bool breakpoint_disabled = false;

#if INTERFACE
#include <setjmp.h>

/** @file
 *  @brief Error handling
 */

typedef enum error_type_e {
  ERROR_TYPE_NONE = 0,
  ERROR_TYPE_UNEXPECTED = 1, /**< An assumption was violated, indicating incorrect operation. */
  ERROR_TYPE_LIMITATION = 2  /**< A known limitation has been reached. */
} error_type_t;

/** Used to throw and catch errors. */
typedef struct {
  jmp_buf env;
  error_type_t type;
  bool quiet;
} error_t;

#define assert_msg(...) DISPATCH(assert_msg, ##__VA_ARGS__)
#define assert_msg_1(cond, ...) "Assertion `" cond "' failed."
#define assert_msg_2(cond, fmt, ...) "Assertion `" cond "' failed: " fmt
#define assert_msg_3(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_4(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_5(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_6(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_7(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_8(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_9(cond, fmt, ...) assert_msg_2(cond, fmt)
#define assert_msg_10(cond, fmt, ...) assert_msg_2(cond, fmt)

/** Assert a limitation.
 * Throw a limitation error if the condition is violated, logging the following arguments.
 */
#define assert_throw(cond, ...)                         \
  do {                                                  \
    if(!(cond)) {                                       \
      throw_error(ERROR_TYPE_LIMITATION,                \
                  assert_msg(#cond, ##__VA_ARGS__),     \
                  ##__VA_ARGS__);                       \
    }                                                   \
  } while(0)

/** Assert an assumption.
 * Throw an unexpected error if the condition is violated, logging the following arguments.
 */
#ifdef NDEBUG
#define assert_error(...) ((void)0)
#else
#define assert_error(cond, ...) _assert_error(cond, #cond, __VA_ARGS__)
#endif

#define _assert_error(cond, cond_str, ...)              \
  do {                                                  \
    if(!(cond)) {                                       \
      throw_error(ERROR_TYPE_UNEXPECTED,                \
                  assert_msg(cond_str, ##__VA_ARGS__),  \
                  ##__VA_ARGS__);                       \
    }                                                   \
  } while(0)

/** Run following code and then throw if the condition is violated. */
#ifdef NDEBUG
#define on_assert_error(...) if(0)
#else
#define on_assert_error(cond, ...) _on_assert_error(cond, #cond, __VA_ARGS__)
#endif

/* Some explanation:
 *
 * This:
 * for(; !cond; throw_error()) {
 *   ...
 * }
 *
 * Is equivalent to this:
 * if(!cond) {
 *   ...
 *   throw_error();
 * }
 *
 * There will be no looping since throw_error() will longjmp out at the end.
 */
#define _on_assert_error(cond, cond_str, ...)                   \
  for(;!(cond);                                                 \
      ({throw_error(ERROR_TYPE_UNEXPECTED,                      \
                    assert_msg(cond_str, ##__VA_ARGS__),        \
                    ##__VA_ARGS__);}))


/** Warn when an assumption doesn't hold.
 * If the condition is violated, log the following arguments and print them.
 */
#ifdef NDEBUG
#define assert_warn(...) ((void)0)
#else
#define assert_warn(cond, ...) _assert_warn(cond, #cond, __VA_ARGS__)
#endif

#define _assert_warn(cond, ...)                 \
  do {                                          \
    if(!(cond)) {                               \
      LOG(MARK("WARN") " "                      \
          assert_msg(cond_str, ##__VA_ARGS__)   \
          DROP(__VA_ARGS__));                   \
      breakpoint();                             \
    }                                           \
  } while(0)

/** Throw an unexpected error after being called `n` times.
 * A quick way to prevent unexpected infinite/long loops.
 * `n` should be some really large number, because this counter
 * cannot be reset.
 */
#ifdef NDEBUG
#define assert_counter(n) ((void)0)
#else
#define assert_counter(n) _assert_counter(n)
#endif

#define _assert_counter(n)                                               \
  do {                                                                   \
    static unsigned int *counter = NULL;                                 \
    if(!counter) counter = alloc_counter();                              \
    if(*counter > (n)) {                                                 \
      throw_error(ERROR_TYPE_UNEXPECTED,                                 \
                  "Assertion counter exhausted: %d > " #n, X, *counter); \
    }                                                                    \
    (*counter)++;                                                        \
  } while(0)

#ifdef NDEBUG
#define assert_op(op, x, y) ((void)0)
#else
#define assert_op(op, x, y)                                     \
  do {                                                          \
    __typeof__(x) _x = (x);                                     \
    __typeof__(y) _y = (y);                                     \
    if(!(_x op _y)) {                                           \
      throw_error(ERROR_TYPE_UNEXPECTED,                        \
                  "Assertion `" #x "' [%d] " #op " `" #y        \
                  "' [%d] failed.", X, (int)_x, (int)_y);       \
    }                                                           \
  } while(0)
#endif

#define assert_eq(x, y)  assert_op(==, x, y)
#define assert_neq(x, y) assert_op(!=, x, y)
#define assert_lt(x, y)  assert_op(<,  x, y)
#define assert_le(x, y)  assert_op(<=, x, y)
#define assert_gt(x, y)  assert_op(>,  x, y)
#define assert_ge(x, y)  assert_op(>=, x, y)

/** Catch errors.
 * `e` is a pointer to an `error_t` that will be set after an error.
 * @snippet error.c error
 */
#define catch_error_1(e) catch_error_2(e, false)
#define catch_error_2(e, q) (current_error = (e), current_error->quiet = (q), !!setjmp((e)->env))
#define catch_error(...) DISPATCH(catch_error, __VA_ARGS__)
#define CATCH(...) SHADOW(current_error) if(catch_error(__VA_ARGS__))

/** Throw an error of a particular type.
 * Returns the error type and logs the following arguments.
 * @snippet error.c error
 */
#define throw_error(type, fmt, ...)                                     \
  do {                                                                  \
    LOG_NO_POS(MARK("!!!") " " __FILE__ ":" STRINGIFY(__LINE__)         \
                  ": %s: " fmt, __func__ DROP(__VA_ARGS__));            \
    breakpoint();                                                       \
    return_error(type);                                                 \
  } while(0)

#endif

error_t *current_error = NULL;

/** Return the error type to `catch_error`. */
void return_error(error_type_t type) {
  if(current_error) {
    current_error->type = type;
    longjmp(current_error->env, type);
  } else {
    exit(-1);
  }
}

TEST(error) {
  /** [error] */
  error_t test_error;
  CATCH(&test_error) {
    printf(NOTE("TEST") " ");
    print_last_log_msg();
  } else {
    COUNTUP(i, 5) {
      assert_throw(i < 3, "Don't worry, it's okay.");
      printf("i = %d\n", (int)i);
    }
  }
  /** [error] */
  return 0;
}

/** A hook that can will be called in breakpoint() */
void __attribute__((weak)) breakpoint_hook();
void breakpoint_hook() {}

/** Convenient place to set a breakpoint for debugger integration. */
void breakpoint() {
  if(breakpoint_disabled) return;
  SHADOW(breakpoint_disabled, true) { // avoid error loops
    if(!maybe_get(current_error, quiet, false)) {
      print_context(5);
      printf(NOTE("BREAKPOINT") " ");
      print_last_log_msg();
    }
    breakpoint_hook();
  }
}

STATIC_ALLOC(counters, unsigned int, 8);
static unsigned int counters_n = 0;

unsigned int *alloc_counter() {
  assert_error(counters_n < counters_size);
  return &counters[counters_n++];
}

void reset_counters() {
  static_zero(counters);
}
