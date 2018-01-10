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

#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"

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
} error_t;

#define assert_msg(...) DISPATCH(assert_msg, ##__VA_ARGS__)
#define assert_msg_1(cond, ...) "Assertion `" #cond "' failed."
#define assert_msg_2(cond, fmt, ...) "Assertion `" #cond "' failed: " fmt
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
                  assert_msg(cond, ##__VA_ARGS__),      \
                  ##__VA_ARGS__);                       \
    }                                                   \
  } while(0)

/** Assert an assumption.
 * Throw an unexpect error if the condition is violated, logging the following arguments.
 */
#ifdef NDEBUG
#define assert_error(...) ((void)0)
#else
#define assert_error(...) _assert_error(__VA_ARGS__)
#endif

#define _assert_error(cond, ...)                        \
  do {                                                  \
    if(!(cond)) {                                       \
      throw_error(ERROR_TYPE_UNEXPECTED,                \
                  assert_msg(cond, ##__VA_ARGS__),      \
                  ##__VA_ARGS__);                       \
    }                                                   \
  } while(0)

/** Throw an unexpected error after being called `n` times.
 * A quick way to prevent unexpected infinite/long loops.
 * `n` should be some really large number, because this counter
 * cannot be reset.
 */
#define assert_counter(n)                                               \
  do {                                                                  \
    static int counter = n;                                             \
    if(!counter--) {                                                    \
      counter = n;                                                      \
      throw_error(ERROR_TYPE_UNEXPECTED,                                \
                  "Assertion counter exhausted.");                      \
    }                                                                   \
  } while(0)

/** Catch errors.
 * `e` is a pointer to an `error_t` that will be set after an error.
 * @snippet error.c error
 */
#define catch_error(e) (current_error = (e), !!setjmp((e)->env))

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
  error_t *prev_error = current_error;
  /** [error] */
  error_t test_error;
  if(catch_error(&test_error)) {
    printf(NOTE("TEST") " ");
    print_last_log_msg();
  } else {
    COUNTUP(i, 5) {
      assert_throw(i < 3, "Don't worry, it's okay.");
      printf("i = %d\n", (int)i);
    }
  }
  /** [error] */
  current_error = prev_error;
  return 0;
}

/** A hook that can will be called in breakpoint() */
void __attribute__((weak)) breakpoint_hook();
void breakpoint_hook() {}

/** Convenient place to set a breakpoint for debugger integration. */
void breakpoint() {
  printf(NOTE("BREAKPOINT") " ");
  print_last_log_msg();
  breakpoint_hook();
}
