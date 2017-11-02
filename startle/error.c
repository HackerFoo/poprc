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

typedef enum error_type_e {
  ERROR_TYPE_NONE = 0,
  ERROR_TYPE_UNEXPECTED = 1,
  ERROR_TYPE_LIMITATION = 2
} error_type_t;

typedef struct {
  jmp_buf env;
  error_type_t type;
} error_t;

#define assert_msg(...) DISPATCH(assert_msg, 10, ##__VA_ARGS__)
#define assert_msg_0(cond, fmt, ...) "Assertion `" #cond "' failed: " fmt
#define assert_msg_1(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_2(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_3(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_4(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_5(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_6(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_7(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_8(cond, fmt, ...) assert_msg_0(cond, fmt)
#define assert_msg_9(cond, ...) "Assertion `" #cond "' failed."

#define assert_throw(cond, ...)                         \
  do {                                                  \
    if(!(cond)) {                                       \
      throw_error(ERROR_TYPE_LIMITATION,                \
                  assert_msg(cond, ##__VA_ARGS__),      \
                  ##__VA_ARGS__);                       \
    }                                                   \
  } while(0)

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

#define assert_counter(n)                                               \
  do {                                                                  \
    static int counter = n;                                             \
    if(!counter--) {                                                    \
      counter = n;                                                      \
      throw_error(ERROR_TYPE_UNEXPECTED,                                \
                  "Assertion counter exhausted.");                      \
    }                                                                   \
  } while(0)

#define catch_error(e) (current_error = (e), !!setjmp((e)->env))

#define throw_error(type, fmt, ...)                                     \
  do {                                                                  \
    LOG_NO_POS(MARK("!!!") " " __FILE__ ":" STRINGIFY(__LINE__)         \
                  ": %s: " fmt, __func__ DROP(__VA_ARGS__));            \
    breakpoint();                                                       \
    return_error(type);                                                 \
  } while(0)

#endif

error_t *current_error = NULL;

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
  current_error = prev_error;
  return 0;
}

void __attribute__((weak)) breakpoint_hook();
void breakpoint_hook() {}

void breakpoint() {
  printf(NOTE("BREAKPOINT") " ");
  print_last_log_msg();
  breakpoint_hook();
}
