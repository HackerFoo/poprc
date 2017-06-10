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

#include "rt_types.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef BACKTRACE
#include <execinfo.h>
#endif

#include "gen/error.h"

#if INTERFACE
#include <assert.h>
#include <setjmp.h>
#endif

#if INTERFACE
typedef struct {
  jmp_buf env;
  const char *msg;
  const char *file;
  int line;
  char function[64];
} error_t;

#define assert_throw(...) DISPATCH(assert_throw, 2, ##__VA_ARGS__)

#ifdef EMSCRIPTEN
#define assert_error(x) assert_throw(x)
#else
#define assert_error(x) assert(x)
#endif

#define assert_throw_0(cond, msg, ...)                                  \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed: " msg); \
    }                                                                   \
  } while(0)

#define assert_throw_1(cond, ...)                                       \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed."); \
    }                                                                   \
  } while(0)

#define catch_error(e) (current_error = (e), !!setjmp((e)->env))
#endif

error_t *current_error = NULL;

#ifdef BACKTRACE
static void *backtrace_buf[128];
#endif

static int backtrace_size = 0;

void throw_error(const char *file, int line, const char *function, const char *msg) {
  if(!current_error) {
    printf("%s:%d: %s: %s\n", file, line, function, msg);
    assert_error(false);
  } else {
#ifdef BACKTRACE_SIZE
    backtrace_size = backtrace(backtrace_buf, LENGTH(backtrace_buf));
#endif
    current_error->msg = msg;
    current_error->file = file;
    current_error->line = line;
    strncpy(current_error->function, function, sizeof(current_error->function));
    longjmp(current_error->env, 1);
  }
}

bool have_backtrace() {
  return backtrace_size != 0;
}

void clear_backtrace() {
  backtrace_size = 0;
}

void print_backtrace() {
#ifdef BACKTRACE
  if(backtrace_size) {
    backtrace_symbols_fd(backtrace_buf, backtrace_size, STDOUT_FILENO);
  }
#endif
}

void print_error(error_t *error) {
    printf("%s:%d: %s: %s\n", error->file, error->line, error->function, error->msg);
}

int test_error() {
  error_t *prev_error = current_error;
  error_t test_error;
  if(catch_error(&test_error)) {
    print_error(&test_error);
  } else {
    COUNTUP(i, 5) {
      assert_throw(i < 3, "Don't worry, it's okay.");
      printf("i = %d\n", (int)i);
    }
  }
  current_error = prev_error;
  return 0;
}
