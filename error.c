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
#include <inttypes.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef BACKTRACE
#ifdef EMSCRIPTEN
#include <emscripten.h>
#else
#include <execinfo.h>
#endif
#endif

#include "gen/error.h"
#ifndef NOLOG
#include "gen/log.h"
#endif

#if INTERFACE
#include <setjmp.h>

typedef enum error_type_e {
  ERROR_TYPE_NONE = 0,
  ERROR_TYPE_UNEXPECTED = 1,
  ERROR_TYPE_LIMITATION = 2
} error_type_t;

typedef struct {
  jmp_buf env;
  const char *msg;
  const char *file;
  int line;
  error_type_t type;
  char function[64];
} error_t;

#define assert_throw(...) _assert_throw(__VA_ARGS__)

#define _assert_throw(...) DISPATCH(assert_throw, 2, ##__VA_ARGS__)
#define assert_throw_0(cond, msg, ...)                                  \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed: " msg, ERROR_TYPE_LIMITATION); \
    }                                                                   \
  } while(0)

#define assert_throw_1(cond, ...)                                       \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed.", ERROR_TYPE_LIMITATION); \
    }                                                                   \
  } while(0)

#ifdef NDEBUG
#define assert_error(...) ((void)0)
#else
#define assert_error(...) _assert_error(__VA_ARGS__)
#endif

#define _assert_error(...) DISPATCH(assert_error, 2, ##__VA_ARGS__)
#define assert_error_0(cond, msg, ...)                                  \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed: " msg, ERROR_TYPE_UNEXPECTED); \
    }                                                                   \
  } while(0)

#define assert_error_1(cond, ...)                                       \
  do {                                                                  \
    if(!(cond)) {                                                       \
      throw_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed.", ERROR_TYPE_UNEXPECTED); \
    }                                                                   \
  } while(0)


#define assert_log(cond, fmt, ...) \
  do {                                                                  \
    if(!(cond)) {                                                       \
      log_error(__FILE__, __LINE__, __func__, "Assertion `" #cond "' failed.", ERROR_TYPE_UNEXPECTED); \
      LOG_NO_POS("!!! " fmt, ##__VA_ARGS__);                            \
      return_error(ERROR_TYPE_UNEXPECTED);                              \
    }                                                                   \
  } while(0)

#define assert_counter(n)                                               \
  do {                                                                  \
    static int counter = n;                                             \
    if(!counter--) {                                                    \
      counter = n;                                                      \
      throw_error(__FILE__, __LINE__, __func__, "Assertion counter exhausted.", ERROR_TYPE_UNEXPECTED); \
    }                                                                   \
  } while(0)

#define catch_error(e) (current_error = (e), !!setjmp((e)->env))
#endif

error_t *current_error = NULL;

#ifdef BACKTRACE
#ifdef EMSCRIPTEN
static char backtrace_buf[4096];
#else
static void *backtrace_buf[128];
#endif
#endif

static int backtrace_size = 0;

void log_error(const char *file, int line, const char *function, const char *msg, error_type_t type) {
  if(!current_error) {
    printf("%s:%d: %s: %s\n", file, line, function, msg);
  } else {
#ifdef BACKTRACE
#ifdef EMSCRIPTEN
    backtrace_size = emscripten_get_callstack(EM_LOG_NO_PATHS | EM_LOG_FUNC_PARAMS,
                                              backtrace_buf, sizeof(backtrace_buf));
#else
    backtrace_size = backtrace(backtrace_buf, LENGTH(backtrace_buf));
#endif
#endif
    current_error->msg = msg;
    current_error->file = file;
    current_error->line = line;
    current_error->type = type;
#ifndef NOLOG
    LOG_NO_POS("!!! %s:%d: %s: %s", file, line, function, msg);
#endif
    strncpy(current_error->function, function, sizeof(current_error->function));
  }
}

void return_error(error_type_t type) {
  if(current_error) {
    longjmp(current_error->env, type);
  } else {
    exit(-1);
  }
}

void throw_error(const char *file, int line, const char *function, const char *msg, error_type_t type) {
  log_error(file, line, function, msg, type);
  return_error(type);
}

void print_backtrace() {
#ifdef BACKTRACE
  if(backtrace_size) {
#ifdef EMSCRIPTEN
    puts(backtrace_buf);
#else
    backtrace_symbols_fd(backtrace_buf, backtrace_size, STDOUT_FILENO);
#endif
  }
#else
  printf("backtrace not supported\n");
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
      _assert_throw(i < 3, "Don't worry, it's okay.");
      printf("i = %d\n", (int)i);
    }
  }
  current_error = prev_error;
  return 0;
}

// print a backtrace
void command_bt(UNUSED cell_t *rest) {
  print_backtrace();
}
