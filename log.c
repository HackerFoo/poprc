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
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

#include "gen/error.h"
#include "gen/log.h"

#define REVERSE 0x80
#define INDENT  0x40
#define MASK (REVERSE | INDENT)

#define LOG_SIZE 4096
static intptr_t log[LOG_SIZE];
static unsigned int log_head = 0;
static unsigned int log_tail = 0;

context_t *__log_context = NULL;

void log_init() {
  log[0] = 0;
  log_head = 0;
  log_tail = 0;
  __log_context = NULL;
}

void log_soft_init() {
  if(log_head != log_tail) {
    log_add((intptr_t)"\xff\xff"); // reset indentation
  }
  __log_context = NULL;
}

static
int log_entry_len(unsigned int idx) {
  const char *fmt = (const char *)log[idx];
  if(!fmt) return 1;
  char len = fmt[0];
  if(len == '\xff') return 1;
  return len & ~MASK;
}

void log_add(intptr_t x) {
  log[log_head] = x;
  log_head = (log_head + 1) % LOG_SIZE;
  if(log_head == log_tail) {
    log_tail = (log_tail + log_entry_len(log_tail)) % LOG_SIZE;
  }
}

static
unsigned int log_printf(unsigned int idx, unsigned int *depth) {
  const char *fmt = (const char *)log[idx++];
  //printf("%d %d %x %s\n", idx, *depth, fmt[0], fmt + 1);
  uint8_t len = fmt[0] & ~MASK;
  const char
    *p = fmt + 1,
    *n = strchr(p, '%');
  LOOP(*depth * 2) putchar(' ');
  if(fmt[0] & INDENT) (*depth)++;
  while(n) {
    printf("%.*s", (int)(n-p), p); // print the text
    if(!n[1]) break;
    switch(n[1]) {
#define CASE(c, type, fmt)                                      \
      case c:                                                   \
        if(len) {                                               \
          idx = idx % LOG_SIZE;                                 \
          printf(fmt, (type)log[idx++]);                        \
          len--;                                                \
        } else {                                                \
          printf("X");                                          \
        }                                                       \
        break;
      CASE('d', int, "%d");
      CASE('u', unsigned int, "%u");
      CASE('x', int, "%x");
      CASE('s', const char *, "%s");
 #undef CASE
    case '.':
      if(n[2] == '*' && n[3] == 's') {
        if(len > 1) {
          idx = idx % LOG_SIZE;
          int size = log[idx++];
          idx = idx % LOG_SIZE;
          printf("%.*s", size, (const char *)log[idx++]);
          len -= 2;
        } else {
          printf("X");
        }
        n += 2;
        break;
      }
    case '%':
      printf("%%");
      break;
    default:
      printf("!?");
      break;
    }
    p = n + 2;
    n = strchr(p, '%');
  }
  printf("%s\n", p);
  return idx % LOG_SIZE;
}

static
bool end_context(unsigned int idx, unsigned int *depth) {
  const char *fmt = (const char *)log[idx];
  // TODO match the ends so that dropping entries doesn't break indentation
  if(fmt[0] != '\xff') return false;
  if(fmt[1] == '\xff') {
    *depth = 0;
  } else if(*depth > 0) {
    (*depth)--;
  }
  return true;
}

static
unsigned int print_contexts(unsigned int idx, unsigned int *depth) {
  if(idx == log_head) return idx;
  const char *fmt = (const char *)log[idx];
  if(fmt[0] == '\xff' ||
     (fmt[0] & REVERSE) == 0) return idx;
  uint8_t len = (fmt[0] & ~MASK) + 1;
  unsigned int ret = print_contexts((idx + len) % LOG_SIZE, depth);
  log_printf(idx, depth);
  return ret;
}

void log_print_all() {
  unsigned int
    depth = 0,
    i = log_tail;
  while(i != log_head) {
    i = print_contexts(i, &depth);
    if(i == log_head) break;
    if(end_context(i, &depth)) {
      i = (i + 1) % LOG_SIZE;
    } else {
      i = log_printf(i, &depth);
    }
  }
}

#if INTERFACE
#define LOG_0(fmt, x0, x1, x2, x3, x4, x5, x6, x7, ...) \
  do {                                                  \
    log_add_context();                                  \
    log_add((intptr_t)("\x08" fmt));                    \
    log_add((intptr_t)(x0));                            \
    log_add((intptr_t)(x1));                            \
    log_add((intptr_t)(x2));                            \
    log_add((intptr_t)(x3));                            \
    log_add((intptr_t)(x4));                            \
    log_add((intptr_t)(x5));                            \
    log_add((intptr_t)(x6));                            \
    log_add((intptr_t)(x7));                            \
  } while(0)
#define LOG_1(fmt, x0, x1, x2, x3, x4, x5, x6, ...)     \
  do {                                                  \
    log_add_context();                                  \
    log_add((intptr_t)("\x07" fmt));                    \
    log_add((intptr_t)(x0));                            \
    log_add((intptr_t)(x1));                            \
    log_add((intptr_t)(x2));                            \
    log_add((intptr_t)(x3));                            \
    log_add((intptr_t)(x4));                            \
    log_add((intptr_t)(x5));                            \
    log_add((intptr_t)(x6));                            \
  } while(0)
#define LOG_2(fmt, x0, x1, x2, x3, x4, x5, ...) \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x06" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
    log_add((intptr_t)(x4));                    \
    log_add((intptr_t)(x5));                    \
  } while(0)
#define LOG_3(fmt, x0, x1, x2, x3, x4, ...)     \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x05" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
    log_add((intptr_t)(x4));                    \
  } while(0)
#define LOG_4(fmt, x0, x1, x2, x3, ...)         \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x04" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
  } while(0)
#define LOG_5(fmt, x0, x1, x2, ...)             \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x03" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
  } while(0)
#define LOG_6(fmt, x0, x1, ...)                 \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x02" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
  } while(0)
#define LOG_7(fmt, x0, ...)                     \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x01" fmt));            \
    log_add((intptr_t)(x0));                    \
  } while(0)
#define LOG_8(fmt, ...)                         \
  do {                                          \
    log_add_context();                          \
    log_add((intptr_t)("\x00" fmt));            \
  } while(0)
#define LOG(fmt, ...) DISPATCH(LOG, 9, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

int test_log() {
  log_init();
  LOG("test %d + %d = %d", 1, 2, 3);
  LOG("WAZZUP %s", "d00d");
  LOG("[%.*s]", 3, "12345");
  log_print_all();
  return 0;
}

void command_log(UNUSED cell_t *rest) {
  log_print_all();
}

#if INTERFACE
typedef struct context_s context_t;
struct context_s {
  struct context_s *next;
  char const *fmt;
  intptr_t arg[0];
};

#define END_OF_CONTEXT_MACRO                    \
  __log_context = (context_t *)__context;
#define CONTEXT_0(fmt, x0, x1, x2, x3, x4, x5, x6, x7, ...)     \
  intptr_t __context[] = {                                      \
    (intptr_t)__log_context,                                    \
    (intptr_t)("\xff\xC8" fmt) + 1,                             \
    (intptr_t)x0,                                               \
    (intptr_t)x1,                                               \
    (intptr_t)x2,                                               \
    (intptr_t)x3,                                               \
    (intptr_t)x4,                                               \
    (intptr_t)x5,                                               \
    (intptr_t)x6,                                               \
    (intptr_t)x7};                                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_1(fmt, x0, x1, x2, x3, x4, x5, x6, ...) \
  intptr_t __context[] = {                              \
    (intptr_t)__log_context,                            \
    (intptr_t)("\xff\xC7" fmt) + 1,                     \
    (intptr_t)x0,                                       \
    (intptr_t)x1,                                       \
    (intptr_t)x2,                                       \
    (intptr_t)x3,                                       \
    (intptr_t)x4,                                       \
    (intptr_t)x5,                                       \
    (intptr_t)x6};                                      \
  END_OF_CONTEXT_MACRO
#define CONTEXT_2(fmt, x0, x1, x2, x3, x4, x5, ...)     \
  intptr_t __context[] = {                              \
    (intptr_t)__log_context,                            \
      (intptr_t)("\xff\xC6" fmt) + 1,                   \
      (intptr_t)x0,                                     \
      (intptr_t)x1,                                     \
      (intptr_t)x2,                                     \
      (intptr_t)x3,                                     \
      (intptr_t)x4,                                     \
      (intptr_t)x5};                                    \
  END_OF_CONTEXT_MACRO
#define CONTEXT_3(fmt, x0, x1, x2, x3, x4, ...) \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC5" fmt) + 1,             \
    (intptr_t)x0,                               \
    (intptr_t)x1,                               \
    (intptr_t)x2,                               \
    (intptr_t)x3,                               \
    (intptr_t)x4};                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_4(fmt, x0, x1, x2, x3, ...)     \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC4" fmt) + 1,             \
    (intptr_t)x0,                               \
    (intptr_t)x1,                               \
    (intptr_t)x2,                               \
    (intptr_t)x3};                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_5(fmt, x0, x1, x2, ...)         \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC3" fmt) + 1,             \
    (intptr_t)x0,                               \
    (intptr_t)x1,                               \
    (intptr_t)x2};                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_6(fmt, x0, x1, ...)             \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC2" fmt) + 1,             \
    (intptr_t)x0,                               \
    (intptr_t)x1};                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_7(fmt, x0, ...)                 \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC1" fmt) + 1,             \
    (intptr_t)x0};                              \
  END_OF_CONTEXT_MACRO
#define CONTEXT_8(fmt, ...)                     \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,                    \
    (intptr_t)("\xff\xC0" fmt) + 1};            \
  END_OF_CONTEXT_MACRO
#define CONTEXT(fmt, ...) __attribute__((cleanup(log_cleanup_context))) DISPATCH(CONTEXT, 9, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

#if INTERFACE
#define CONTEXT_LOG_0(fmt, x0, x1, x2, x3, x4, x5, x6, x7, ...) \
  do {                                                          \
    log_add_context();                                          \
    __context = "\xff\x48" fmt;                                 \
    log_add((intptr_t)(__context + 1));                         \
    log_add((intptr_t)(x0));                                    \
    log_add((intptr_t)(x1));                                    \
    log_add((intptr_t)(x2));                                    \
    log_add((intptr_t)(x3));                                    \
    log_add((intptr_t)(x4));                                    \
    log_add((intptr_t)(x5));                                    \
    log_add((intptr_t)(x6));                                    \
    log_add((intptr_t)(x7));                                    \
  } while(0)
#define CONTEXT_LOG_1(fmt, x0, x1, x2, x3, x4, x5, x6, ...)     \
  do {                                                          \
    log_add_context();                                          \
    __context = "\xff\x47" fmt;                                 \
    log_add((intptr_t)(__context + 1));                         \
    log_add((intptr_t)(x0));                                    \
    log_add((intptr_t)(x1));                                    \
    log_add((intptr_t)(x2));                                    \
    log_add((intptr_t)(x3));                                    \
    log_add((intptr_t)(x4));                                    \
    log_add((intptr_t)(x5));                                    \
    log_add((intptr_t)(x6));                                    \
  } while(0)
#define CONTEXT_LOG_2(fmt, x0, x1, x2, x3, x4, x5, ...) \
  do {                                                  \
    log_add_context();                                  \
    __context = "\xff\x46" fmt;                         \
    log_add((intptr_t)(__context + 1));                 \
    log_add((intptr_t)(x0));                            \
    log_add((intptr_t)(x1));                            \
    log_add((intptr_t)(x2));                            \
    log_add((intptr_t)(x3));                            \
    log_add((intptr_t)(x4));                            \
    log_add((intptr_t)(x5));                            \
  } while(0)
#define CONTEXT_LOG_3(fmt, x0, x1, x2, x3, x4, ...)     \
  do {                                                  \
    log_add_context();                                  \
    __context = "\xff\x45" fmt;                         \
    log_add((intptr_t)(__context + 1));                 \
    log_add((intptr_t)(x0));                            \
    log_add((intptr_t)(x1));                            \
    log_add((intptr_t)(x2));                            \
    log_add((intptr_t)(x3));                            \
    log_add((intptr_t)(x4));                            \
  } while(0)
#define CONTEXT_LOG_4(fmt, x0, x1, x2, x3, ...) \
  do {                                          \
    log_add_context();                          \
    __context = "\xff\x44" fmt;                 \
    log_add((intptr_t)(__context + 1));         \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
  } while(0)
#define CONTEXT_LOG_5(fmt, x0, x1, x2, ...)     \
  do {                                          \
    log_add_context();                          \
    __context = "\xff\x43" fmt;                 \
    log_add((intptr_t)(__context + 1));         \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
  } while(0)
#define CONTEXT_LOG_6(fmt, x0, x1, ...)         \
  do {                                          \
    log_add_context();                          \
    __context = "\xff\x42" fmt;                 \
    log_add((intptr_t)(__context + 1));         \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
  } while(0)
#define CONTEXT_LOG_7(fmt, x0, ...)             \
  do {                                          \
    log_add_context();                          \
    __context = "\xff\x41" fmt;                 \
    log_add((intptr_t)(__context + 1));         \
    log_add((intptr_t)(x0));                    \
  } while(0)
#define CONTEXT_LOG_8(fmt, ...)                 \
  do {                                          \
    log_add_context();                          \
    __context = "\xff\x40" fmt;                 \
    log_add((intptr_t)(__context + 1));         \
  } while(0)
#define CONTEXT_LOG(fmt, ...)                                           \
  const char *__context __attribute__((cleanup(log_cleanup_context_log))); \
  DISPATCH(CONTEXT_LOG, 9, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

void log_cleanup_context(void *p) {
  context_t *ctx = p;
  if(ctx->fmt[0] == '\xff') {
    // add end marker
    log_add((intptr_t)ctx->fmt);
  }
  __log_context = ctx->next;
}

void log_cleanup_context_log(const char **fmt) {
  log_add((intptr_t)*fmt);
}

void log_add_context() {
  context_t *p = __log_context;
  while(p &&
        p->fmt[0] != '\xff') {
    log_add((intptr_t)p->fmt);
    uint8_t len = p->fmt[0] & ~MASK;
    COUNTUP(i, len) {
      log_add(p->arg[i]);
    }
    p->fmt--;
    p = p->next;
  }
}

static
void __test_context_c(int x) {
  CONTEXT_LOG("C %d", x);
}

static
void __test_context_b(int x) {
  CONTEXT("B %d", x);
  if(x == 0) LOG("(b) zero x");
}

static
void __test_context_a(int x) {
  CONTEXT("A %d", x);
  __test_context_b(x - 1);
  if(x > 0) LOG("(a) nonzero x");
}

static
void __test_context_e(int x) {
  CONTEXT_LOG("E %d", x);
}

static
void __test_context_d(int x) {
  CONTEXT_LOG("D %d", x);
  __test_context_e(x);
  LOG("exiting d");
}

int test_context() {
  log_init();
  __test_context_a(2);
  __test_context_a(1);
  __test_context_a(0);
  __test_context_c(3);
  __test_context_d(42);
  log_print_all();
  return 0;
}

