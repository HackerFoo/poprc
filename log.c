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
#include <assert.h>
#include <inttypes.h>

#include "gen/log.h"

#define LOG_SIZE 4096
static intptr_t log[LOG_SIZE];
static unsigned int log_head = 0;
static unsigned int log_tail = 0;

void log_init() {
  log[0] = 0;
  log_head = 0;
}

int log_entry_len(unsigned int idx) {
  const char *fmt = (const char *)log[idx];
  return fmt ? fmt[0] : 1;
}

void log_add(intptr_t x) {
  log[log_head] = x;
  log_head = (log_head + 1) % LOG_SIZE;
  if(log_head == log_tail) {
    log_tail = (log_tail + log_entry_len(log_tail)) % LOG_SIZE;
  }
}

unsigned int log_printf(unsigned int idx) {
  const char *fmt = (const char *)log[idx++];
  uint8_t len = fmt[0];
  const char
    *p = fmt + 1,
    *n = strchr(p, '%');
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
  printf("%s", p);
  return idx % LOG_SIZE;
}

void log_print_all() {
  for(unsigned int i = log_tail;
      i != log_head;
      i = log_printf(i));
}

#if INTERFACE
#define LOG_0(fmt, x0, x1, x2, x3, x4, x5, x6, x7, ...) \
  do {                                                  \
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
    log_add((intptr_t)("\x05" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
    log_add((intptr_t)(x4));                    \
  } while(0)
#define LOG_4(fmt, x0, x1, x2, x3, ...)         \
  do {                                          \
    log_add((intptr_t)("\x04" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
    log_add((intptr_t)(x3));                    \
  } while(0)
#define LOG_5(fmt, x0, x1, x2, ...)             \
  do {                                          \
    log_add((intptr_t)("\x03" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
    log_add((intptr_t)(x2));                    \
  } while(0)
#define LOG_6(fmt, x0, x1, ...)                 \
  do {                                          \
    log_add((intptr_t)("\x02" fmt));            \
    log_add((intptr_t)(x0));                    \
    log_add((intptr_t)(x1));                    \
  } while(0)
#define LOG_7(fmt, x0, ...)                     \
  do {                                          \
    log_add((intptr_t)("\x01" fmt));            \
    log_add((intptr_t)(x0));                    \
  } while(0)
#define LOG_8(fmt, ...)                         \
  do {                                          \
    log_add((intptr_t)("\x00" fmt));            \
  } while(0)
#define LOG(fmt, ...) DISPATCH(LOG, 9, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

int test_log() {
  log_init();
  LOG("test %d + %d = %d\n", 1, 2, 3);
  LOG("WAZZUP %s\n", "d00d");
  LOG("[%.*s]\n", 3, "12345");
  log_print_all();
  return 0;
}

void command_logs(UNUSED cell_t *rest) {
  log_print_all();
}
