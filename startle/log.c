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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <stdbool.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/static_alloc.h"

/** @file
 *  @brief Structured in-memory logging
 */


#define FORMAT__ITEM(file, line, name, c)             \
  void format_##name(intptr_t) __attribute__((weak)); \
  void format_##name(intptr_t x) { printf("?0x%lx", x); }
#include "format_list.h"
#undef FORMAT__ITEM

#define REVERSE       0x80
#define START_CONTEXT 0x40
#define END_CONTEXT   '\xff'
#define MASK (~(REVERSE | START_CONTEXT))

// must be a power of two size
STATIC_ALLOC(log_data, intptr_t, 1 << 12);

static unsigned int log_start = 0;
static unsigned int log_end = 0;
static unsigned int log_watch = ~0;
static unsigned int log_watch_to = ~0;
static intptr_t log_watch_fmt = 0;
static bool set_log_watch_fmt = false;
static bool watching = false;
static unsigned int msg_head = 0;

static bool tweak_enabled = false;
static bool set_tweak_fmt = false;
static char *tweak_fmt = NULL;
static unsigned int tweak_trigger = ~0;
static intptr_t tweak_value = 0;

STATIC_ALLOC(hash_tag_set, uintptr_t, 63);

log_context_t *__log_context = NULL;

/** Call this first to initialize the log. */
void log_init() {
  log_data[0] = 0;
  log_start = 0;
  log_end = 0;
  __log_context = NULL;
  log_watch = ~0;
  log_watch_to = ~0;
  log_watch_fmt = 0;
  set_log_watch_fmt = false;
  watching = false;
  tweak_enabled = false;
  tweak_trigger = ~0;
  set_tweak_fmt = false;
  tweak_fmt = NULL;
  msg_head = 0;
  static_zero(hash_tag_set);
}

#define log(x) (log_data[(x) & (log_data_size - 1)])

/** Set a tag to break on.
 * Break when this tag is reached.
 * @param after break on at the same point after hitting the tag.
 */
void set_log_watch(const tag_t tag, bool after) {
  log_watch = log_watch_to = read_tag(tag);
  set_log_watch_fmt = after;
  log_watch_fmt = 0;
  watching = false;
}

/** Set a range to break.
 * @param tag_from first tag to break on.
 * @param tag_to last tag to break on.
 */
void set_log_watch_range(const tag_t tag_from, const char *tag_to) {
  log_watch = read_tag(tag_from);
  log_watch_to = tag_to ? read_tag(tag_to) : ~0;
  log_watch_fmt = 0;
  watching = false;
}

/** Re-initialize the log without clearing it. */
void log_soft_init() {
  if(log_start != log_end) {
    log_add((intptr_t)"\xff\xff"); // reset indentation
  }
  __log_context = NULL;
}

static
uint8_t log_entry_len(unsigned int idx) {
  const char *fmt = (const char *)log(idx);
  if(!fmt) return 0;
  char len = fmt[0];
  if(len == END_CONTEXT) return 0;
  return (uint8_t)(len & MASK);
}

static
unsigned int log_next(unsigned int i) {
  return i + log_entry_len(i) + 1;
}

// not the most efficient
static
char *strchrnul(const char *s, int c) {
  char *res = strchr(s, c);
  return res ? res : strchr(s, 0);
}

static
unsigned int log_printf(unsigned int idx, unsigned int *depth, bool event) {
  unsigned int msg_id = idx;
  const char *fmt = (const char *)log(idx++);
  tag_t tag;
  //printf("%d %d %x %s\n", idx, *depth, fmt[0], fmt + 1);
  uint8_t len = fmt[0] & MASK;
  intptr_t x;
  const char
    *p = fmt + 1,
    *n = strpbrk(p, "%#@");
  LOOP(*depth * 2) putchar(' ');
  if(fmt[0] & START_CONTEXT) (*depth)++;
  while(n) {
    printf("%.*s", (int)(n-p), p); // print the text
    if(!n[1]) break;
    if(n[0] != '%') {
      p = strchrnul(n, ' ');
      uintptr_t key = nonzero_hash(n+1, p-n-1);
      if(n[0] == '@' || set_member(key, hash_tag_set, hash_tag_set_size)) {
        printf(NOTE("%.*s"), (int)(p-n), n);
      } else {
        printf("%.*s", (int)(p-n), n);
      }
    } else {
      switch(n[1]) {
#define CASE_PRINT(c, print)                    \
        case c:                                 \
          if(len) {                             \
            x = log(idx++);                     \
            print;                              \
            len--;                              \
          } else {                              \
            printf("X");                        \
          }                                     \
          break;
#define CASE(c, cast, fmt)                      \
        CASE_PRINT(c, printf(fmt, cast(x)))
#define FORMAT__ITEM(file, line, name, c) CASE_PRINT(c, format_##name(x))
#include "format_list.h"
#undef FORMAT__ITEM
        CASE('d', (intptr_t), "%" PRIdPTR);
        CASE('u', (uintptr_t), "%" PRIuPTR);
        CASE('x', (uintptr_t), "%" PRIxPTR);
        CASE('s', (const char *), "%s");
        CASE('p', (void *), "%p");
#undef CASE
#undef CASE_PRINT
      case '.':
        if(n[2] == '*' && n[3] == 's') {
          if(len > 1) {
            int size = log(idx++);
            printf("%.*s", size, (const char *)log(idx++));
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
    }
    n = strpbrk(p, "%#@");
  }
  if(event) {
    write_tag(tag, msg_id);
    printf("%s " FADE(FORMAT_TAG) "\n", p, tag);
  } else {
    printf("%s\n", p);
  }
  return idx;
}

void log_add(intptr_t x) {
  log(log_end++) = x;
  if(log_end - log_start >= log_data_size) {
    log_start += log_entry_len(log_start) + 1;
  }
}

void log_add_first(intptr_t x) {
  msg_head = log_end;
  log_add(x);
}

bool log_add_last(intptr_t x) {
  log_add(x);
  if unlikely(msg_head == log_watch ||
              (watching &&
               (!set_log_watch_fmt ||
                log(msg_head) == log_watch_fmt))) {
    watching = true;
    if(set_log_watch_fmt) {
      log_watch_fmt = log(msg_head);
    } else if(msg_head == log_watch_to) {
      watching = false;
    }
    return true;
  }
  return false;
}

bool log_add_only(intptr_t x) {
  msg_head = log_end;
  return log_add_last(x);
}

unsigned int log_depth() {
  unsigned int
    i = log_start,
    depth = 0;
  while(i != log_end) {
    const char *fmt = (const char *)log(i);
    if(*fmt == END_CONTEXT) {
      if(fmt[1] == END_CONTEXT) { // log break
        depth = 0;
      } else if(depth) {
        depth--;
      }
      i++;
    } else {
      if(*fmt & START_CONTEXT) {
        depth++;
      }
      uint8_t len = *fmt & MASK;
      i += len + 1;
    }
  }
  return depth;
}

#ifdef DEBUG
static
void print_ints(unsigned int *ar, unsigned int n, unsigned int x) {
  COUNTUP(i, n) {
    if(i == x) {
      printf("[%d] ", ar[i]);
    } else {
      printf(" %d  ", ar[i]);
    }
  }
  printf("\n");
}
#else
#define print_ints(...) ((void)0)
#endif

static
unsigned int count_reversed(unsigned int start) {
  unsigned int
    i = start,
    cnt = 0;
  while(i != log_end) {
    const char *fmt = (const char *)log(i);
    if(!(*fmt & REVERSE)) break;
    i = log_next(i);
    cnt++;
  }
  return cnt;
}

unsigned int log_find_context(unsigned int *ca, unsigned int size) {
  unsigned int
    i = log_start,
    depth = 0,
    current_depth = log_depth(),
    low = current_depth > size ? current_depth - size : 0,
    high = low + size - 1;
  memset(ca, 0, sizeof(ca[0]) * size);
  while(i != log_end) {
    const char *fmt = (const char *)log(i);
    if(*fmt == END_CONTEXT) {
      if(fmt[1] == END_CONTEXT) { // log break
        depth = 0;
      } else if(depth) {
        depth--;
      }
    } else {
      if(*fmt & START_CONTEXT) {
        if(*fmt & REVERSE) {
          unsigned int cnt = count_reversed(i);
          COUNTDOWN(j, cnt) {
            unsigned int depth_r = depth + j;
            if(INRANGE(depth_r, low, high)) {
              unsigned int d = depth_r - low;
              ca[d] = i;
              print_ints(ca, size, d);
            }
            i = log_next(i);
          }
          depth += cnt;
          continue;
        } else {
          if(INRANGE(depth, low, high)) {
            unsigned int d = depth - low;
            ca[d] = i;
            print_ints(ca, size, d);
          }
          depth++;
        }
      }
    }
    i = log_next(i);
  }
  return min(size, current_depth);
}

void print_context(size_t s) {
  unsigned int ca[s];
  unsigned int n = log_find_context(ca, s);
  COUNTUP(i, n) {
    unsigned int depth = i;
#ifdef DEBUG
    printf("%d ", ca[i]);
#endif
    log_printf(ca[i], &depth, false);
  }
}

void print_last_log_msg() {
  unsigned int depth = 0;
  log_printf(msg_head, &depth, true);
}

static
bool end_context(unsigned int idx, unsigned int *depth) {
  const char *fmt = (const char *)log(idx);
  // TODO match the ends so that dropping entries doesn't break indentation
  if(fmt[0] != END_CONTEXT) return false;
  if(fmt[1] == END_CONTEXT) {
    *depth = 0;
    putchar('\n');
  } else if(*depth > 0) {
    (*depth)--;
  }
  return true;
}

static
unsigned int print_contexts(unsigned int idx, unsigned int *depth) {
  if(idx == log_end) return idx;
  const char *fmt = (const char *)log(idx);
  if(fmt[0] == END_CONTEXT ||
     (fmt[0] & REVERSE) == 0) return idx;
  uint8_t len = (fmt[0] & MASK) + 1;
  unsigned int ret = print_contexts(idx + len, depth);
  log_printf(idx, depth, false);
  return ret;
}

/** Print all stored log entries. */
void log_print_all() {
  log_scan_tags();
  unsigned int
    depth = 0,
    i = log_start;
  while(i != log_end) {
    i = print_contexts(i, &depth);
    if(i == log_end) break;
    if(end_context(i, &depth)) {
      i++;
    } else {
      i = log_printf(i, &depth, true);
    }
  }
}

void log_scan_tags() {
  static_zero(hash_tag_set);
  unsigned int i = log_start;
  while(i != log_end) {
    const char *fmt = (const char *)log(i);
    if(*fmt == END_CONTEXT) {
      i++;
      continue;
    }
    uint8_t len = *fmt & MASK;
    i += len + 1;
    const char *p = fmt;
    while((p = strchr(p, '@'))) {
      p++;
      const char *e = strchrnul(p, ' ');
      if(p == e) continue;
      uintptr_t key = nonzero_hash(p, e-p);
      set_insert(key, hash_tag_set, hash_tag_set_size);
      p = e;
    }
  }
}

#if INTERFACE
#define LOG_pre                                 \
  do {                                          \
    log_add_context();
#define LOG_first(s, fmt) log_add_first((intptr_t)(s fmt));
#define LOG_middle(x) log_add((intptr_t)(x));
#define LOG_last(x) if(log_add_last((intptr_t)(x))) breakpoint();
#define LOG_only(s, fmt) if(log_add_only((intptr_t)(s fmt))) breakpoint();
#define LOG_post                                \
  } while(0)
#define LOG_args ("\x00", "\x01", "\x02", "\x03", "\x04", "\x05", "\x06", "\x07", "\x08")
#define LOG_NO_POS(...) FORARG(LOG, __VA_ARGS__)

/** Log the format string and arguments.
 * @snippet log.c log
 */
#define LOG(fmt, ...) LOG_NO_POS(__FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)

/** Log when `test` is true. */
#define LOG_WHEN(test, fmt, ...) ((test) && (({ LOG(fmt, ##__VA_ARGS__); }), true))

/** Log unless `test` is true. */
#define LOG_UNLESS(test, fmt, ...) ((test) || (({ LOG(fmt, ##__VA_ARGS__); }), false))

// same as LOG, but don't call log_add_{last, only} to avoid calling breakpoint()
#define LOG_NOBREAK_pre                         \
  do {                                          \
    log_add_context();
#define LOG_NOBREAK_first(s, fmt) log_add_first((intptr_t)(s fmt));
#define LOG_NOBREAK_middle(x) log_add((intptr_t)(x));
#define LOG_NOBREAK_last(x) log_add_last((intptr_t)(x));
#define LOG_NOBREAK_only(s, fmt) log_add_only((intptr_t)(s fmt));
#define LOG_NOBREAK_post                        \
  } while(0)
#define LOG_NOBREAK_args LOG_args
#define LOG_NOBREAK(fmt, ...) FORARG(LOG_NOBREAK, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)

#endif

TEST(log) {
  /** [log] */
  log_init();
  LOG("test %d + %d = %d", 1, 2, 3);
  LOG("WAZZUP %s", "d00d");
  LOG("[%.*s]", 3, "12345");
  log_print_all();
  /** [log] */
  return 0;
}

#if INTERFACE
typedef struct log_context log_context_t;
struct log_context {
  struct log_context *next;
  char const *fmt;
  intptr_t arg[0];
};

#define CONTEXT_pre                             \
  __attribute__((cleanup(log_cleanup_context))) \
  intptr_t __context[] = {                      \
    (intptr_t)__log_context,
#define CONTEXT_first(s, fmt) (intptr_t)(s fmt) + 1,
#define CONTEXT_middle(x) (intptr_t)(x),
#define CONTEXT_last(x) (intptr_t)(x)};
#define CONTEXT_only(s, fmt) (intptr_t)(s fmt) + 1};
#define CONTEXT_post                            \
  __log_context = (log_context_t *)__context;
#define CONTEXT_args ("\xff\xc0", "\xff\xc1", "\xff\xc2", "\xff\xc3", "\xff\xc4", "\xff\xc5", "\xff\xc6", "\xff\xc7", "\xff\xc8")

/** Store log context.
 * Context is logged if a message is logged within the context's scope.
 * This allows for more non-local information to be logged about an event.
 * @snippet log.c context
 */
#define CONTEXT(fmt, ...) FORARG(CONTEXT, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

#if INTERFACE
#define CONTEXT_LOG_pre                                                 \
  const char *__context __attribute__((cleanup(log_cleanup_context_log))); \
  do {                                                                  \
    log_add_context();
#define CONTEXT_LOG_first(s, fmt)               \
  __context = s fmt;                            \
  log_add_first((intptr_t)(__context + 1));
#define CONTEXT_LOG_middle(x)                   \
  log_add((intptr_t)(x));
#define CONTEXT_LOG_last(x)                     \
  if(log_add_last((intptr_t)(x))) breakpoint();
#define CONTEXT_LOG_only(s, fmt)                                \
  __context = s fmt;                                            \
  if(log_add_only((intptr_t)(__context + 1))) breakpoint();
#define CONTEXT_LOG_post                        \
  } while(0)

/** Store context to log.
 * Like CONTEXT, but always logged.
 */
#define CONTEXT_LOG_args ("\xff\x40", "\xff\x41", "\xff\x42", "\xff\x43", "\xff\x44", "\xff\x45", "\xff\x46", "\xff\x47", "\xff\x48")
#define CONTEXT_LOG(fmt, ...) FORARG(CONTEXT_LOG, __FILE__ ":" STRINGIFY(__LINE__) ": " fmt, ##__VA_ARGS__)
#endif

void log_cleanup_context(void *p) {
  log_context_t *ctx = p;
  if(ctx->fmt[0] == END_CONTEXT) {
    // add end marker
    log_add((intptr_t)ctx->fmt);
  }
  __log_context = ctx->next;
}

void log_cleanup_context_log(const char **fmt) {
  log_add((intptr_t)*fmt);
}

void log_add_context() {
  log_context_t *p = __log_context;
  while(p &&
        p->fmt[0] != END_CONTEXT) {
    log_add((intptr_t)p->fmt);
    uint8_t len = p->fmt[0] & MASK;
    COUNTUP(i, len) {
      log_add(p->arg[i]);
    }
    p->fmt--;
    p = p->next;
  }
}

/** [log_context] */
static
void __test_context_c(int x) {
  CONTEXT_LOG("C %d", x);
}

static
void __test_context_b(int x) {
  CONTEXT("B %d", x);
  LOG_WHEN(x == 0, "(b) zero x");
  if(x == 0) {
    LOG("printing context");
    printf("__ print_context() __\n");
    print_context(4);
    printf("__ end print_context() __\n");
  }
}

static
void __test_context_a(int x) {
  CONTEXT("A %d", x);
  __test_context_b(x - 1);
  LOG_WHEN(x > 0, "(a) nonzero x");
}

static
void __test_context_e(int x) {
  CONTEXT_LOG("E %d", x);
  LOG("printing context");
  printf("__ print_context() __\n");
  print_context(4);
  printf("__ end print_context() __\n");
}

static
void __test_context_d(int x) {
  CONTEXT_LOG("D %d", x);
  __test_context_e(x);
  LOG("exiting d");
}

TEST(log_context) {
  log_init();
  __test_context_a(2);
  __test_context_a(1);
  __test_context_a(0);
  __test_context_c(3);
  __test_context_d(42);
  log_print_all();
  return 0;
}
/** [log_context] */

#if INTERFACE
typedef char tag_t[5];
#define FORMAT_TAG "%.5s"
#endif

char to_tag_char(int x) {
  x &= 31;
  if(x < 24) {
    return 'a' + x;
  } else {
    return '2' + x - 24;
  }
}

int from_tag_char(char c) {
  if(c >= 'a') {
    if(c <= 'x') {
      return c - 'a';
    } else {
      return -1;
    }
  } else if(c >= '0') {
    return c - '2' + 24;
  } else {
    return -1;
  }
}

int spread_bits(int x) {
  int y = 0;
  COUNTUP(i, 4) {
    int t = 0;
    COUNTUP(j, 5) {
      t <<= 4;
      t |= x & 1;
      x >>= 1;
    }
    t <<= i;
    y |= t;
  }
  return y;
}

int gather_bits(int y) {
  int x = 0;
  COUNTDOWN(i, 4) {
    int t = y >> i;
    COUNTUP(j, 5) {
      x <<= 1;
      x |= t & 1;
      t >>= 4;
    }
  }
  return x;
}

TEST(spread_gather_bits) {
  int x = 0x9AC35;
  int spread = spread_bits(x);
  int gather = gather_bits(spread);
  return x == gather ? 0 : -1;
}

// modular multiplicative inverses
// 3 + (5 << 5) + (7 << 10) + (11 << 15) + (13 << 20)
const unsigned int tag_factor = 13999267;
const unsigned int tag_factor_inverse = 10221323;
#define TAG_BITS (sizeof(tag_t) * 5 - 1)
const unsigned int tag_mask = (1 << TAG_BITS) - 1;

/** Write log tag for the given value.
 * @snippet log.c tag
 */
void write_tag(tag_t tag, unsigned int val) {
  val += 1;
  val *= tag_factor;
  val &= tag_mask;
  COUNTDOWN(i, sizeof(tag_t)) {
    tag[i] = to_tag_char(val);
    val >>= 5;
  }
}

/** Return value from the given log tag.
 * @snippet log.c tag
 */
int read_tag(const tag_t tag) {
  unsigned int val = 0;
  COUNTUP(i, sizeof(tag_t)) {
    int x = from_tag_char(tag[i]);
    if(x < 0) return x;
    val = (val << 5) | x;
  }
  val *= tag_factor_inverse;
  val &= tag_mask;
  return val - 1;
}

TEST(tag) {
  /** [tag] */
  tag_t tag = "great";
  int x = read_tag(tag);
  write_tag(tag, x);
  printf("tag: %d = " FORMAT_TAG "\n", x, tag);
  return strncmp("great", tag, sizeof(tag)) == 0 ? 0 : -1;
  /** [tag] */
}

/** Get the most recent log tag. */
void get_tag(tag_t tag) {
  write_tag(tag, msg_head);
}

#if INTERFACE
/** Tweaks are values that can be changed at a specific point.
 * Tweaks allow modifying something at a specific time, identified
 * by looking up the log tag in the log.
 * @param default_value value returned when not "tweaked."
 * Remaining arguments are logged.
 */
#define TWEAK(default_value, fmt, ...)                          \
  ({                                                            \
    const char *__c;                                            \
    intptr_t __x;                                               \
    bool __t;                                                   \
    if unlikely(__t = log_do_tweak(&__x, fmt)) {                \
      __c = COLOR_blue;                                         \
    } else {                                                    \
      __x = (default_value);                                    \
      __c = COLOR_normal;                                       \
    }                                                           \
    LOG(COLORs("TWEAK(%d)") " " fmt, __c, __x, ##__VA_ARGS__);  \
    if(__t) breakpoint();                                       \
    __x;                                                        \
  })
#endif

bool log_do_tweak(intptr_t *x, char *fmt) {
  log_add_context();
  if unlikely(tweak_enabled &&
              (log_end == tweak_trigger ||
               (set_tweak_fmt && tweak_fmt == fmt))) {
    *x = tweak_value;
    if(set_tweak_fmt && !tweak_fmt) tweak_fmt = fmt;
    return true;
  } else {
    return false;
  }
}

/** Tweak to the value at the given log tag.
 * Only one tweak can be set at a time.
 */
void log_set_tweak(const tag_t tag, intptr_t value, bool after) {
  tweak_enabled = true;
  tweak_trigger = read_tag(tag);
  tweak_value = value;
  set_tweak_fmt = after;
}

/** Clear the tweak. */
void log_unset_tweak() {
  tweak_enabled = false;
}
