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
#include <inttypes.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdbool.h>

#if INTERFACE
#include <stdio.h>
#endif

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/stats_types.h"
#include "startle/stats.h"

/** @file
 *  @brief Generally useful functions
 */

#if INTERFACE
#define RANGE_ALL_INIT { .min = INTPTR_MIN, .max = INTPTR_MAX }
#define RANGE_ALL ((range_t)RANGE_ALL_INIT)
#define RANGE_NONE_INIT { .min = INTPTR_MAX, .max = INTPTR_MIN }
#define RANGE_NONE ((range_t)RANGE_NONE_INIT)
#define RANGE_POS_INIT { .min = 0, .max = INTPTR_MAX }
#define RANGE_POS ((range_t)RANGE_POS_INIT)
#endif

#define KEY(ptr) (*(uintptr_t *)(ptr))
#define INDEX(ptr, width, i) ((void *)((char *)(ptr) + (width) * (i)))

/** Estimate the median of an array */
unsigned int median3(void *array, size_t width, size_t lo, size_t hi) {
  size_t mid = lo + (hi - lo) / 2;
  uintptr_t
    a = KEY(INDEX(array, width, lo)),
    b = KEY(INDEX(array, width, mid)),
    c = KEY(INDEX(array, width, hi));
  if(a < b) {
    if(a < c) {
      if (b < c) {
        return mid;
      } else { // b >= c
        return hi;
      }
    } else { // a >= c
      return lo;
    }
  } else { // a >= b
    if(a < c) {
      return lo;
    } else { // a >= c
      if(b < c) {
        return hi;
      } else {
        return mid;
      }
    }
  }
}

/** Swap two pairs. */
void swap(pair_t *x, pair_t *y) {
  pair_t tmp = *x;
  *x = *y;
  *y = tmp;
}

/** Print the pairs in an array. */
void print_pairs(pair_t *array, size_t len) {
  if(!len) {
    printf("{}\n");
  } else {
    printf("{{%" PRIuPTR ", %" PRIuPTR "}", array[0].first, array[0].second);
    RANGEUP(i, 1, len) {
      printf(", {%" PRIuPTR ", %" PRIuPTR "}", array[i].first, array[i].second);
    }
    printf("}\n");
  }
}

/** Print the pairs in an array, where the first item is a string. */
void print_string_pairs(pair_t *array, size_t len) {
  if(!len) {
    printf("{}\n");
  } else {
    printf("{{%s, %" PRIuPTR "}", (char *)array[0].first, array[0].second);
    RANGEUP(i, 1, len) {
      printf(", {%s, %" PRIuPTR "}", (char *)array[i].first, array[i].second);
    }
    printf("}\n");
  }
}

TEST(sort) {
  /** [sort] */
  pair_t array[] = {{3, 0}, {7, 1}, {2, 2}, {4, 3}, {500, 4}, {0, 5}, {8, 6}, {4, 7}};
  quicksort(array, WIDTH(array), LENGTH(array));
  uintptr_t last = array[0].first;
  printf("{{%d, %d}", (int)array[0].first, (int)array[0].second);
  RANGEUP(i, 1, LENGTH(array)) {
    printf(", {%d, %d}", (int)array[i].first, (int)array[i].second);
    if(array[i].first < last) {
      printf(" <- ERROR\n");
      return -1;
    }
  }
  printf("}\n");

  pair_t *p1 = find(array, LENGTH(array), 7);
  bool r1 = p1 && p1->second == 1;
  printf("index find existing: %s\n", r1 ? "PASS" : "FAIL");
  bool r2 = !find(array, LENGTH(array), 5);
  printf("index find missing: %s\n", r2 ? "PASS" : "FAIL");
  /** [sort] */
  return r1 && r2 ? 0 : -1;
}

/** Use the Quicksort algorithm to sort an array of pairs by the first element.
 * @snippet support.c sort
 */
void quicksort(void *array, size_t width, size_t size) {
  if(size <= 1) return;

  unsigned int lo = 0, hi = size-1;
  struct frame {
    unsigned int lo, hi;
  } stack[size-1];
  void
    *pivot_value = alloca(width),
    *buf = alloca(width);
  struct frame *top = stack;

  for(;;) {
    void
      *pivot = INDEX(array, width, median3(array, width, lo, hi)),
      *right = INDEX(array, width, hi),
      *left = INDEX(array, width, lo),
      *x = left;
    memcpy(pivot_value, pivot, width);
    memcpy(pivot, right, width);
    unsigned int fill_index = lo;

    RANGEUP(i, lo, hi) {
      if(KEY(x) < KEY(pivot_value)) {

        // swap
        memcpy(buf, x, width);
        memcpy(x, left, width);
        memcpy(left, buf, width);

        left = INDEX(left, width, 1);
        fill_index++;
      }
      x = INDEX(x, width, 1);
    }
    memcpy(right, left, width);

    memcpy(left, pivot_value, width);

    if(hi > fill_index + 1) {
      top->lo = fill_index+1;
      top->hi = hi;
      top++;
    }

    if(fill_index > lo + 1) {
      hi = fill_index-1;
    } else if(top > stack) {
      top--;
      lo = top->lo;
      hi = top->hi;
    } else break;
  }
}

/** Find a pair with matching key in a sorted array of pairs using binary search.
 * O(log n) time.
 */
pair_t *find(pair_t *array, size_t size, uintptr_t key) {
  size_t low = 0, high = size;
  while(high > low) {
    const size_t pivot = low + ((high - low) / 2);
    const uintptr_t pivot_key = array[pivot].first;
    if(pivot_key == key) {
      return &array[pivot];
    } else if(pivot_key < key) {
      low = pivot + 1;
    } else {
      high = pivot;
    }
  }
  return NULL;
}

/** Like `find`, but find the last match.
 * O(log n + m) time, where m is the most entries with the same key.
 * *est is the estimated location
 */
pair_t *find_last(pair_t *array, size_t size, uintptr_t key, size_t *est) {
  size_t low = 0, high = size;
  size_t pivot = est ? *est : low + ((high - low + 1) / 2);
  while(high > low + 1) {
    COUNTER(find_last, 1);
    if(high - low <= 0) {
      size_t l = low;
      low = high - 1;
      RANGEUP(i, l + 1, high) {
        uintptr_t k = array[i].first;
        if(k > key) {
          low = i - 1;
          break;
        }
      }
      break;
    }
    const uintptr_t pivot_key = array[pivot].first;
    if(pivot_key <= key) {
      low = pivot;
    } else {
      high = pivot;
    }
    pivot = low + ((high - low + 1) / 2);
  }
  pair_t *p = &array[low];
  if(est) *est = low;
  return p->first == key ? p : NULL;
}

/** Like `find`, but find the last match, with a string key.
 * O(log n) time.
 */
pair_t *find_last_string(pair_t *array, size_t size, const char *key) {
  size_t low = 0, high = size;
  while(high > low + 1) {
    const size_t pivot = low + ((high - low + 1) / 2);
    const char *pivot_key = (char *)array[pivot].first;
    if(strcmp(pivot_key, key) <= 0) {
      low = pivot;
    } else {
      high = pivot;
    }
  }
  pair_t *p = &array[low];
  return strcmp((char *)p->first, key) == 0 ? p : NULL;
}

/** Compare a string to a seg_t */
int segcmp(const char *str, seg_t seg) {
  size_t cnt = seg.n;
  const char *a = str, *b = seg.s;
  while(cnt--) {
    int diff = *a - *b;
    if(diff || !*a) return diff;
    a++;
    b++;
  }
  return *a;
}

/** Return a character from a seg_t if it is a single character, otherwise '\0'. */
char seg_char(seg_t seg) {
  return seg.n == 1 ? *seg.s : '\0';
}

/** Like `find`, but find the last match, with a string segment key.
 * O(log n) time.
 */
pair_t *find_last_seg(pair_t *array, size_t size, seg_t key) {
  size_t low = 0, high = size;
  while(high > low + 1) {
    const size_t pivot = low + ((high - low + 1) / 2);
    const char *pivot_key = (char *)array[pivot].first;
    if(segcmp(pivot_key, key) <= 0) {
      low = pivot;
    } else {
      high = pivot;
    }
  }
  pair_t *p = &array[low];
  return segcmp((char *)p->first, key) == 0 ? p : NULL;
}

/** Return a pointer after the end of the string segment. */
const char *seg_end(seg_t seg) {
  return seg.s ? seg.s + seg.n : NULL;
}

/** Copy a string segment into a character array, as a zero terminated C string. */
size_t seg_read(seg_t seg, char *str, size_t size) {
  size_t n = size - 1 < seg.n ? size - 1 : seg.n;
  memcpy(str, seg.s, n);
  str[n] = 0;
  return n;
}

/** Return the segment after the first character matching `c`. */
seg_t seg_after(seg_t s, char c) {
  const char *p = s.s;
  const char *end = p + s.n;
  while(p < end) {
    if(*p == c) s.s = p;
    p++;
  }
  s.n = end - s.s;
  return s;
}

/** Test if `b` immediately follows `a`. */
bool seg_adjacent(seg_t a, seg_t b) {
  return a.s && b.s && a.s + a.n == b.s;
}

/** Create a string segment from a C string. */
seg_t string_seg(const char *str) {
  return (seg_t) {str, str ? strlen(str) : 0};
}

/** Look up the string segment key in a sorted table.
 * Each row starts with a C string key.
 * Binary search, O(log n) time.
 * @param table the table
 * @param width width of each row in bytes
 * @param rows the number of rows
 * @param key_seg the key string segment
 * @snippet support.c lookup
 */
void *lookup(void *table, size_t width, size_t rows, seg_t key_seg) {
  size_t low = 0, high = rows, pivot;
  char const *key = key_seg.s;
  size_t key_length = key_seg.n;
  void *entry, *ret = 0;
  if(key_length > width) key_length = width;
  while(high > low) {
    pivot = low + ((high - low) >> 1);
    entry = (uint8_t *)table + width * pivot;
    int c = strncmp(key, entry, key_length);
    if(c == 0) {
      /* keep looking for a lower key */
      ret = entry;
      high = pivot;
    } else if(c < 0) high = pivot;
    else low = pivot + 1;
  }
  return ret;
}

TEST(lookup) {
  /** [lookup] */
  struct row {
    char name[8]; // must be large enough for the key and terminating null byte
    int count;
    double cost_per;
  };

  /* The rows must be sorted by the key. */
  struct row table[] = {
    { "apple", 3, 0.95 },
    { "grape", 5, 0.50 },
    { "pear", 2, 1.50 }
  };

  struct row *result = lookup(table, WIDTH(table), LENGTH(table), string_seg("grape"));
  if(result) {
    printf("The total for %d %ss at $%.2f each is $%.2f.\n",
           result->count,
           result->name,
           result->cost_per,
           result->count * result->cost_per);
    return 0;
  } else {
    printf("Sorry, we don't have any of that.\n");
    return -1;
  }
  /** [lookup] */
}

/** Look up the string segment key in an unsorted table.
 * Each row starts with a C string key.
 * Linear search, O(n) time.
 * @param table the table
 * @param width width of each row in bytes
 * @param rows the number of rows
 * @param key_seg the key string segment
 */
void *lookup_linear(void *table, size_t width, size_t rows, seg_t key_seg) {
  char const *key = key_seg.s;
  size_t key_length = key_seg.n;
  uint8_t *entry = table;
  unsigned int rows_left = rows;
  if(key_length > width) key_length = width;
  while(rows_left-- && *entry) {
    if(!strncmp(key, (void *)entry, key_length)) return entry;
    entry += width;
  }
  return NULL;
}

#if INTERFACE
struct mmfile {
  int dirfd; /**< Directory file descriptor */
  const char *path; /**< File path */
  int fd; /**< File descriptor */
  size_t size; /**< Size in bytes, set by mmap_file */
  char *data; /**< Pointer to char data, set by mmap_file */
  bool read_only; /**< True if read-only */
};
#endif

/** Map a file to memory
 * Use `path` and `read_only` from the `mmfile` referenced by `f`.
 * Sets `data` and `size`.
 * @return `true` on success, otherwise `false`.
 * @snippet support.c mmap_file
 */
bool mmap_file(struct mmfile *f) {
  f->fd = openat(f->dirfd ? f->dirfd : AT_FDCWD, f->path, f->read_only ? O_RDONLY : O_RDWR);
  if(f->fd < 0) return false;
  struct stat file_info;
  if(fstat(f->fd, &file_info) < 0) return false;
  f->size = file_info.st_size;
  if(f->size == 0) {
    f->data = NULL;
    return true;
  }
  f->data = mmap(f->data,
                 f->size,
                 f->read_only ? PROT_READ : PROT_READ | PROT_WRITE,
                 MAP_SHARED,
                 f->fd,
                 0);

  if(f->data == MAP_FAILED) {
    f->data = NULL;
    return false;
  } else {
    return true;
  }
}

/** Un-map a file from memory using the data set by `mmap_file`.
 * @snippet support.c mmap_file
 */
bool munmap_file(struct mmfile *f) {
  bool success = true;
  success &= munmap(f->data, f->size) == 0;
  success &= close(f->fd) == 0;
  memset(f, 0, sizeof(*f));
  return success;
}

TEST(mmap_file) {
  /** [mmap_file] */
  struct mmfile f = {
    .path = "eval.c",
    .read_only = true
  };
  if(!mmap_file(&f)) return -1;
  char *c = f.data + 3;
  size_t n = f.size;
  while(n--) {
    putchar(*c);
    if(*c == '\n') break;
    c++;
  }
  if(!munmap_file(&f)) return -1;
  /** [mmap_file] */
  return 0;
}

/** Count lines to reach `e` starting from `s`. */
size_t line_number(const char *s, const char *e) {
  if(!s || !e) return 0;
  size_t cnt = 1;
  while(s < e) {
    if(*s++ == '\n') cnt++;
  }
  return cnt;
}

/** Find a line containing the address.
 * @param x address into a line of text
 * @s [in,out] start of text/line
 * @size [in,out] size of text/line
 * @return true if the line was found
 */
bool find_line(const char *x, const char **s, size_t *size) {
  if(!x || !s || !size || !*s || x < *s || (size_t)(x - *s) >= *size) return false;
  const char *e = *s + *size;
  const char *p = *s;
  const char *ls = p;
  const char *le = e;
  while(p < x) {
    if(*p == '\n') {
      ls = p;
    }
    p++;
  }
  while(p < e) {
    if(*p == '\n') {
      le = p;
      break;
    }
    p++;
  }
  *s = ls;
  *size = le - ls;
  return true;
}

/** Integer log2 */
unsigned int int_log2(unsigned int x) {
  return x <= 1 ? 0 : sizeof_bits(x) - __builtin_clz(x - 1);
}

/** Long integer log2 */
unsigned int int_log2l(long unsigned int x) {
  return x <= 1 ? 0 : sizeof_bits(x) - __builtin_clzl(x - 1);
}

/** Insert into a set.
 * O(1) time.
 * @param x to insert
 * @param set the set
 * @param size number of entries the set can hold
 * @return true if `x` was already in the set
 * @snippet support.c set
 */
bool set_insert(uintptr_t x, uintptr_t *set, size_t size) {
  assert_error(x);
  size_t j = x % size;
  size_t d = 0;
  COUNTUP(i, size) {
    uintptr_t *p = &set[j];
    if(*p == x) return true;
    if(*p) {
      /* Robin Hood hash */
      size_t dx = (size + j - *p % size) % size;
      if(dx < d) {
        d = dx;
        uintptr_t tmp = *p;
        *p = x;
        x = tmp;
      }
    } else {
      *p = x;
      return false;
    }
    d++;
    j = (j + 1) % size;
  }

  assert_throw(false, "set is full");
  return false;
}

size_t squeeze(uintptr_t *set, size_t size) {
  uintptr_t *end = set + size;
  uintptr_t *r = set;
  while(r < end && *r) r++;
  uintptr_t *l = r;
  while(r < end) {
    if(*r) *l++ = *r;
    r++;
  }
  return l - set;
}

/** Return if an item is in the set.
 * O(1) time.
 * @param x item to test
 * @param set the set
 * @param size number of entries the set can hold
 * @return true if `x` is in the set
 * @snippet support.c set
 */
bool set_member(uintptr_t x, uintptr_t *set, size_t size) {
  if(!x) return false;
  size_t offset = x % size;
  COUNTUP(i, size) {
    uintptr_t *p = &set[(i + offset) % size];
    if(*p == x) return true;
  }
  return false;
}

/** Remove an item from the set.
 * O(1) time.
 * @param x to remove
 * @param set the set
 * @param size number of entries the set can hold
 * @return true if `x` was in the set
 * @snippet support.c set
 */
bool set_remove(uintptr_t x, uintptr_t *set, size_t size) {
  if(!x) return false;
  size_t offset = x % size;
  COUNTUP(i, size) {
    uintptr_t *p = &set[(i + offset) % size];
    if(*p == x) {
      *p = 0;
      return true;
    }
  }
  return false;
}

TEST(set) {
  /** [set] */
  uintptr_t set[7] = {0};
  const size_t size = LENGTH(set);
  uintptr_t data[] = {7, 8, 9, 14, 21, 28};
  FOREACH(i, data) {
    if(set_insert(data[i], set, size)) return -1;
  }
  FOREACH(i, set) {
    if(set[i]) printf("set[%d] = %d\n", (int)i, (int)set[i]);
  }
  FOREACH(i, data) {
    if(!set_member(data[i], set, size)) return -2;
  }
  FOREACH(i, data) {
    if(!set_remove(data[i], set, size)) return -3;
  }
  /** [set] */
  return 0;
}

/** Swap two pointers. */
void swap_ptrs(void **x, void **y) {
  void *tmp = *x;
  *x = *y;
  *y = tmp;
}

/** Reverse an array of pointers.
 * @param n number of pointers.
 */
void reverse_ptrs(void **a, size_t n) {
  size_t m = n / 2;
  void **b = a + n;
  while(m--) swap_ptrs(a++, --b);
}

char *arguments(int argc, char **argv) {
  if(argc == 0) return NULL;
  size_t len = argc + 1;
  COUNTUP(i, argc) {
    len += strlen(argv[i]);
  }
  char *result = malloc(len), *p = result;
  COUNTUP(i, argc) {
    char *a = argv[i];
    char *np = stpcpy(p, a);
    if(a[0] == '-' && is_letter(a[1])) {
      *p = ':';
      if(i) {
        *(p-1) = '\n';
      }
    }
    *np = ' ';
    p = np + 1;
  }
  *(p-1) = '\n';
  *p = '\0';
  return result;
}

/** Hash the string to a non-zero number. */
uintptr_t nonzero_hash(const char *str, size_t len)
{
  uintptr_t hash = 5381;
  LOOP(len) {
    hash = hash * 33 + (unsigned char)*str++;
  }
  return hash ? hash : -1;
}

int ctz(uintptr_t x) {
  return x ? __builtin_ctzll(x) : 0;
}

int next_bit(uintptr_t *mask) {
  if(*mask) {
    int res = __builtin_ctzll(*mask);
    *mask &= ~(1 << res);
    return res;
  } else {
    return -1;
  }
}

/** Replace the first occurrence of a character. */
char *replace_char(char *s, char c_old, char c_new) {
  char *p = s;
  while(*p) {
    if(*p == c_old) {
      *p = c_new;
      break;
    }
    p++;
  }
  return s;
}

/** Replace all occurrences of a character. */
char *replace_all_char(char *s, char c_old, char c_new) {
  char *p = s;
  while(*p) {
    if(*p == c_old) {
      *p = c_new;
    }
    p++;
  }
  return s;
}

TEST(replace_char) {
  char buf[] = "apples and bananas";
  printf("%s\n", replace_char(buf, 'p', 'P'));
  printf("%s\n", replace_all_char(buf, 'a', 'o'));
  return 0;
}

bool is_whitespace(char c) {
  return ONEOF(c, ' ', '\n', '\r', '\t');
}

bool is_letter(char c) {
  return INRANGE(c, 'a', 'z') || INRANGE(c, 'A', 'Z');
}

seg_t seg_trim(seg_t s) {
  while(s.n && is_whitespace(*s.s)) {
    s.n--;
    s.s++;
  }
  const char *e = seg_end(s) - 1;
  while(s.n && is_whitespace(*e)) {
    e--;
    s.n--;
  }
  return s;
}

TEST(seg_trim) {
  seg_t s = string_seg(" \t  hi    \n");
  s = seg_trim(s);
  printf("trimmed: \"%.*s\"\n", (int)s.n, s.s);
  return strncmp("hi", s.s, s.n) == 0 ? 0 : -1;
}

int digit_value(char c) {
  if(INRANGE(c, '0', '9')) {
    return c - '0';
  } else if(INRANGE(c, 'A', 'Z')) {
    return c - 'A' + 10;
  } else if(INRANGE(c, 'a', 'z')) {
    return c - 'a' + 10;
  } else {
    return -1;
  }
}

char digit(int n) {
  if(!INRANGE(n, 0, 35)) return '?';
  if(n <= 9) return '0' + n;
  return 'a' + (n - 10);
}

int strnum(const char *s, size_t n, int base) {
  if(!INRANGE(base, 1, 36)) return -1;
  int v = 0;
  LOOP(n) {
    if(*s == '\0') break;
    int d = digit_value(*s++);
    if(!INRANGE(d, 0, base - 1)) return -1;
    v = v * base + d;
  }
  return v;
}

TEST(strnum) {
  if(strnum("1234", 3, 8) != 0123) return -1;
  if(strnum("80", 2, 8) != -1) return -2;
  if(strnum("cAFe", 4, 16) != 0xcafe) return -3;
  return 0;
}

size_t unescape_string(char *dst, size_t n, seg_t str) {
  char *d = dst;
  const char
    *s = str.s,
    *s_end = seg_end(str),
    *d_end = d + n;
  while(*s && s < s_end && d < d_end) {
    if(s[0] == '\\' && s + 1 < s_end) {
      switch(s[1]) {
      case '\'': *d++ = '\''; break;
      case '\"': *d++ = '\"'; break;
      case '\\': *d++ = '\\'; break;
      case '0':  *d++ = '\0'; break; // just handle \0
      case '?':  *d++ = '?';  break;
      case 'a':  *d++ = '\a'; break;
      case 'b':  *d++ = '\b'; break;
      case 'f':  *d++ = '\f'; break;
      case 'n':  *d++ = '\n'; break;
      case 'r':  *d++ = '\r'; break;
      case 't':  *d++ = '\t'; break;
      case 'v':  *d++ = '\v'; break;
      case 'x': {
        if(s + 3 >= s_end) goto copy_char;
        int v = strnum(s + 2, 2, 16);
        if(v < 0) goto copy_char;
        *d++ = v;
        s += 4;
      } continue;
        // TODO Unicode
      default:
        goto copy_char;
      }
      s += 2;
    } else {
    copy_char:
      *d++ = *s++;
    }
  }
  if(d < d_end) *d = '\0';
  return d - dst;
}

TEST(unescape_string) {
  char out[32];
  seg_t escaped = SEG("test\\n\\\\string\\bG\\x21\\0stuff");
  seg_t unescaped = SEG("test\n\\string\bG\x21\0stuff");
  unescape_string(out, sizeof(out), escaped);
  printf("%s\n", out);
  return segcmp(out, unescaped) == 0 ? 0 : -1;
}

size_t escape_string(char *dst, size_t n, seg_t str, bool xml) {
  char *d = dst;
  const char
    *s = str.s,
    *s_end = seg_end(str),
    *d_end = d + n;
  while(s < s_end && d < d_end) {
    char c = *s++;
    if(INRANGE(c, 32, 126) && c != '\\') {
      if(xml) {
        char *out = NULL;
        int out_n = 0;

#define CASE(c, res)                            \
        case c:                                 \
          out = res;                            \
          out_n = sizeof(res) - 1;              \
          break

        switch(c) {
          CASE('<', "&lt;");
          CASE('>', "&gt;");
          CASE('"', "&quot;");
          CASE('`', "&apos;");
          CASE('&', "&amp;");
        }
#undef CASE
        if(out) {
          if(d + out_n >= d_end) break;
          memcpy(d, out, out_n);
          d += out_n;
        } else {
          *d++ = c;
        }
      } else {
        *d++ = c;
      }
    } else {
      if(d + 1 >= d_end) break;
      switch(c) {
      case '\'':
      case '\"':
      case '\\':
        break;
      case '\0': c = '0'; break;
      case '\a': c = 'a'; break;
      case '\b': c = 'b'; break;
      case '\f': c = 'f'; break;
      case '\n': c = 'n'; break;
      case '\r': c = 'r'; break;
      case '\t': c = 't'; break;
      case '\v': c = 'v'; break;
      default:
        if(d + 3 >= d_end) break;
        *d++ = '\\';
        *d++ = 'x';
        *d++ = digit(c >> 4);
        *d++ = digit(c & 0x0f);
        continue;
      };
      *d++ = '\\';
      *d++ = c;
    }
  }
  if(d < d_end) *d = '\0';
  return d - dst;
}

TEST(escape_string) {
  char out[64];
  seg_t escaped = SEG("test\\n\\\\string\\bG\\0stuff\\x1b&amp;t");
  seg_t unescaped = SEG("test\n\\string\bG\0stuff\x1b&t");
  escape_string(out, sizeof(out), unescaped, true);
  printf("%s\n", out);
  return segcmp(out, escaped) == 0 ? 0 : -1;
}

void fprintf_escaped_string(FILE *f, seg_t str, bool xml) {
  char buf[64];
  while(str.n > 16) {
    escape_string(buf, sizeof(buf), (seg_t) { .s = str.s, .n = 16 }, xml);
    fprintf(f, "%s", buf);
    str.s += 16;
    str.n -= 16;
  }
  escape_string(buf, sizeof(buf), str, xml);
  fprintf(f, "%s", buf);
}

void print_escaped_string(seg_t str, bool xml) {
  fprintf_escaped_string(stdout, str, xml);
}

TEST(print_escaped_string) {
  seg_t unescaped = SEG("test\n\\string\bG\0stuff&");
  print_escaped_string(unescaped, true);
  printf("\n");
  return 0;
}

bool eq_seg(seg_t a, seg_t b) {
  return a.n == b.n && memcmp(a.s, b.s, a.n) == 0;
}

#if INTERFACE
typedef struct ring_buffer {
  size_t head, tail, size;
  char data[0];
} ring_buffer_t;

#define APPEND_DATA_TO(_type, _size)            \
  struct {                                      \
    _type hdr;                                  \
    char data[_size];                           \
  }

#define RING_BUFFER(_size)                      \
  ((ring_buffer_t *)                            \
   &(APPEND_DATA_TO(ring_buffer_t, _size))      \
  {{ .size = _size }, {0}})
#endif

TEST(append_data_to) {
  APPEND_DATA_TO(ring_buffer_t, 1) x;
  return x.data == x.hdr.data ? 0 : -1;
}

ring_buffer_t *rb_init(char *mem, size_t n) {
  if(n < sizeof(ring_buffer_t)) return NULL;
  ring_buffer_t *rb = (ring_buffer_t *)mem;
  rb->head = 0;
  rb->tail = 0;
  rb->size = n - sizeof(ring_buffer_t);
  return rb;
}

size_t rb_available(const ring_buffer_t *rb) {
  return (rb->size + rb->head - rb->tail) % rb->size;
}

size_t rb_capacity(const ring_buffer_t *rb) {
  return rb->size - rb_available(rb) - 1;
}

void rb_clear(ring_buffer_t *rb) {
  rb->tail = rb->head;
}

size_t rb_write(ring_buffer_t *rb, const char *src, size_t size) {
  size = min(size, rb_capacity(rb));
  size_t right = rb->size - rb->head;
  if(size <= right) {
    memcpy(&rb->data[rb->head], src, size);
    rb->head += size;
  } else {
    memcpy(&rb->data[rb->head], src, right);
    size_t left = size - right;
    memcpy(&rb->data[0], src + right, left);
    rb->head = left;
  }
  return size;
}

size_t rb_read(ring_buffer_t *rb, char *dst, size_t size) {
  size = min(size, rb_available(rb));
  size_t right = rb->size - rb->tail;
  if(size <= right) {
    memcpy(dst, &rb->data[rb->tail], size);
    rb->tail += size;
  } else {
    memcpy(dst, &rb->data[rb->tail], right);
    size_t left = size - right;
    memcpy(dst + right, &rb->data[0], left);
    rb->tail = left;
  }
  return size;
}

#define WRITE(str) rb_write(rb, (str), sizeof(str)-1)
#define PRINT(n)                                                \
  do {                                                          \
    size_t _size = rb_read(rb, out, min(sizeof(out), (n)));     \
    printf("%.*s\n", (int)_size, out);                          \
  } while(0)

TEST(ring_buffer) {
  char out[8];
  ring_buffer_t *rb = RING_BUFFER(8);

  WRITE("hello");
  PRINT(2);
  PRINT(2);
  WRITE(" world!");
  PRINT(8);
  return 0;
}

const char *seg_find(seg_t haystack, seg_t needle) {
  if(needle.n > haystack.n) return NULL;
  if(needle.n == 0) return haystack.s;
  const char *end = haystack.s + (haystack.n - needle.n + 1);
  const char *p = haystack.s;
  while(p < end) {
    if(memcmp(p, needle.s, needle.n) == 0) return p;
    p++;
  }
  return NULL;
}

TEST(seg_find) {
  seg_t hello = SEG("Hello. My name is Inigo Montoya. You killed my father. Prepare to die.");
  seg_t father = SEG("father");
  seg_t mother = SEG("mother");
  if(!seg_find(hello, father)) return -1;
  if(seg_find(hello, mother)) return -2;
  return 0;
}

const char *seg_find_char(seg_t haystack, char needle) {
  if(haystack.n == 0) return NULL;
  const char *end = seg_end(haystack);
  const char *p = haystack.s;
  while(p < end) {
    if(*p == needle) return p;
    p++;
  }
  return NULL;
}

TEST(seg_find_char) {
  seg_t hello = SEG("hello");
  if(!seg_find_char(hello, 'l')) return -1;
  if(seg_find_char(hello, '!')) return -2;
  return 0;
}

char capitalize(char c) {
  return INRANGE(c, 'a', 'z') ? c - ('a' - 'A') : c;
}

range_t range_intersect(range_t a, range_t b) {
  return (range_t) {
    .min = max(a.min, b.min),
    .max = min(a.max, b.max)
  };
}

bool range_empty(range_t a) {
  return a.min > a.max;
}

range_t range_union(range_t a, range_t b) {
  if(range_empty(b)) return a;
  if(range_empty(a)) return b;
  return (range_t) {
    .min = min(a.min, b.min),
    .max = max(a.max, b.max)
  };
}

bool range_singleton(range_t a) {
  return a.min == a.max;
}

#if INTERFACE
#define RANGE_1(x)                              \
  ({                                            \
    __typeof__(x) __x = (x);                    \
    (range_t) { .min = __x, .max = __x };       \
  })
#define RANGE_2(lo, hi) ((range_t) { .min = (lo), .max = (hi) })
#define RANGE(...) DISPATCH(RANGE, __VA_ARGS__)
#endif

bool range_eq(range_t a, range_t b) {
  return
    a.min == b.min &&
    a.max == b.max;
}

intptr_t range_span(range_t a) {
  return a.max >= a.min ? sat_subi(a.max, a.min) + 1 : 0;
}

bool range_has_lower_bound(range_t a) {
  return a.min != INTPTR_MIN;
}

bool range_has_upper_bound(range_t a) {
  return a.max != INTPTR_MAX;
}

bool range_bounded(range_t a) {
  return
    range_has_lower_bound(a) &&
    range_has_upper_bound(a);
}

range_t range_bits(int b) {
  if(b == 0) {
    return (range_t) {
      .min = 0,
      .max = 0
    };
  } else if(b < 0) {
    if(b <= -(int)sizeof_bits(intptr_t))
      return RANGE_ALL;
    int w = -b - 1;
    return (range_t) {
      .min = -(((intptr_t)1) << w),
        .max = (((intptr_t)1) << w) - 1
    };
  } else {
    if(b >= (int)sizeof_bits(intptr_t) - 1)
      return RANGE_POS;
    return (range_t) {
      .min = 0,
      .max = (((intptr_t)1) << b) - 1
    };
  }
}

bool range_partially_bounded(range_t a) {
  return
    range_has_lower_bound(a) ||
    range_has_upper_bound(a);
}

intptr_t sign(intptr_t x) {
  return x < 0 ? -1 :
    x > 0 ? 1 : 0;
}

intptr_t div_min(intptr_t n, intptr_t d) {
  return (n < 0) == (d < 0) ? n / d :
    sat_sub(n, d - sign(d)) / d;
}

intptr_t div_mini(intptr_t n, intptr_t d) {
  if(n == INTPTR_MAX) return INTPTR_MAX;
  if(n == INTPTR_MIN) return INTPTR_MIN;
  return div_min(n, d);
}

intptr_t div_max(intptr_t n, intptr_t d) {
  return (n < 0) != (d < 0) ? n / d :
    sat_add(n, d - sign(d)) / d;
}

intptr_t div_maxi(intptr_t n, intptr_t d) {
  if(n == INTPTR_MAX) return INTPTR_MAX;
  if(n == INTPTR_MIN) return INTPTR_MIN;
  return div_max(n, d);
}

intptr_t div_i(intptr_t n, intptr_t d) {
  if(n == INTPTR_MAX) return INTPTR_MAX;
  if(n == INTPTR_MIN) return INTPTR_MIN;
  return n / d;
}

intptr_t sat_abs(intptr_t x) {
  if(x == INTPTR_MIN) return INTPTR_MAX;
  return x < 0 ? -x : x;
}

intptr_t sat_neg(intptr_t x) {
  if(x == INTPTR_MIN) return INTPTR_MAX;
  return -x;
}

intptr_t sat_negi(intptr_t x) {
  if(x == INTPTR_MIN) return INTPTR_MAX;
  if(x == INTPTR_MAX) return INTPTR_MIN;
  return -x;
}

#define SHOW(expr, expected)                                            \
  {                                                                     \
    intptr_t x = (expr);                                                \
    intptr_t e = (expected);                                            \
    bool pass = x == e;                                                 \
    printf(#expr " = %" PRIdPTR ", expected %" PRIdPTR ": %s\n",        \
           x, e, pass ? "PASS" : "FAIL");                               \
    if(!pass) ret = -1;                                                 \
  }

TEST(div_min_max) {
  int ret = 0;
  SHOW(div_min(5, 3), 1);
  SHOW(div_min(-5, 3), -2);
  SHOW(div_min(5, -3), -2);
  SHOW(div_min(-5, -3), 1);

  SHOW(div_max(5, 3), 2);
  SHOW(div_max(-5, 3), -1);
  SHOW(div_max(5, -3), -1);
  SHOW(div_max(-5, -3), 2);

  SHOW(div_min(2, 2), 1);
  SHOW(div_min(2, -2), -1);
  SHOW(div_min(-2, 2), -1);
  SHOW(div_min(-2, -2), 1);
  SHOW(div_max(2, 2), 1);
  SHOW(div_max(2, -2), -1);
  SHOW(div_max(-2, 2), -1);
  SHOW(div_max(-2, -2), 1);

  return ret;
}

intptr_t sat_add(intptr_t x, intptr_t y) {
  if(x >= 0) {
    if(y > INTPTR_MAX - x) return INTPTR_MAX;
  } else {
    if(y < INTPTR_MIN - x) return INTPTR_MIN;
  }
  return x + y;
}

intptr_t sat_addi(intptr_t x, intptr_t y) {
  if(ONEOF(INTPTR_MAX, x, y)) return INTPTR_MAX;
  if(ONEOF(INTPTR_MIN, x, y)) return INTPTR_MIN;
  return sat_add(x, y);
}

TEST(sat_arith) {
  int ret = 0;
  SHOW(sat_add(INTPTR_MIN, INTPTR_MIN), INTPTR_MIN);
  SHOW(sat_add(INTPTR_MAX, INTPTR_MIN), -1);
  SHOW(sat_add(INTPTR_MIN, INTPTR_MAX), -1);
  SHOW(sat_add(INTPTR_MAX, INTPTR_MAX), INTPTR_MAX);

  SHOW(sat_sub(INTPTR_MIN, INTPTR_MIN), 0);
  SHOW(sat_sub(INTPTR_MAX, INTPTR_MIN), INTPTR_MAX);
  SHOW(sat_sub(INTPTR_MIN, INTPTR_MAX), INTPTR_MIN);
  SHOW(sat_sub(INTPTR_MAX, INTPTR_MAX), 0);

  SHOW(sat_mul(INTPTR_MIN, 2), INTPTR_MIN);
  SHOW(sat_mul(INTPTR_MAX, 2), INTPTR_MAX);
  SHOW(sat_mul(2, INTPTR_MIN), INTPTR_MIN);
  SHOW(sat_mul(2, INTPTR_MAX), INTPTR_MAX);
  SHOW(sat_mul(INTPTR_MIN, -2), INTPTR_MAX);
  SHOW(sat_mul(INTPTR_MAX, -2), INTPTR_MIN);
  SHOW(sat_mul(-2, INTPTR_MIN), INTPTR_MAX);
  SHOW(sat_mul(-2, INTPTR_MAX), INTPTR_MIN);
  return ret;
}

intptr_t sat_sub(intptr_t x, intptr_t y) {
  if(x >= 0) {
    if(y < x - INTPTR_MAX) return INTPTR_MAX;
  } else {
    if(y > x - INTPTR_MIN) return INTPTR_MIN;
  }
  return x - y;
}

intptr_t sat_subi(intptr_t x, intptr_t y) {
  if(x == INTPTR_MAX || y == INTPTR_MIN) return INTPTR_MAX;
  if(x == INTPTR_MIN || y == INTPTR_MAX) return INTPTR_MIN;
  return sat_sub(x, y);
}

intptr_t sat_mul(intptr_t x, intptr_t y) {
  if(x == 0 || y == 0) return 0;
  intptr_t low_limit = x > 0 ? INTPTR_MIN : INTPTR_MAX;
  intptr_t high_limit = x > 0 ? INTPTR_MAX : INTPTR_MIN;
  intptr_t min_y = div_max(low_limit, x);
  if(y < min_y) return low_limit;
  intptr_t max_y = div_min(high_limit, x);
  if(y > max_y) return high_limit;
  return x * y;
}

intptr_t sat_muli(intptr_t x, intptr_t y) {
  if(x == 0 || y == 0) return 0;
  if(ONEOF(x, INTPTR_MIN, INTPTR_MAX) ||
     ONEOF(x, INTPTR_MIN, INTPTR_MAX)) {
    return (x > 0) == (y > 0) ?
      INTPTR_MAX : INTPTR_MIN;
  }
  return sat_mul(x, y);
}

seg_t seg_range(seg_t a, seg_t b) {
  if(!b.s) {
    return a;
  } else if(!a.s) {
    return b;
  } else {
    const char *l = min(a.s, b.s);
    const char *r = max(seg_end(a), seg_end(b));
    return (seg_t) {
      .s = l,
      .n = r - l
    };
  }
}

TEST(seg_range) {
  const char *text = "just some random words";
  seg_t
    just = (seg_t) { .s = text, .n = 4 },
    some = (seg_t) { .s = text + 5, .n = 4 },
    random = (seg_t) { .s = text + 10, .n = 6 },
    words = (seg_t) { .s = text + 17, .n = 5 };

#define SEG_RANGE_TEST(c, a, b)                                         \
    c = seg_range(a, b);                                                \
    printf("\"%.*s\" + \"%.*s\" = \"%.*s\"\n", (int)a.n, a.s, (int)b.n, b.s, (int)c.n, c.s)

    seg_t a, b, c, d, e = { .s = NULL };
    SEG_RANGE_TEST(a, random, just);
    SEG_RANGE_TEST(b, some, words);
    SEG_RANGE_TEST(c, a, b);
    SEG_RANGE_TEST(d, e, c);
#undef SEG_RANGE_TEST
    return 0;
}

size_t flatten_ranges(uintptr_t *l, uintptr_t *r, pair_t *res, size_t n) {
  quicksort(l, WIDTH(l), n);
  quicksort(r, WIDTH(r), n);
  int depth = 0;
  size_t out_n = 0;
  uintptr_t *l_end = l + n;
  LOOP(n * 2) {
    if(l >= l_end || *l > *r) {
      depth--;
      if(!depth) {
        res->second = *r;
        res++;
        out_n++;
      }
      r++;
    } else if (*l < *r) {
      if(!depth) {
        res->first = *l;
      }
      depth++;
      l++;
    } else { // *l == *r
      l++;
      r++;
    }
  }
  return out_n;
}

TEST(flatten_ranges) {
  uintptr_t l[] = {0, 3, 8,  9,  15, 15, 13};
  uintptr_t r[] = {5, 7, 12, 11, 15, 18, 14};
  pair_t res[LENGTH(l)];
  size_t n = flatten_ranges(l, r, res, LENGTH(l));
  COUNTUP(i, n) {
    printf("[%d, %d]\n", (int)res[i].first, (int)res[i].second);
  }
  return 0;
}

pair_t seg_pair(seg_t seg) {
  return seg.s ?
    (pair_t) {
      .first = (uintptr_t)seg.s,
      .second = (uintptr_t)(seg.s + seg.n)
    } :
  (pair_t) {0, 0};
}

seg_t pair_seg(pair_t p) {
  return p.first && p.second ?
    (seg_t) {
      .s = (char *)p.first,
      .n = (char *)p.second - (char *)p.first
    } :
  (seg_t) {NULL, 0};
}

/** Return the string after the last occurrence of a character. */
const char *suffix(const char *str, char c) {
  char *p = strrchr(str, c);
  return p ? p + 1 : str;
}

TEST(suffix) {
  const char *str = "start:middle:end";
  printf("%s\n", suffix(str, ':'));
  return 0;
}

/** Count characters in a string. */
int count_char(const char *str, char c) {
  const char *p = str;
  int count = 0;
  while(*p) {
    count += *p++ == c;
  }
  return count;
}

TEST(count_char) {
  return count_char("banana", 'a') == 3 ? 0 : -1;
}
