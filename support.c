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
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdbool.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"

/** @file
 *  @brief Generally useful functions
 */

/** Estimate the median of an array */
unsigned int median3(pair_t *array, unsigned int lo, unsigned int hi) {
  unsigned int mid = lo + (hi - lo) / 2;
  uint32_t
    a = array[lo].first,
    b = array[mid].first,
    c = array[hi].first;
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
  quicksort(array, LENGTH(array));
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
void quicksort(pair_t *array, unsigned int size) {
  if(size <= 1) return;

  unsigned int lo = 0, hi = size-1;
  struct frame {
    unsigned int lo, hi;
  } stack[size-1];
  struct frame *top = stack;

  for(;;) {
    pair_t
      *pivot = &array[median3(array, lo, hi)],
      *right = &array[hi],
      *left = &array[lo],
      *x = left;
    pair_t pivot_value = *pivot;
    *pivot = *right;
    unsigned int fill_index = lo;

    RANGEUP(i, lo, hi) {
      if(x->first < pivot_value.first) {
        swap(x, left);
        left++;
        fill_index++;
      }
      x++;
    }
    *right = *left;

    *left = pivot_value;

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
 * O(log n) time.
 */
pair_t *find_last(pair_t *array, size_t size, uintptr_t key) {
  size_t low = 0, high = size;
  while(high > low + 1) {
    const size_t pivot = low + ((high - low + 1) / 2);
    const uintptr_t pivot_key = array[pivot].first;
    if(pivot_key <= key) {
      low = pivot;
    } else {
      high = pivot;
    }
  }
  pair_t *p = &array[low];
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

/** Create a string segment from a C string. */
seg_t string_seg(const char *str) {
  seg_t seg = {str, strlen(str)};
  return seg;
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
  f->fd = open(f->path, f->read_only ? O_RDONLY : O_RDWR);
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
  return x <= 1 ? 0 : (sizeof(x) * 8) - __builtin_clz(x - 1);
}

/** Long integer log2 */
unsigned int int_log2l(long unsigned int x) {
  return x <= 1 ? 0 : (sizeof(x) * 8) - __builtin_clzl(x - 1);
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
    if(*a == '-') {
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
