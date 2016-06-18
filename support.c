#include "rt_types.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>

#include "gen/support.h"

// find the median of 3 integers
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

void swap(pair_t *x, pair_t *y) {
  pair_t tmp = *x;
  *x = *y;
  *y = tmp;
}

void print_pairs(pair_t *array, size_t len) {
  if(!len) {
    printf("{}\n");
  } else {
    printf("{{%" PRIuPTR ", %" PRIuPTR "}", array[0].first, array[0].second);
    for(unsigned int i = 1; i < len; i++) {
      printf(", {%" PRIuPTR ", %" PRIuPTR "}", array[i].first, array[i].second);
    }
    printf("}\n");
  }
}

void print_string_pairs(pair_t *array, size_t len) {
  if(!len) {
    printf("{}\n");
  } else {
    printf("{{%s, %" PRIuPTR "}", (char *)array[0].first, array[0].second);
    for(unsigned int i = 1; i < len; i++) {
      printf(", {%s, %" PRIuPTR "}", (char *)array[i].first, array[i].second);
    }
    printf("}\n");
  }
}

int test_sort(UNUSED char *name) {
  pair_t array[] = {{3, 0}, {7, 1}, {2, 2}, {4, 3}, {500, 4}, {0, 5}, {8, 6}, {4, 7}};
  quicksort(array, LENGTH(array));
  uintptr_t last = array[0].first;
  printf("{{%d, %d}", (int)array[0].first, (int)array[0].second);
  for(unsigned int i = 1; i < LENGTH(array); i++) {
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

  return r1 && r2 ? 0 : -1;
}

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

    for(unsigned int i = lo; i < hi; i++, x++) {
      if(x->first < pivot_value.first) {
        swap(x, left);
        left++;
        fill_index++;
      }
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

// find the index of a value in a sorted array
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

// find the last index of a value in a sorted array
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

// find the last index of a value in a sorted array with string keys
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

// compare a string to a seg_t
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

// find the last index of a value in a sorted array with string keys using a seg_t key
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

// string segments
const char *seg_end(seg_t seg) {
  return seg.s ? seg.s + seg.n : NULL;
}

size_t seg_read(seg_t seg, char *str, size_t size) {
  size_t n = size - 1 < seg.n ? size - 1 : seg.n;
  memcpy(str, seg.s, n);
  str[n] = 0;
  return n;
}

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
