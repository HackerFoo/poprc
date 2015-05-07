#include "rt_types.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "gen/support.h"

#if INTERFACE
typedef struct pair_t {
  uintptr_t first, second;
} pair_t;
#endif

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
    printf("{{%ld, %ld}", (long int)array[0].first, (long int)array[0].second);
    for(unsigned int i = 1; i < len; i++) {
      printf(", {%ld, %ld}", (long int)array[i].first, (long int)array[i].second);
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
static TEST(test_sort);

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
