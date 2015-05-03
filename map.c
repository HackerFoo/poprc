#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "rt_types.h"
#include "gen/support.h"

void merge(pair_t *arr, size_t n) {
  //printf("merge ");
  //print_pairs(arr, n);
  pair_t
    *a = arr,
    *b = arr + (n >> 1),
    *q = b, // queue where the head is q and the tail is q - 1
    *q_low = q; // q_low is the lowest queue address

  while(a < b) {
    /*
    printf("\n");
    print_pairs(arr, a-arr);
    printf("a: ");
    print_pairs(a, q_low-a);
    printf("q[%d]: ", q-q_low);
    print_pairs(q_low, b-q_low);
    printf("b: ");
    print_pairs(b, n-(b-arr));
    */
    if(q >= q_low && q != b && q->first < a->first) {
      if(b->first < q->first) { // b < q < a
        goto select_b;
      } else { // q < a, b >= q
        swap(a, q++);
        if(q == b) { // queue ran out of elements after q, un-rotate
          q = q_low;
        }
      }
    } else if(b->first < a->first) {
    select_b:
      // `b` moves, increasing the size of the queue
      // put `a` at the tail of the queue
      if(q <= q_low) {
        // no bubble
        swap(a, b);
      } else {
        // remove bubble in the queue
        pair_t _a = *a;
        pair_t * const t = q - 1;
        *a = *b;
        *b = *t;
        memmove(q_low, q_low + 1, (char *)q - (char *)(q_low + 1));

        // put `a` at the tail
        *t = _a;
      }
      b++;
      if(b >= arr + n) return;
    }

    a++;

    // if a runs into the queue, just keep going
    // this works because the queue is sorted
    if(a >= q_low) q_low++;
    if(q < q_low) q = q_low;
  }
}

#define MERGE_TEST(arr)                         \
  do {                                          \
    merge((arr), LENGTH(arr));                  \
    printf(#arr ": ");                          \
    print_pairs((arr), LENGTH(arr));            \
  } while(0)


static int test_merge(UNUSED char *name) {
  pair_t arr1[] = {{0, 0}, {2, 1}, {4, 2}, {6, 3}, {1, 4}, {3, 5}, {5, 6}, {7, 7}};
  MERGE_TEST(arr1);

  pair_t arr2[] = {{0, 0}, {1, 1}, {6, 2}, {7, 3}, {2, 4}, {3, 5}, {4, 6}, {5, 7}};
  MERGE_TEST(arr2);

  pair_t arr3[] = {{4, 0}, {5, 1}, {6, 2}, {7, 3}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};
  MERGE_TEST(arr3);

  pair_t arr4[] = {{1, 0}, {2, 1}, {4, 2}, {5, 3}, {0, 4}, {3, 5}, {6, 6}, {7, 7}};
  MERGE_TEST(arr4);

  pair_t arr5[] = {{2, 0}, {3, 1}, {5, 2}, {8, 3}, {0, 4}, {1, 5}, {4, 6}, {7, 7}};
  MERGE_TEST(arr5);

  return 0;
}
static TEST(test_merge);

typedef pair_t * map_t;

size_t map_size(map_t map) {
  return map[0].first;
}

size_t *map_cnt(map_t map) {
  return &map[0].second;
}

pair_t *map_elems(map_t map) {
  return &map[1];
}

// called after inserting an element
void map_sort(map_t map) {
  uintptr_t cnt = *map_cnt(map);
  pair_t *elems = map_elems(map);
  if(!cnt || cnt & 1) return;
  uintptr_t x = cnt - 1;
  uintptr_t bit = 1;
  while(x & bit) {
    x &= ~bit;
    bit <<= 1;
    merge(&elems[x], bit);
  }
}

bool map_insert(map_t map, pair_t x) {
  uintptr_t *size = &map[0].first;
  uintptr_t *cnt = &map[0].second;
  if(*cnt >= *size) return false;
  map[++*cnt] = x;
  map_sort(map);
  return true;
}

#define MAP(name, size) pair_t name[size+1] = {{size, 0}}

#define print_map(map) _print_map(#map, map)

void _print_map(char *name, map_t map) {
  size_t size = map_size(map);
  size_t cnt = *map_cnt(map);
  printf("%s[%ld] = ", name, (long)size);
  print_pairs(map_elems(map), cnt);
}

pair_t test_pair(uintptr_t x) {
  pair_t p = {x, x};
  return p;
}

int test_map(UNUSED char *name) {
  MAP(map, 32);
  int elems[] = {2, 5, 8, 3, 1, 0, 4, 7, 6, 9, 10, 15, 13, 11, 12, 14};
  for(size_t i = 0; i < LENGTH(elems); i++) {
    map_insert(map, test_pair(elems[i]));
  }
  print_map(map);
  return 0;
}
static TEST(test_map);
