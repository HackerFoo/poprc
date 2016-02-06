#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <stddef.h>
#include "rt_types.h"
#include "gen/support.h"
#include "gen/map.h"
#include <stdlib.h>

#if INTERFACE
typedef pair_t * map_t;
#define MAP(name, size) pair_t name[(size) + 1] = {{(size), 0}}
#endif
#define DEBUG 0

// scans for a pair with a lesser key
size_t scan(pair_t *start, pair_t *end, uintptr_t key) {
  size_t cnt = 0;
  pair_t *x = start;
  while(x < end && x->first < key) {
    ++cnt;
    ++x;
  }
  return cnt;
}

// NOTE: non-overlapping
void swap_block(pair_t *a, pair_t *b, size_t n) {
  while(n--) {
    swap(a++, b++);
  }
}


void reverse(pair_t *a, size_t n) {
  size_t m = n / 2;
  pair_t *b = a + n;
  while(m--) swap(a++, --b);
}

// O(n) inplace rotate
void rotate(pair_t *a, pair_t *b, size_t n) {
  if(a == b) return;
  const size_t m = b - a;
  reverse(a, m);
  reverse(b, n - m);
  reverse(a, n);
}

/*
// alternate implementation
void rotate(pair_t *a, pair_t *b, size_t n) {
  size_t offset = b - a;
  while(offset) {
    pair_t
      *p = b,
      *q = a + n;
    assert(a <= b);
    assert(b <= q);
    while(q > b) {
      swap(--p, --q);
      if(p <= a) p = b;
    }
    if(offset > 1) {
      b = a + (offset - (n % offset));
      n = offset;
      offset = b - a;
    } else break;
  }
}
*/

// arr points to the array of size n
// b is the start of the second sorted section
void merge(pair_t *arr, pair_t *b, size_t n) {
#if(DEBUG)
  printf("merge ");
  print_pairs(arr, n);
#endif
  // |== output ==|== a ==|== q ==|== b ==|
  //                  head--^
  pair_t
    *a = arr, // sorted region a
    *q = b,   // queuing region
    *h = b,   // head (and tail) of the queue
    *end = arr + n;

  while(a < b) {
#if(DEBUG)
    printf("\n");
    print_pairs(arr, a-arr);
    printf("a[%ld]: ", a-arr);
    print_pairs(a, q-a);
    printf("q[%ld, %ld]: ", q-arr, h-q);
    print_pairs(q, b-q);
    printf("b[%ld]: ", b-arr);
    print_pairs(b, n-(b-arr));
#endif
    // non-empty queue
    if(q < b) {
      // count b < h
      size_t b_run = scan(b, end, h->first);
      if(b_run) { // choose minimum copying to preserve queue
        swap_block(a, b, b_run); // swap the run into the output replacing a block of 'a'
        // |== output b_left ==|== a_right ==|== q a_left ==|== b_right ==|
        if((ptrdiff_t)b_run >= h - q) {
          // rotate h back to q
          // leave new items on end
          rotate(q, h, b - q);
          h = q;
          // ==|== q a_left ==|==
          //   ^-- h
        } else {
          // rotate h over new items
          rotate(h, b, b_run + (b - h));
          h += b_run;
          // ==|== q_left a_left q_right ==|==
          //                    ^-- h
        }
        b += b_run;
        a += b_run;
      } else { // b >= h
        swap(a++, h++);
        if(h >= b) { // queue wrap around
          h = q;
        }
      }
    } else {
      // empty queue
      if(b < end && b->first < a->first) {
        swap(a++, b++);
      } else {
        a++;
      }
    }

    // if a runs into the queue, just steal the queue
    if(a >= q) {
      rotate(q, h, b-q);
      a = q;
      q = b;
      h = q;
    }
  }
}

#define MERGE_TEST(arr)                                         \
  do {                                                          \
    merge((arr), (arr) + (LENGTH(arr) >> 1), LENGTH(arr));      \
    printf(#arr ": ");                                          \
    print_pairs((arr), LENGTH(arr));                            \
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

  pair_t arr6[] = {{0, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}, {7, 0},
                   {0, 1}, {1, 1}, {2, 1}, {3, 1}, {4, 1}, {5, 1}, {6, 1}, {7, 1}};
  MERGE_TEST(arr6);

  pair_t arr7[] = {{0, 0}, {2, 0}, {4, 0}, {6, 0}, {8, 0}, {10, 0}, {12, 0}, {14, 0},
                   {1, 0}, {3, 0}, {5, 0}, {7, 0}, {9, 0}, {11, 0}, {13, 0}, {15, 0}};
  MERGE_TEST(arr7);

  return 0;
}
static TEST(test_merge);

size_t map_size(map_t map) {
  return map[0].first;
}

size_t *map_cnt(map_t map) {
  return &map[0].second;
}

void map_clear(map_t map) {
  *map_cnt(map) = 0;
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
    merge(&elems[x], &elems[x+bit], bit << 1);
    bit <<= 1;
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

bool map_replace_insert(map_t map, pair_t x) {
  pair_t *p = map_find(map, x.first);
  if(p) {
    p->second = x.second;
    return true;
  } else {
    return map_insert(map, x);
  }
}

pair_t *map_find(map_t map, uintptr_t key) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  uintptr_t bit = 1;
  while(x) {
    if(x & bit) {
      x &= ~bit;
      if((result = find_last(&elems[x], bit, key))) break;
    }
    bit <<= 1;
  }
  return result;
}

pair_t *map_find_value(map_t map, uintptr_t value) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  for(uintptr_t i = 0; i < x; i++) {
    if(elems[i].second == value) {
      result = &elems[i];
      break;
    }
  }
  return result;
}

#if INTERFACE
#define print_map(map) _print_map(#map, map)
#endif

void _print_map(char *name, map_t map) {
  size_t size = map_size(map);
  size_t cnt = *map_cnt(map);
  printf("%s[%ld] = ", name ? name : "map", (long)size);
  print_pairs(map_elems(map), cnt);
}

#define find_test(map, key) _find_test((map), #map, (key))

pair_t *_find_test(map_t map, char *name, uintptr_t key) {
  pair_t *p = map_find(map, key);
  printf("map_find(%s, %d) = ", name, (int)key);
  if(p) {
    printf("{%d, %d}\n", (int)p->first, (int)p->second);
  } else {
    printf("NULL\n");
  }
  return p;
}

static int test_map(UNUSED char *name) {
  int ret = 0;
  MAP(map, 32);
  int elems[] = {2, 5, 8, 3, 1, 0, 4, 7, 6, 9, 10, 15, 13, 11, 12};
  FOREACH(i, elems) {
    pair_t p = {elems[i], i};
    map_insert(map, p);
  }
  print_map(map);

  FOREACH(i, elems) {
    pair_t *p = find_test(map, elems[i]);
    if(p) {
      if(p->second != i) {
        printf("MISMATCH\n");
        ret = -1;
      }
    } else {
      printf("MISSING\n");
      ret = -1;
    }
  }
  return ret;
}
static TEST(test_map);

static bool expect(map_t map, uintptr_t key, uintptr_t x) {
  pair_t *p = map_find(map, key);
  if(!p) {
    printf("expect(%ld, %ld): missing\n", key, x);
    return false;
  } else if(p->second != x) {
    printf("expect(%ld, %ld): %ld\n", key, x, p->second);
    return false;
  }
  return true;
}

static int test_map_stack_behavior(UNUSED char *name) {
  #define M 2
  #define N 8
  MAP(map, (M * N));
  for(int i = 0; i < M; i++) {
    for(int j = 0; j < N; j++) {
      pair_t x = {j, i};
      map_insert(map, x);
    }
    for(int j = 0; j < N; j++) {
      if(!expect(map, j, i)) {
        print_map(map);
        return -1;
      }
    }
  }
  return 0;
  #undef M
  #undef N
}
static TEST(test_map_stack_behavior);

static int test_rotate(UNUSED char *name) {
  pair_t arr[7];
  FOREACH(i, arr) {
    arr[i].first = i;
    arr[i].second = i;
  };
  pair_t tmp[LENGTH(arr)];

  FOREACH(i, arr) {
    memcpy(tmp, arr, sizeof(arr)); 
    rotate(tmp, tmp+i, LENGTH(tmp));
    print_pairs(tmp, LENGTH(tmp));
  }
  return 0;
}
static TEST(test_rotate);

/*
static int test_rotate_speed(UNUSED char *name) {
  size_t cnt = 1 << 28;
  pair_t *arr = calloc(cnt, sizeof(pair_t));
  rotate(arr, arr + (1 << 27) - 1, cnt);
  free(arr);
  return 0;
}
static TEST(test_rotate_speed);
*/
