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

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <inttypes.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/map.h"

/** @file
 *  @brief Zero space overhead maps
 */

#if INTERFACE

/** A map for key-value pairs. */
typedef pair_t * map_t;

/** Declare a map to hold `size` key-value pairs. */
#define MAP(name, size) pair_t name[(size) + 1] = {{(size), 0}}

/** A key comparison function. */
typedef bool (cmp_t)(uintptr_t, uintptr_t);

#endif

#define DEBUG 0

/** Scan for a pair with a lesser key. */
static
size_t scan(pair_t *start, pair_t *end, uintptr_t key, cmp_t cmp) {
  size_t cnt = 0;
  pair_t *x = start;
  while(x < end && cmp(x->first, key)) {
    ++cnt;
    ++x;
  }
  return cnt;
}

/** Swap two non-overlapping blocks of `n` pairs. */
void swap_block(pair_t *a, pair_t *b, size_t n) {
  while(n--) {
    swap(a++, b++);
  }
}

/** Reverse `n` pairs. */
void reverse(pair_t *a, size_t n) {
  size_t m = n / 2;
  pair_t *b = a + n;
  while(m--) swap(a++, --b);
}

/** O(n) in-place rotation by `n` pairs. */
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
    assert_error(a <= b);
    assert_error(b <= q);
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

#if(DEBUG)
static
bool is_ordered(pair_t *x, size_t r, size_t n, cmp_t cmp) {
  if(n <= 1) return true;
  r = r % n;
  uintptr_t prev = x[r].first;
  RANGEUP(i, r + 1, n) {
    uintptr_t next = x[i].first;
    if(cmp(next, prev)) return false;
    prev = next;
  }
  COUNTUP(i, r) {
    uintptr_t next = x[i].first;
    if(cmp(next, prev)) return false;
    prev = next;
  }
  return true;
}
#endif

/** Merge an array of two sorted sequences of pairs into one.
 * @param arr the array
 * @b pointer to second sequence
 * @n number of pairs in the array
 * @param cmp key comparison function.
 */
static
void merge(pair_t *arr, pair_t *b, size_t n, cmp_t cmp) {
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
    printf("a[%" PRIuPTR "]: ", a-arr);
    print_pairs(a, q-a);
    printf("q[%" PRIuPTR ", %" PRIuPTR "]: ", q-arr, h-q);
    print_pairs(q, b-q);
    printf("b[%" PRIuPTR "]: ", b-arr);
    print_pairs(b, n-(b-arr));
    assert_error(is_ordered(a, 0, q-a, cmp));
    assert_error(is_ordered(q, h-q, b-q, cmp));
    assert_error(is_ordered(b, 0, n-(b-arr), cmp));
#endif
    // non-empty queue
    if(q < b) {
      // count b < h
      // limit the scan to the size of a
      size_t b_run = scan(b, min(b + (q - a), end), h->first, cmp);
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
      if(b < end && cmp(b->first, a->first)) {
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

/** Direct key comparison. */
static
bool key_cmp(uintptr_t a, uintptr_t b) {
  return a < b;
}

/** String key comparison. */
static
bool string_cmp(uintptr_t a, uintptr_t b) {
  return strcmp((char *)a, (char *)b) < 0;
}

#define MERGE_TEST(arr)                                                 \
  do {                                                                  \
    merge((arr), (arr) + (LENGTH(arr) >> 1), LENGTH(arr), key_cmp);     \
    printf(#arr ": ");                                                  \
    print_pairs((arr), LENGTH(arr));                                    \
  } while(0)


TEST(merge) {
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

  pair_t arr8[] = {{0, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}, {6, 0}, {9, 0}, {12, 0},
                   {5, 1}, {7, 1}, {8, 1}, {10, 1}, {11, 1}, {13, 1}, {14, 1}, {15, 1}};
  MERGE_TEST(arr8);

  return 0;
}

/** Return number of entries that can be stored in a map. */
size_t map_size(map_t map) {
  return map[0].first;
}

/** Return number of entries currently stored in a map. */
size_t *map_cnt(map_t map) {
  return &map[0].second;
}

/** Clear all entries from the map. */
void map_clear(map_t map) {
  *map_cnt(map) = 0;
}

pair_t *map_elems(map_t map) {
  return &map[1];
}

// called after inserting an element
static
void map_sort(map_t map, uintptr_t bit, cmp_t cmp) {
  uintptr_t cnt = *map_cnt(map);
  pair_t *elems = map_elems(map);
  if(!cnt || cnt & 1) return;
  uintptr_t x = (cnt - 1) & ~(bit - 1);
  while(x & bit) {
    x &= ~bit;
    merge(&elems[x], &elems[x+bit], bit << 1, cmp);
    bit <<= 1;
  }
}

static
void _map_sort_full(map_t map, cmp_t cmp) {
  uintptr_t
    cnt = *map_cnt(map),
    x = cnt & ~(1 << __builtin_ctz(cnt));
  pair_t *elems = map_elems(map);
  while(x) {
    uintptr_t y = x & ~(1 << __builtin_ctz(x));
    merge(&elems[y], &elems[x], cnt - y, cmp);
    x = y;
  }
}

/** Fully sort map.
 * Takes O(log n) time.
 * Lookup will be O(log n) afterwards.
 */
void map_sort_full(map_t map) {
  _map_sort_full(map, key_cmp);
}

/** Fully sort a map with string keys. */
void string_map_sort_full(map_t map) {
  _map_sort_full(map, string_cmp);
}

TEST(map_sort_full) {
  MAP(a, 21);
  *map_cnt(a) = 21;
  COUNTUP(i, 16) {
    pair_t *p = &a[i + 1];
    p->first = i;
    p->second = 0;
  }
  COUNTUP(i, 4) {
    pair_t *p = &a[i + 17];
    p->first = 2 * i;
    p->second = 1;
  }
  pair_t *p = &a[21];
  p->first = 0;
  p->second = 2;
  map_sort_full(a);
  print_map(a);
  return 0;
}

static
bool _map_insert(map_t map, pair_t x, cmp_t cmp) {
  uintptr_t *size = &map[0].first;
  uintptr_t *cnt = &map[0].second;
  if(*cnt >= *size) return false;
  map[++*cnt] = x;
  map_sort(map, 1, cmp);
  return true;
}

/** Insert an entry into a map.
 * O(log n) average time, up to O(n).
 * @return true if inserted
 * @snippet map.c map
 */
bool map_insert(map_t map, pair_t x) {
  return _map_insert(map, x, key_cmp);
}

/** Insert an entry into a string map. */
bool string_map_insert(map_t map, pair_t x) {
  return _map_insert(map, x, string_cmp);
}

static
bool _map_merge(map_t map, pair_t *x, size_t n, cmp_t cmp) {
  if(n == 0) return true;
  uintptr_t *size = &map[0].first;
  uintptr_t *cnt = &map[0].second;
  pair_t *elems = map_elems(map);
  assert_error(check_map(map, cmp));
  assert_error(check_order(x, n, cmp));
  if(*cnt + n > *size) return false;
  if(*cnt == 0) {
      memcpy(elems, x, n * sizeof(pair_t));
      *cnt = n;
      return true;
  }
  uintptr_t bit = 1;
  while(n) {
    uintptr_t m = 1 << __builtin_ctz(*cnt);
    if(m > n) {
      memcpy(&elems[*cnt], x, n * sizeof(pair_t));
      *cnt += n;
      break;
    }
    memcpy(&elems[*cnt], x, m * sizeof(pair_t));
    x += m;
    n -= m;
    *cnt += m;
    map_sort(map, bit, cmp);
    bit = m; // avoid redundant sorting
  }
  assert_error(check_map(map, cmp));
  return true;
}

/** Merge `n` sorted entries at `x` into the map. */
bool map_merge(map_t map, pair_t *x, size_t n) {
  return _map_merge(map, x, n, key_cmp);
}

/** Merge `n` sorted string entries at `x` into the map. */
bool string_map_merge(map_t map, pair_t *x, size_t n) {
  return _map_merge(map, x, n, string_cmp);
}

bool _map_union(map_t a, map_t b, cmp_t cmp) {
  uintptr_t
    na = *map_cnt(a),
    nb = *map_cnt(b), x = nb;
  if(nb == 0) return true;
  if(na + nb > map_size(a)) return false;

  pair_t *bs = map_elems(b);
  uintptr_t bit = (uintptr_t)1 << int_log2l(x + 1);
  while(x) {
    if(x & bit) {
      _map_merge(a, bs, bit, cmp);
      bs += bit;
      x &= ~bit;
    }
    bit >>= 1;
  }
  assert_error(check_map(a, cmp));
  return true;
}

/** Merge both maps into `a`. */
bool map_union(map_t a, map_t b) {
  return _map_union(a, b, key_cmp);
}

/** Merge both string maps into `a`. */
bool string_map_union(map_t a, map_t b) {
  return _map_union(a, b, string_cmp);
}

TEST(map_union) {
  COUNTUP(n, 9) {
    MAP(a, 16);
    MAP(b, 8);
    COUNTUP(i, 16-n) {
      pair_t
        pa = {i*2, 0},
        pb = {i*2 + 1, 1};
      map_insert(a, pa);
      if(i < n) map_insert(b, pb);
    }
    map_union(a, b);
    print_map(a);
  }
  return 0;
}

static
bool _map_replace_insert(map_t map, pair_t x, cmp_t cmp) {
  pair_t *p = map_find(map, x.first);
  if(p) {
    p->second = x.second;
    return true;
  } else {
    return _map_insert(map, x, cmp);
  }
}

TEST(map_merge) {
  pair_t map[17] = {
    {LENGTH(map) - 1, 5},
    {0,0}, {4,0}, {12, 0}, {16, 0},
              {8, 1}};
  pair_t x[11];
  FOREACH(i, x) {
    x[i].first = i * 2 + 1;
    x[i].second = 10 + 10 * i;
  }
  map_merge(map, x, LENGTH(x));
  print_map(map);
  pair_t *r = map_find(map, 16);
  if(r) {
    printf("r = {%d, %d}\n", (int)r->first, (int)r->second);
  } else {
    printf("r = NULL\n");
  }
  return 0;
}

/** Insert into a map, replacing any entry with the same key. */
bool map_replace_insert(map_t map, pair_t x) {
  return _map_replace_insert(map, x, key_cmp);
}

/** Insert into a string map, replacing any entry with the same key. */
bool string_map_replace_insert(map_t map, pair_t x) {
  return _map_replace_insert(map, x, string_cmp);
}

/** Look up an entry in a map.
 * O((log n)^2) time
 * O(log n) time if the map is fully sorted.
 * @see map_sort_full
 * @return a pointer to the entry if found, otherwise NULL.
 */
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

/** Return the value for a key in a map.
 * Raises an error if the key in not found.
 */
uintptr_t map_get(map_t map, uintptr_t key) {
  pair_t *x = map_find(map, key);
  assert_error(x);
  return x->second;
}

/** Look up an entry in a string map.
 * O((log n)^2) time
 * O(log n) time if the map is fully sorted.
 * @see map_sort_full
 * @return pointer to the entry if found, otherwise NULL
 */
pair_t *string_map_find(map_t map, const char *key) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  uintptr_t bit = 1;
  while(x) {
    if(x & bit) {
      x &= ~bit;
      if((result = find_last_string(&elems[x], bit, key))) break;
    }
    bit <<= 1;
  }
  return result;
}

/** Look up an entry in a string segment map.
 * O((log n)^2) time
 * O(log n) time if the map is fully sorted.
 * @see map_sort_full
 * @return pointer to the entry if found, otherwise NULL
 */
pair_t *seg_map_find(map_t map, seg_t key) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  uintptr_t bit = 1;
  while(x) {
    if(x & bit) {
      x &= ~bit;
      if((result = find_last_seg(&elems[x], bit, key))) break;
    }
    bit <<= 1;
  }
  return result;
}

/** Look up a value in a map.
 * O(n) time
 * @return pointer to the value if found, otherwise NULL
 */
pair_t *map_find_value(map_t map, uintptr_t value) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  COUNTUP(i, x) {
    if(elems[i].second == value) {
      result = &elems[i];
      break;
    }
  }
  return result;
}

#if INTERFACE
/** Print a map. */
#define print_map(map) _print_map(#map, map)
#endif

void _print_map(char *name, map_t map) {
  size_t cnt = *map_cnt(map);
  printf("%s[%" PRIuPTR "] = ", name ? name : "map", cnt);
  print_pairs(map_elems(map), cnt);
}

#if INTERFACE
/** Print a string map. */
#define print_string_map(map) _print_string_map(#map, map)
#endif

void _print_string_map(char *name, map_t map) {
  size_t cnt = *map_cnt(map);
  printf("%s[%" PRIuPTR "] = ", name ? name : "map", cnt);
  print_string_pairs(map_elems(map), cnt);
}

#define find_test(map, key) _find_test((map), #map, (key))

pair_t *_find_test(map_t map, char *name, uintptr_t key) {
  pair_t *p = map_find(map, key);
  printf("map_find(%s, %" PRIuPTR ") = ", name, key);
  if(p) {
    printf("{%" PRIuPTR ", %" PRIuPTR "}\n", p->first, p->second);
  } else {
    printf("NULL\n");
  }
  return p;
}

TEST(map) {
  /** [map] */
  int ret = 0;
  MAP(map, 32);
  int elems[] = {2, 5, 8, 3, 1, 0, 4, 7, 6, 9, 10, 15, 13, 11, 12};
  FOREACH(i, elems) {
    pair_t p = {elems[i], i};
    map_insert(map, p);
  }
  print_map(map);
  /** [map] */

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

  if(!check_map(map, key_cmp)) ret = -2;
  return ret;
}

static bool expect(map_t map, uintptr_t key, uintptr_t x) {
  pair_t *p = map_find(map, key);
  if(!p) {
    printf("expect(%" PRIuPTR ", %" PRIuPTR "): missing\n", key, x);
    return false;
  } else if(p->second != x) {
    printf("expect(%" PRIuPTR ", %" PRIuPTR "): %" PRIuPTR "\n", key, x, p->second);
    return false;
  }
  return true;
}

TEST(map_stack_behavior) {
  #define M 2
  #define N 8
  MAP(map, (M * N));
  COUNTUP(i, M) {
    COUNTUP(j, N) {
      pair_t x = {j, i};
      map_insert(map, x);
    }
    COUNTUP(j, N) {
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

TEST(rotate) {
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

/*
// TEST(rotate_speed) {
  size_t cnt = 1 << 28;
  pair_t *arr = calloc(cnt, sizeof(pair_t));
  rotate(arr, arr + (1 << 27) - 1, cnt);
  free(arr);
  return 0;
}
*/

TEST(string_map) {
  char *strings[] = {
    "one",
    "two",
    "three",
    "abc",
    "cow",
    "bat",
    "cheese",
    "quilt"
  };
  MAP(map, LENGTH(strings));
  FOREACH(i, strings) {
    pair_t x = {(uintptr_t)strings[i], i};
    string_map_insert(map, x);
  }
  print_string_map(map);

  pair_t *p = string_map_find(map, "cow");
  if(!p) return -1;
  printf("%s => %" PRIuPTR "\n", (char *)p->first, p->second);
  if(p->second != 4) return -1;
  return 0;
}

bool check_order(pair_t *elems, size_t size, cmp_t cmp) {
  if(size <= 1) return true;
  uintptr_t prev = elems[0].first;
  RANGEUP(i, 1, size) {
    uintptr_t x = elems[i].first;
    if(cmp(x, prev)) {
      printf("elems[%d] < prev\n", (int)i);
      return false;
    }
    prev = x;
  }
  return true;
}

bool check_map(map_t map, cmp_t cmp) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  uintptr_t bit = 1;
  while(x) {
    if(x & bit) {
      x &= ~bit;
      if(!check_order(&elems[x], bit, cmp)) return false;
    }
    bit <<= 1;
  }
  return true;
}
