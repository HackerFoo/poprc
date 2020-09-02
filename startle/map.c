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
#include "startle/stats_types.h"
#include "startle/macros.h"
#include "startle/error.h"
#include "startle/log.h"
#include "startle/support.h"
#include "startle/map.h"
#include "startle/stats.h"

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
  if(a == b) return;
  COUNTER(swap, n);
  while(n--) {
    swap(a++, b++);
  }
}

/** Reverse `n` pairs. */
void reverse(pair_t *a, size_t n) {
  size_t m = n / 2;
  COUNTER(swap, m);
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

size_t isqrt(size_t x) {
  if(x < 2) return x;
  size_t a = x / 2, b;
  b = x / a; a = (a + b) / 2;
  b = x / a; a = (a + b) / 2;
  b = x / a; a = (a + b) / 2;
  b = x / a; a = (a + b) / 2;
  return a;
}

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
void merge(pair_t *arr, pair_t *b, size_t n, cmp_t cmp) {
  if(GET_COUNTER(merge_depth) == 0) {
    COUNTER(merge, n);
  }
#if DEBUG
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
#if DEBUG
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
        size_t swaps;
        if((ptrdiff_t)b_run >= min(b - h, h - q)) {
          // rotate h back to q
          // leave new items on end
          rotate(q, h, b - q);
          swaps = b - q;
          h = q;
          // ==|== q a_left ==|==
          //   ^-- h
        } else {
          // rotate h over new items
          // push b_run
          // swaps: b_run + min(b - h, h - q), avg. b_run + q size / 2
          if(b - h < h - q) {
            // move right side
            rotate(h, b, b_run + (b - h));
            swaps = b_run + (b - h);
            h += b_run;
          } else {
            // swap left side
            swap_block(q, b, b_run);
            rotate(q, q + b_run, h - q);
            swaps = b_run + h - q;
          }

          // ==|== q_left a_left q_right ==|==
          //                    ^-- h
        }
        b += b_run;
        a += b_run;

        // re-balance
        if(swaps > (1 << 7) &&
           swaps > isqrt(b - q) / 2) {

          rotate(q, h, b - q);
          h = q;

          COUNTER(merge_depth, 1);
          merge(q, b, end - q, cmp);
          b = q;
          COUNTER(merge_depth, -1);
          COUNTER_MAX(merge_depth_max, GET_COUNTER(merge_depth));
        }
      } else { // b >= h
        swap(a++, h++);
        COUNTER(swap, 1);
        if(h >= b) { // queue wrap around
          h = q;
        }
      }
    } else {
      // empty queue
      if(b < end && cmp(b->first, a->first)) {
        swap(a++, b++);
        COUNTER(swap, 1);
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
bool key_cmp(uintptr_t a, uintptr_t b) {
  return a < b;
}

/** String key comparison. */
static
bool string_cmp(uintptr_t a, uintptr_t b) {
  return strcmp((char *)a, (char *)b) < 0;
}

void merge_left_short(pair_t *a, pair_t *b, size_t n, cmp_t cmp) {
  pair_t *end = a + n;
  while(a < b && b < end) {
    pair_t *x = b;
    while(x < end && cmp(x->first, a->first)) x++;

    // all [b...x) < a[0];
    rotate(a, b, x - a);
    a += x - b + 1;
    b = x;
  }
}

TEST(merge_left_short) {
  pair_t arr[] =
    {{1,1}, {3,3}, {6,6},
     {0,0}, {2,2}, {4,4},
     {5,5}, {7,7}, {8,8}};
  merge_left_short(arr, arr + 3, 9, key_cmp);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

void merge_right_short(pair_t *a, pair_t *b, size_t n, cmp_t cmp) {
  if(n <= 1 || a >= b) return;
  pair_t *end = a + n;
  while(a < b && b < end) {
    pair_t *x = b, *last = end - 1;
    while(x > a && cmp(last->first, x[-1].first)) x--;

    // all [x...b) > last
    rotate(x, b, end - x);
    size_t r = b - x;
    b -= r;
    end -= r + 1;
  }
}

TEST(merge_right_short) {
  pair_t arr[] =
    {{1, 1}, {2, 2}, {3, 3},
     {4, 4}, {5, 5}, {6, 6},
     {0, 0}, {7, 7}, {8, 8}};
  merge_right_short(arr, arr + 6, 9, key_cmp);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

#define NEXT(x, r)                              \
  ({                                            \
    LET(__px, &(x));                            \
    LET(__x, *__px);                            \
    LET(__xn, __x + 1);                         \
    *__px = __xn < r##_end ? __xn: r;           \
    __x;                                        \
  })

// preserves, but scrambles, the buffer
void merge_with_buffer(pair_t *a, pair_t *b, size_t n, cmp_t cmp, pair_t *buf) {
  pair_t
    *const a_end = b,
    *const b_end = a + n,
    *const buf_end = buf + min(a_end - a, b_end - b),
    *h = buf,
    *t = buf;
  size_t buf_n = 0;
  while(a < a_end && b < b_end) {
    if(!buf_n || cmp(b->first, t->first)) {
      if(cmp(b->first, a->first)) {
        swap(a, b);
        if(b < b_end - 1) {
          swap(b++, NEXT(h, buf)); // b
          buf_n++;
        }
      }
    } else { // h > t
      if(!cmp(a->first, t->first)) {
        swap(a, t);
        swap(NEXT(h, buf), NEXT(t, buf));
      }
    }
    a++;
  }
  if(a == a_end) {
    while(b < b_end) {
      if(buf_n && !cmp(b->first, t->first)) {
        swap(a, NEXT(t, buf));
        buf_n--;
      } else {
        swap(a, b++);
      }
      a++;
    }
  } else {
    while(a < a_end) {
      if(buf_n && !cmp(a->first, t->first)) {
        swap(a, t);
        swap(NEXT(h, buf), NEXT(t, buf));
      }
      a++;
    }
  }
  while(buf_n) {
    swap(a++, NEXT(t, buf));
    buf_n--;
  }
}

TEST(merge_with_buffer) {
  pair_t arr[] =
    {{0,0}, {4,4}, {8,8},
     {1,1}, {2,2}, {3,3},
     {5,5}, {6,6}, {7,7}};
  pair_t buf[3];
  memset(buf, -1, sizeof(buf));
  merge_with_buffer(arr, arr+3, 9, key_cmp, buf);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

TEST(merge_with_buffer2) {
  pair_t arr[] = {{11, 11}, {3, 3}, {8, 8}, {10, 10}};
  pair_t buf[4];
  memset(buf, -1, sizeof(buf));
  merge_with_buffer(arr, arr+1, 4, key_cmp, buf);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

// on return, *pa points to unmerged elements, and *pbuffer points to the location of the buffer
// returns number of merged elements
size_t merge_into_buffer(pair_t **pbuffer, pair_t **pa, pair_t *b, size_t n, cmp_t cmp) {
  pair_t
    *buffer = *pbuffer,
    *a = *pa;
  size_t
    buffer_n = a - buffer,
    a_n = b - a,
    b_n = n - buffer_n - a_n;

  assert_ge(buffer_n, min(a_n, b_n));
  while(a_n) {
    if(b_n && cmp(b->first, a->first)) {
      swap(buffer++, b++);
      b_n--;
    } else {
      swap(buffer++, a++);
      a_n--;
    }
  }
  *pbuffer = buffer;
  *pa = b;
  return n - b_n - buffer_n;
}

TEST(merge_into_buffer) {
  pair_t arr[20];
  zero(arr);
  pair_t
    *buffer = arr,
    *a = buffer + 5,
    *b = a + 10,
    *end = arr + LENGTH(arr);
  test_pairs(a, 15, 10, 42);
  size_t n = merge_into_buffer(&buffer, &a, b, LENGTH(arr), key_cmp);
  if(!is_ordered(arr, n, key_cmp)) return -1;
  if(!is_ordered(a, end - a, key_cmp)) return -2;
  return 0;
}

// overwrites the buffer
void merge_with_buffer_fast(pair_t *a, pair_t *b, size_t n, cmp_t cmp, pair_t *buf) {
  pair_t
    *const a_end = b,
    *const b_end = a + n,
    *const buf_end = buf + min(a_end - a, b_end - b),
    *h = buf,
    *t = buf;
  while(a < a_end && b < b_end) {
    if(h == t || cmp(b->first, t->first)) {
      if(cmp(b->first, a->first)) {
        if(b < b_end - 1) {
          *NEXT(h, buf) = *a;
          *a = *b++;
        } else {
          swap(a, b);
        }
      }
    } else { // h > t
      if(!cmp(a->first, t->first)) {
        *NEXT(h, buf) = *a;
        *a = *NEXT(t, buf);
      }
    }
    a++;
  }
  if(a == a_end) {
    while(b < b_end) {
      if(h != t && !cmp(b->first, t->first)) {
        *a = *NEXT(t, buf);
      } else {
        *a = *b++;
      }
      a++;
    }
  } else {
    while(a < a_end) {
      if(h != t && !cmp(a->first, t->first)) {
        *NEXT(h, buf) = *a;
        *a = *NEXT(t, buf);
      }
      a++;
    }
  }
  while(t != h) {
    *a++ = *NEXT(t, buf);
  }
}

TEST(merge_with_buffer_fast) {
  pair_t arr[] =
    {{2, 2}, {4, 4}, {5, 5},
     {6, 6}, {7, 7}, {8, 8},
     {0, 0}, {1, 1}, {3, 3}};
  pair_t buf[3];
  memset(buf, -1, sizeof(buf));
  merge_with_buffer_fast(arr, arr+6, 9, key_cmp, buf);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

void bubble_sort(pair_t *arr, size_t n, cmp_t cmp) {
  COUNTUP(i, n-1) {
    COUNTUP(j, n-i-1) {
      if(cmp(arr[j+1].first, arr[j].first)) {
        swap(&arr[j], &arr[j+1]);
      }
    }
  }
}

bool is_ordered(pair_t *arr, size_t n, cmp_t cmp) {
  COUNTUP(i, n-1) {
    if(cmp(arr[i+1].first, arr[i].first)) {
      return false;
    }
  }
  return true;
}

#define BLOCK(i) min(n, bs * (i))
#define BLOCK_LAST(i) (BLOCK((i) + 1) - 1)
#define CHECK_BLOCKS(name)                                      \
  do {                                                          \
    COUNTUP(i, n_blocks) {                                      \
      pair_t *bl = &a[BLOCK(i)];                                \
      if(abs(bl - buffer) < bs) continue;                       \
      on_assert_error(is_ordered(bl, bs, cmp)) {                \
        printf("OUT OF ORDER @ " #name " BLOCK(%d): ", i);      \
        print_pairs(bl, bs);                                    \
      }                                                         \
    }                                                           \
  } while(0)

// Huang, Bing-Chao & Langston, Michael. (1988). Practical In-Place Merging.
// Communications of the ACM. 31. 348-352. 10.1145/42392.42403.
// http://www.akira.ruc.dk/~keld/teaching/algoritmedesign_f04/Artikler/04/Huang88.pdf
// NOTE: slower than merge
void merge_huang88(pair_t *arr, pair_t *b, size_t n, cmp_t cmp) {
  size_t
    bs = isqrt(n),
    offset = (b - arr) % bs,
    n_blocks = (n - offset) / bs,
    tail = n - (bs * n_blocks) - offset;
  pair_t
    *end = arr + n,
    *a = arr + offset; // first full block

  if(b - arr < (ptrdiff_t)(bs + offset)) {
    merge_left_short(arr, b, n, cmp);
    return;
  }

  if(end - b < (ptrdiff_t)bs) {
    merge_right_short(arr, b, n, cmp);
    return;
  }

  // find top bs elements
  pair_t *top_a = b, *top_b = end;
  LOOP(bs) {
    if(cmp(top_a[-1].first, top_b[-1].first)) {
      top_b--;
    } else {
      top_a--;
    }
  }

  // handle last block
  // swap top_b into last block of a (buffer)
  pair_t *buffer = b - bs;
  swap_block(buffer, top_b, end - top_b);

  // merge the last few elements
  merge_with_buffer(end - bs - tail, top_b, bs + tail, cmp, buffer);

  // handle first block
  if(offset) {
    pair_t *p = buffer + (bs - offset);
    swap_block(arr, p, offset);
    merge_with_buffer(p, b, bs + offset, cmp, arr);
    swap_block(arr, p, offset);
  }

  // move buffer to the first block
  swap_block(&a[BLOCK(0)], buffer, bs);
  buffer = &a[BLOCK(0)];

  // selection sort on sqrt(n) blocks of size sqrt(n)
  // comparing the last item in the block
  RANGEUP(i, 1, n_blocks) {
    // find minimum in remaining blocks
    size_t min = i;
    uintptr_t min_key = a[BLOCK_LAST(min)].first;
    RANGEUP(j, i + 1, n_blocks) {
      uintptr_t key = a[BLOCK_LAST(j)].first;
      if(cmp(key, min_key)) {
        min = j;
        min_key = key;
      }
    }
    if(min > i) swap_block(&a[BLOCK(i)], &a[BLOCK(min)], bs);
  }

  pair_t *p = &a[BLOCK(1)];
  RANGEUP(i, 2, n_blocks + !!tail) {
    pair_t *b = &a[BLOCK(i)];
    size_t size = min((ptrdiff_t)bs, end - b);
    if(cmp(b->first, b[-1].first)) {
      merge_into_buffer(&buffer, &p, b, (b + size) - buffer, cmp);
    }
  }

  bubble_sort(buffer, bs, cmp);
  rotate(buffer, p, end - buffer);
  assert_error(is_ordered(arr, n, cmp));
}

void test_pairs(pair_t *arr, size_t n, size_t a, unsigned int s) {
  const uint32_t k = (1ul << 31) - 1;
  COUNTUP(i, n) arr[i] = (pair_t) {i, i};
  COUNTUP(i, n - 1) { // shuffle
    s = (s * k) % (n - i);
    swap(&arr[i], &arr[s]);
  }
  quicksort(arr, WIDTH(arr), a);
  quicksort(arr + a, WIDTH(arr), n - a);
}

TEST(merge_huang88) {
  pair_t arr[16];
  const int b = 7;
  test_pairs(arr, LENGTH(arr), b, 0);
  merge_huang88(arr, arr + b, LENGTH(arr), key_cmp);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
}

TEST(merge_huang88b) {
  pair_t arr[] = {{16, 16},
    {17, 17}, {18, 18}, {19, 19}, {20, 20}, {21, 21},
    {22, 22}, {23, 23}, {24, 24}, {25, 25}, {26, 26},
    {27, 27}, {28, 28}, {29, 29}, {30, 30}, {31, 31},
    {0, 0},   {1, 1},   {2, 2},   {3, 3},   {4, 4},
    {5, 5},   {6, 6},   {7, 7},   {8, 8},   {9, 9},
    {10, 10}, {11, 11}, {12, 12}, {13, 13}, {14, 14},
    {15, 15}};
  const int b = 16;
  merge_huang88(arr, arr + b, LENGTH(arr), key_cmp);
  if(!is_ordered(arr, LENGTH(arr), key_cmp)) return -1;
  return 0;
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
  pair_t *buf = &elems[cnt];
  size_t buf_size = map_size(map) - cnt;
  if(!cnt || cnt & 1) return;
  uintptr_t x = (cnt - 1) & ~(bit - 1);
  while(x & bit) {
    x &= ~bit;
    if(bit <= buf_size) {
      merge_with_buffer_fast(&elems[x], &elems[x+bit], bit << 1, cmp, buf);
    } else {
      merge(&elems[x], &elems[x+bit], bit << 1, cmp);
    }
    bit <<= 1;
  }
}

static
void _map_sort_full(map_t map, cmp_t cmp) {
  uintptr_t
    cnt = *map_cnt(map),
    x = cnt & ~(1 << __builtin_ctz(cnt));
  pair_t *elems = map_elems(map);
  pair_t *buf = &elems[cnt];
  size_t buf_size = map_size(map) - cnt;
  while(x) {
    uintptr_t y = x & ~(1 << __builtin_ctz(x));
    if(min(x - y, cnt - x) <= buf_size) {
      merge_with_buffer_fast(&elems[y], &elems[x], cnt - y, cmp, buf);
    } else {
      merge(&elems[y], &elems[x], cnt - y, cmp);
    }
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
  map_iterator it = map_iterator_begin(map, key);
  return map_find_iter(&it);
}

#if INTERFACE
typedef struct {
  pair_t *elems;
  uintptr_t key, x, bit, a;
  size_t est;
} map_iterator;
#endif

map_iterator map_iterator_begin(map_t map, uintptr_t key) {
  map_iterator it;
  it.elems = map_elems(map);
  it.key = key;
  it.x = *map_cnt(map);
  it.bit = 1ll << int_log2(it.x);
  it.a = 0;
  it.est = it.bit >> 1;
  return it;
}

pair_t *map_find_iter(map_iterator *it) {
  pair_t *result = NULL;
  while(it->x && !result) {
    if(it->x & it->bit) {
      it->x &= ~it->bit;
      result = find_last(&it->elems[it->a], it->bit, it->key, &it->est);
      it->a += it->bit;
    }
    it->est >>= 1;
    it->bit >>= 1;
  }
  return result;
}

pair_t *map_next(map_iterator *it, pair_t *prev) {
  if(prev &&
     prev >= &it->elems[it->a - (it->bit << 1)] &&
     prev[-1].first == it->key) {
    return &prev[-1];
  } else {
    return map_find_iter(it);
  }
}

TEST(map_iterate) {
  MAP(map, 32);
  int key[] = {1, 2, 1, 2, 3, 3, 1};
  FOREACH(i, key) {
    pair_t p = {key[i], i};
    map_insert(map, p);
  }
  print_map(map);
  int count = 0;
  COUNTUP(key, 4) {
    printf("matches for key = %d:", (int)key);
    map_iterator it = map_iterator_begin(map, key);
    pair_t *p = map_find_iter(&it);
    while(p) {
      if(p->first != key) {
        printf("key mismatch!\n");
        return -1;
      }
      printf(" %d", (int)p->second);
      count++;
      p = map_next(&it, p);
    }
    printf("\n");
  }
  if(count != LENGTH(key)) {
    printf("wrong count\n");
    return -2;
  }
  return 0;
}

pair_t *map_find_recent(map_t map, uintptr_t key) {
  uintptr_t x = *map_cnt(map);
  pair_t *elems = map_elems(map);
  pair_t *result = NULL;
  uintptr_t bit = 1;
  size_t est = 1;
  while(x) {
    if(x & bit) {
      x &= ~bit;
      if((result = find_last(&elems[x], bit, key, &est))) break;
    }
    est = (est << 1) + 1;
    bit <<= 1;
  }
  return result;
}

pair_t *map_find_sorted(map_t map, uintptr_t key) {
  return find_last(map_elems(map), *map_cnt(map), key, NULL);
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

map_t alloc_map(uintptr_t size) {
  map_t map = (map_t)malloc(sizeof(pair_t) * (size + 1));
  map[0] = (pair_t) {size, 0};
  return map;
}

map_t init_map(pair_t *mem, size_t n) {
  mem[0] = (pair_t) {n - 1, 0};
  return mem;
}
