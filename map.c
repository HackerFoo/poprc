#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "rt_types.h"
#include "gen/support.h"

void map_merge(pair_t *arr, size_t n) {
  pair_t
    *a = arr,
    *b = arr + (n >> 1),
    *q = b; // queue where the head is q and the tail is b - 1
  size_t qr = 0;

  for(size_t i = 0; i < n; i++, a++) {
    if(q->first < a->first) {
      if(b->first < q->first) { // b < q < a
        goto select_b;
      } else { // q < a, b >= q
        swap(a, q++);
        qr++;
        if(q == b) { // queue ran out of elements after q, un-rotate
          q -= qr;
          qr = 0;
        }
      }
    } else if(b->first < a->first) {
    select_b:
      // `b` moves, increasing the size of the queue
      // put `a` at the tail of the queue
      if(!qr) {
        // no bubble
        swap(a, b);
      } else {
        // remove bubble in the queue
        pair_t _a = *a;
        pair_t * const t = q - 1;
        *a = *b;
        *b = *t;
        memmove(t, t+1, sizeof(*t) * (qr-1));

        // put `a` at the tail
        *t = _a;
      }
      b++;
    }
  }
}

int test_map_merge(UNUSED char *name) {
  pair_t arr[] = {{0, 0}, {2, 1}, {4, 2}, {1, 3}, {3, 4}, {5, 5}};
  map_merge(arr, LENGTH(arr));
  for(size_t i = 0; i < LENGTH(arr); i++) {
    printf("%d\n", (int)arr[i].first);
  }
  return 0;
}
static TEST(test_map_merge);

void map_sort(pair_t *map) {

}

inline
bool map_insert(pair_t *map, pair_t x) {
  uintptr_t *size = &map[0].first;
  uintptr_t *cnt = &map[0].second;
  if(*cnt >= *size) return false;
  map[*cnt++] = x;
  map_sort(map);
  return true;
}
