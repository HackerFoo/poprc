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
#include <string.h>
#include <inttypes.h>
#include <stdbool.h>

#include "startle/types.h"
#include "startle/macros.h"
#include "startle/support.h"
#include "startle/error.h"
#include "startle/test.h"
#include "startle/log.h"

/** @file
 *  @brief Unit testing
 */

typedef struct test_entry {
  char *name;
  int (*run)();
} test_entry_t;

#define TEST__ITEM(file, line, name) extern int test_##name();
#include "test_list.h"
#undef TEST__ITEM

#define TEST__ITEM(_file, _line, _name)         \
  {                             \
    .name = #_name,             \
    .run = &test_##_name        \
  },

static test_entry_t tests[] = {
#include "test_list.h"
};

#undef TEST__ITEM

/** Run all tests matching the name. */
int run_test(seg_t name) {
  int fail = 0;
  FOREACH(i, tests) {
    test_entry_t *entry = &tests[i];
    if(strncmp(entry->name, name.s, name.n) == 0) {
      printf("@ %s\n", entry->name);
      int result = entry->run();
      printf("%s => %d\n", entry->name, result);
      if(result && !fail) fail = result;
    }
  }
  return fail;
}

// Macro tests

TEST(loops) {
  /** [loops] */
  COUNTUP(i, 3) {
    printf("up: %d\n", (unsigned int)i);
  }
  COUNTDOWN(i, 3) {
    printf("down: %d\n", (unsigned int)i);
  }
  COUNTUP(i, 0) {
    printf("down: shouldn't print this\n");
  }
  COUNTDOWN(i, 0) {
    printf("down: shouldn't print this\n");
  }

  unsigned int arr[] = {1, 4, 9};
  FOREACH(i, arr) {
    printf("arr[%d] = %d\n", (unsigned int)i, arr[i]);
  }
  LOOP(3) {
    LOOP(3) {
      putchar('x');
    }
    putchar('\n');
  }

  RANGEUP(i, 3, 7) {
    printf("range up: i = %d, REVI(i) = %d\n", (int)i, (int)REVI(i));
  }
  RANGEDOWN(i, 3, 7) {
    printf("range down: i = %d, REVI(i) = %d\n", (int)i, (int)REVI(i));
  }
  /** [loops] */
  return 0;
}

TEST(formask) {
  unsigned int mask = 0x11ae;
  unsigned int prev_mask = (mask << 1) + 1;
  FORMASK(i, j, mask) {
    printf("%d, %d\n", (int)i, (int)j);
    if((prev_mask - (__mask << (__z + 1))) != 1) return -1;
    prev_mask = __mask;
  }
  return 0;
}

TEST(next_bit) {
  uintptr_t mask = 0x11af, m = mask;
  while(m) {
    int x = next_bit(&m);
    if(x < 0) return -1;
    mask &= ~(1 << x);
    printf("bit = %d\n", x);
  }
  return mask ? -2 : 0;
}

/** [macro_dispatch] */

#define TEST_0() printf("TEST_0()\n")
#define TEST_1(x0) printf("TEST_1(" x0 ")\n")
#define TEST_2(x0, x1) printf("TEST_2(" x0 ", " x1 ")\n")

TEST(macro_dispatch) {
  DISPATCH(TEST);
  DISPATCH(TEST, "1");
  DISPATCH(TEST, "1", "2");
  return 0;
}

/** [macro_dispatch] */

TEST(inrange) {
  if(!INRANGE(1, -3, 3)) return -1;
  if(!INRANGE(12, -3, 3, 10, 20)) return -2;
  if(!INRANGE(42, -3, 3, 10, 20, 40, 100)) return -3;
  return 0;
}

TEST(oneof) {
  if(!ONEOF(1, 1)) return -1;
  if(!ONEOF(2, 1, 2)) return -2;
  if(!ONEOF(3, 1, 2, 3)) return -3;
  return 0;
}

static int shadow_x = 1;
TEST(shadow) {
  if(shadow_x != 1) return -1;
  SHADOW(shadow_x) {
    shadow_x = 2;
    if(shadow_x != 2) return -2;
    SHADOW(shadow_x, 3)
      if(shadow_x != 3) return -3;
    if(shadow_x != 2) return -4;
  }
  if(shadow_x != 1) return -5;
  return 0;
}

TEST(macro_math) {
  if(min(3, 4) != 3) return -1;
  if(max(3, 4) != 4) return -2;
  if(DIV_UP(4, 3) != 2) return -3;
  if(DIV_UP(3, 3) != 1) return -4;
  if(csub(3, 4) != 0) return -5;
  if(csub(4, 3) != 1) return -6;
  if(SNAP_UP(1, 3) != 3) return -7;
  if(SNAP_UP(3, 3) != 3) return -8;
  return 0;
}

TEST(for_mask) {
  int count = 0;
  int prev = -1;
  FOR_MASK(i, 0xa569) {
    if(i == prev) return -1;
    if(i & ~__mask) return -2;
    prev = i;
    count++;
  }
  if(count != 256) return -3;
  return 0;
}
