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

#define TEST_ITEM(name) extern int test_##name();
#include "test_list.h"
#undef TEST_ITEM

#define TEST_ITEM(name)                                  \
  {                                                      \
    .first = (uintptr_t)#name,                           \
    .second = (uintptr_t)&test_##name                    \
  },

pair_t tests[] = {
#include "test_list.h"
};

#undef TEST_ITEM

/** Run all tests matching the name. */
int run_test(seg_t name) {
  int fail = 0;
  FOREACH(i, tests) {
    pair_t *entry = &tests[i];
    char *entry_name = (char *)entry->first;
    int (*entry_func)() = (int (*)())entry->second;
    if(strncmp(entry_name, name.s, name.n) == 0) {
      printf("@ %s\n", entry_name);
      int result = entry_func();
      printf("%s => %d\n", entry_name, result);
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
  /** [loops] */
  return 0;
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
