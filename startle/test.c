/* Copyright 2012-2017 Dustin DeWeese
   This file is part of PoprC.

    PoprC is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PoprC is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PoprC.  If not, see <http://www.gnu.org/licenses/>.
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
  return 0;
}

#define TEST_2(x0, x1, x2, ...) printf("TEST2(" x0 ", " x1 ", " x2 ")\n")
#define TEST_1(...) printf("TEST1\n")

TEST(macro_dispatch) {
  DISPATCH(TEST, 5, "1", "2", "3");
  DISPATCH(TEST, 5, "1", "2", "3", "4");
  return 0;
}
