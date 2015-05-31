/* Copyright 2012-2015 Dustin DeWeese
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

#include "rt_types.h"
#include <stdio.h>
#include <string.h>

#ifdef __MACH__
#include <mach-o/getsect.h>
#endif

#include "linenoise/linenoise.h"
#include "gen/rt.h"
#include "gen/test.h"
#include "gen/eval.h"
#include "gen/primitive.h"

int test_alloc(UNUSED char *name) {
  cell_t *a[30];
  LOOP(50) {
    FOREACH(i, a) {
      a[i] = func(func_add, 9, 1);
    }
    FOREACH(i, a) {
      closure_free(a[i]);
    }
  }
  return check_free() ? 0 : -1;
}
static TEST(test_alloc);

int test_loops(UNUSED char *name) {
  COUNTUP(i, 3) {
    printf("up: %d\n", (unsigned int)i);
  }
  COUNTDOWN(i, 3) {
    printf("down: %d\n", (unsigned int)i);
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
static TEST(test_loops);

bool check_free() {
  bool leak = false;
  FOREACH(i, cells) {
    if(is_closure(&cells[i])) {
      printf("LEAK: %d (%ld)\n", (unsigned int)i, (long int)cells[i].n);
      leak = true;
    }
  }
  FOREACH(i, alt_live) {
    if(alt_live[i]) {
      printf("ALT LEAK: %d (%ld)\n", (unsigned int)i, (long int)alt_live[i]);
      leak = true;
    }
  }
  return !leak;
}

#define MAX_NAME_SIZE 4096
#ifndef __MACH__
extern char __tests_start;
extern char __tests_end;
#endif

int test_run(char *name, void (*logger)(char *name, int result)) {
#ifdef __MACH__
  unsigned long secsize;
  char *section_start = getsectdata("__TEXT", "__tests", &secsize);
  char *section_end = section_start + secsize;
#else
  char *section_start = &__tests_start;
  char *section_end = &__tests_end;
#endif

  int name_size = strnlen(name, MAX_NAME_SIZE);
  int fail = 0;
  for(struct __test_entry *entry = (struct __test_entry *)section_start;
      (char *)entry < section_end;
      entry++) {
    int entry_name_size = strnlen(entry->name, MAX_NAME_SIZE);
    if(strncmp(name, entry->name, min(name_size, entry_name_size)) == 0) {
      int result = entry->func(name);
      if((uintptr_t)logger > 1) logger(entry->name, result);
      if(result && !fail) fail = result;
    }
  }
  return fail;
}

void test_log(char *name, int result) {
  printf("%s => %d\n", name, result);
}
