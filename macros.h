/* Copyright 2012-2016 Dustin DeWeese
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

#ifndef __MACROS__
#define __MACROS__

#define sizeof_field(s, f) sizeof(((s *)0)->f)

#define zero(a) memset((a), 0, sizeof(a))

#define show(x) printf(#x " = %d\n", (int)(x))
#define WIDTH(a) (sizeof((a)[0]))
#define LENGTH(a) (sizeof(a) / WIDTH(a))
#define COUNTDOWN(i, n) if(n) for(size_t i = (n); i--; )
#define COUNTUP(i, n) for(size_t i = 0, __n = (n); i < __n; i++)
#define FOREACH(i, a) COUNTUP(i, LENGTH(a))
#define _CONCAT(x, y) x##y
#define CONCAT(x, y) _CONCAT(x, y)
#define UNIQUE CONCAT(__unique_, __LINE__)
#define LOOP(n) COUNTDOWN(UNIQUE, n)
#define sizeof_field(s, f) sizeof(((s *)0)->f)

#define min(a, b) ((a) <= (b) ? (a) : (b))
#define max(a, b) ((a) >= (b) ? (a) : (b))

#ifdef EMSCRIPTEN
#define MARK_BIT (1<<31)
#else
#define MARK_BIT 2
#endif

#define is_marked(p) (((uintptr_t)(p) & (MARK_BIT)) != 0)
#define mark_ptr(p) ((void *)((uintptr_t)(p) | (MARK_BIT)))
#define clear_ptr(p) ((void *)((uintptr_t)(p) & ~(MARK_BIT)))

#define WORD(name, func, in, out)                        \
  {                                                      \
    name,                                                \
    func_##func,                                         \
    in,                                                  \
    out,                                                 \
    NULL                                                 \
  }

#define TEST(name)                                       \
  {                                                      \
    &test_##name,                                        \
    "test_" #name                                        \
  }

struct __test_entry {
  int (*func)(char *name);
  char *name;
};

#define BITSET(name, size) uint8_t name[((size)+7)/8] = {0}
#define BITSET_INDEX(name, array) BITSET(name, LENGTH(array))

#endif
