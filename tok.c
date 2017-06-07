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

#include "rt_types.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "gen/test.h"
#include "gen/support.h"
#include "gen/tok.h"

char_class_t char_class(char c) {
  assert_throw(c < 127, "ASCII only please");
  if(c <= ' ' || c > '~')
    return CC_NONE;
  if(c >= '0' && c <= '9')
    return CC_NUMERIC;
  if((c >= 'a' && c <= 'z') ||
     (c >= 'A' && c <= 'Z'))
    return CC_ALPHA;
  if(c == '?') return CC_VAR;
  if(c == '_') return CC_COMMENT;
  if(strchr("[](){}", c))
    return CC_BRACKET;
  return CC_SYMBOL;
}

// starts at comment
const char *skip_comment(const char *s, const char *e) {
  int level = 0;
  char_class_t before = CC_NONE;
  const char *ptr = s;

  for(;;) {
    // move cursor past comment character, and record character class after it
    ptr++;
    if(ptr >= e) return e;
    char_class_t after;
    unsigned int length = 1;
    while((after = char_class(*ptr)) == CC_COMMENT) {
      length++;
      ptr++;
      if(ptr >= e) return e;
    }

    if(before == CC_NONE) {
      if(after != CC_NONE) {
        level++;
      } else if(level == 0) {
        if(length > 1) {
          // line comment
          while(ptr < e && *ptr && *ptr != '\n') ptr++;
          return ptr;
        } else {
          // just a lone comment char
          return s;
        }
      }
    } else if(after == CC_NONE) {
      level--;
      if(ptr >= e || !*ptr || level <= 0) return ptr;
    }

    // move cursor to next comment character, tracking character class before it
    ptr++;
    char_class_t cur = after;
    do {
      before = cur;
      ptr++;
      if(ptr >= e || !*ptr) return ptr;
      cur = char_class(*ptr);
    } while(cur != CC_COMMENT);
  }
}

void mark_comments(char c, char *str) {
  char *ptr = str;
  char *e = str + strlen(str);
  while(*ptr) {
    char_class_t cc = char_class(*ptr);
    switch(cc) {
    case CC_NONE:
      ptr++;
      break;
    case CC_COMMENT: {
      char *start = ptr;
      ptr = (char *)skip_comment(ptr, e);
      if(ptr == start) {
        ptr++;
      } else {
        memset(start, c, ptr - start);
      }
      break;
    }
    default:
      while(char_class(*++ptr) != CC_NONE);
    }
  }
}

int test_comments() {
  char str[] =
    "[1] One def\n"
    "[2] T_w_o def\n"
    "{ _an inline _n_e_s_t_ed_ comment_\n"
    "  [one t_w_o +] :three def\n"
    "  three *\n"
    "} M def __ a line comment\n"
    "__ stack is: 6\n"
    "M:three\n"
    "__ stack is: 6 3\n";
  mark_comments('#', str);
  printf("%s", str);
  return 0;
}

seg_t tok(const char *s, const char* e, char_class_t *class) {
  seg_t seg = {NULL, 0};
  char_class_t cc = CC_NONE;

  /* skip spaces & comments */
  for(;;) {
    if(s >= e || !*s) goto done;
    cc = char_class(*s);
    if(cc == CC_COMMENT) {
      const char *n = skip_comment(s, e);
      if(s == n) break;
      s = n;
    } else if(cc == CC_NONE) {
      s++;
    } else break;
  }

  /* at start of token */
  seg.s = s;

  /* allow adjacent brackets to be separately tokenized */
  if(cc == CC_BRACKET ||
     cc == CC_COMMENT) {
    seg.n = 1;
    goto done;
  }

  while(++s < e && *s) {
    char_class_t ncc = char_class(*s);
    if(cc == ncc || cc == CC_NONE) {
      cc = ncc;
      continue;
    }

    // exceptions
    switch(ncc) {
    case CC_NUMERIC: // negative numbers
      if(s[-1] == '-') {
        cc = ncc;
        continue;
      } else if(cc == CC_ALPHA) { // allow numeric after alpha
        continue;
      }
      break;
    case CC_COMMENT: // comment char inside token
      if(cc == CC_ALPHA ||
         cc == CC_SYMBOL) {
        continue;
      }
      break;
    case CC_SYMBOL:
      if(s[0] == '.') {
        if(cc == CC_ALPHA) {
          cc = CC_NONE;
          continue; // allow dots in alpha identifiers
        }
        if(cc == CC_NUMERIC) {
          char *end;
          strtod(seg.s, &end);
          if(end != seg.s) {
            seg.n = end - seg.s;
            if(class) *class = CC_FLOAT;
            return seg;
          }
        }
      }
      break;
    default:
      break;
    }
    break;
  }
  seg.n = s - seg.s;
done:
  if(class) *class = cc;
  // separate trailing colon
  if(seg.n > 1 && seg.s[seg.n-1] == ':') seg.n--;
  return seg;
}

void update_line(const char *start, const char *end, const char **line) {
  const char *p = end - 1;
  while(p >= start) {
    if(*p == '\n') {
      *line = p + 1;
      break;
    } else {
      p--;
    }
  }
}
