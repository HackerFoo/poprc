/* Copyright 2012-2020 Dustin DeWeese
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

#include "startle/error.h"
#include "startle/log.h"
#include "startle/test.h"
#include "startle/support.h"

#include "parse/tok.h"

void attribute_parser(const char *s, const char *e, bool after_newline, attr_t *attr_before, attr_t *attr_after);

char_class_t char_class(char c) {
  assert_throw(c < 127, "ASCII only please");
  if(!INRANGE(c, '!', '~'))
    return CC_NONE;
  if(INRANGE(c, '0', '9'))
    return CC_NUMERIC;
  if(INRANGE(c, 'a', 'z', 'A', 'Z'))
    return CC_ALPHA;
  if(c == '?') return CC_VAR;
  if(c == '.') return CC_DOT;
  if(c == '_') return CC_COMMENT;
  if(c == '\"') return CC_STRING;
  if(c == ',') return CC_COMMA;
  if(ONEOF(c, '[', ']', '(', ')', '{', '}'))
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

TEST(comments) {
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

const char *string_literal_end(const char *s, const char *e) {
  assert_throw(s + 1 < e && *s++ == '\"');
  while(s < e && *s != '\"') {
    if(*s == '\\' && ++s == e) break;
    s++;
  }
  assert_throw(*s == '\"');
  return s + 1;
}

TEST(string_literal) {
  const char s[] = "\"test (\\\") string\"end";
  const char *e = s + sizeof(s) - 1;
  const char *a = string_literal_end(s, e);
  printf("end = \"%s\"\n", a);
  return strcmp(a, "end") ? -1 : 0;
}

seg_t tok(const char *s, const char* e, char_class_t *class, attr_t *attr_before, attr_t *attr_after) {
  seg_t seg = {NULL, 0};
  char_class_t cc = CC_NONE;
  bool after_newline = false;

  /* skip spaces & comments */
  for(;;) {
    if(s >= e || !*s) goto done;
    cc = char_class(*s);
    if(cc == CC_COMMENT) {
      const char *n = skip_comment(s, e);
      if(s == n) break;
      attribute_parser(s, n, after_newline, attr_before, attr_after);
      s = n;
    } else if(cc == CC_NONE) {
      if(*s == '\n') after_newline = true;
      s++;
    } else break;
  }

  /* at start of token */
  seg.s = s;

  /* string literals */
  if(cc == CC_STRING) {
    s = string_literal_end(s, e);
    seg.n = s - seg.s;
    goto done;
  }

  /* separately tokenize these character classes */
  if(ONEOF(cc, CC_BRACKET, CC_COMMENT, CC_COMMA)) {
    seg.n = 1;
    goto done;
  }

  bool hex_mode = false;
  int i = 0;

  while(++s < e && *s) {
    i++;
    char_class_t ncc = char_class(*s);
    if(cc == ncc || cc == CC_NONE) {
      cc = ncc;
      continue;
    }

    // exceptions
    switch(ncc) {
    case CC_NUMERIC: // negative numbers
      if(cc == CC_FLOAT) continue;
      if(i == 1 && s[-1] == '-') {
        cc = CC_NUMERIC;;
        continue;
      } else if(cc == CC_ALPHA) { // allow numeric after alpha
        continue;
      }
      break;
    case CC_COMMENT: // underscore after token e.g. foo_bar
      if(ONEOF(cc, CC_ALPHA, CC_SYMBOL)) {
        continue;
      }
      break;
    case CC_DOT:
      if(s[1] != '.') {
        if(cc == CC_ALPHA) {
          cc = CC_NONE;
          continue; // allow single dots in identifiers e.g. foo.bar
        }
        if(cc == CC_NUMERIC) { // e.g. 1.2
          cc = CC_FLOAT;
          continue;
        }
      }
      break;
    case CC_ALPHA:
      if(cc == CC_NUMERIC) {
        // 0x... or -0x...
        if(*s == 'x' &&
           INRANGE(i, 1, 2) &&
           s[-1] == '0' &&
           (i == 1 || s[-2] == '-')) {
          hex_mode = true;
          continue;
        }
        // allow a-fA-F in hex
        if(hex_mode && INRANGE(*s, 'a', 'f', 'A', 'F')) continue;
      }
      if(ONEOF(cc, CC_NUMERIC, CC_FLOAT) &&
         ONEOF(*s, 'e', 'E')) { // e.g. 1.2e3
        cc = CC_FLOAT;
        continue;
      }
      if(cc == CC_SYMBOL && s[-1] != ':') {
        cc = CC_ALPHA;
        continue; // allow symbols to be followed with letters
      }
      break;
    case CC_SYMBOL: // 5e-3
      if(cc == CC_FLOAT &&
         i > 0 && ONEOF(s[-1], 'e', 'E')) {
        continue;
      }
      break;
    default:
      break;
    }
    break;
  }
  seg.n = s - seg.s;
done:
  if(cc == CC_DOT) cc = CC_SYMBOL;
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
