#include <stdio.h>
#include <stdlib.h>

#include "cgen/primitives.h"
#include "startle/macros.h"
#include "startle/support.h"

static int mem[256];
static int *mem_ptr = mem;
static char strings[1024];
static char *strings_ptr = strings;
static char string_buffer[64];

int *mem_alloc(unsigned int n) {
  assert_throw(mem_ptr - mem + n <= LENGTH(mem), "out of mem");
  int *m = mem_ptr;
  mem_ptr += n;
  return m;
}

int *alloc_arr(array *arr, unsigned int n) {
  arr->size = n;
  arr->elem = mem_alloc(n);
  return arr->elem;
}

char *string_alloc(unsigned int n) {
  assert_throw(strings_ptr - strings + n <= LENGTH(strings), "out of mem");
  char *s = strings_ptr;
  strings_ptr += n;
  return s;
}

seg_t alloc_seg(char *s, unsigned int n) {
  char *ns = string_alloc(n);
  memcpy(ns, s, n);
  return (seg_t) {
    .s = ns,
    .n = n
  };
}

seg_t __primitive_to_string(int x) {
  int len = snprintf(string_buffer,sizeof(string_buffer), "%d", x);
  return alloc_seg(string_buffer, min(sizeof(string_buffer), len));
}

symbol_t __primitive_print(symbol_t io, seg_t str) {
  printf("%.*s\n", (int)str.n, str.s);
  fflush(stdout);
  return io;
}

symbol_t __primitive_input(symbol_t io, seg_t *str) {
  char *s = fgets(string_buffer, sizeof(string_buffer), stdin);
  int n = 0;
  if(s) {
    char *e = s;
    while(e < *(&string_buffer + 1) &&
          !ONEOF(*e, '\0', '\n')) e++;
    n = e - s;
  }
  if(str) *str = alloc_seg(s, n);
  return io;
}

seg_t __primitive_strtrim(seg_t str) {
  return seg_trim(str);
}

bool __from_string(seg_t str, int *x) {
  char *end = NULL;
  long lx = strtol(str.s, &end, 0);
  if(!end) {
    return true;
  } else {
    *x = lx;
    return false;
  }
}

symbol_t __primitive_eq_str(seg_t a, seg_t b) {
  return a.n == b.n && strncmp(a.s, b.s, a.n) == 0 ?
    SYM_True : SYM_False;
}

seg_t __primitive_strcat(seg_t a, seg_t b) {
  char *s = string_alloc(a.n + b.n);
  memcpy(s, a.s, a.n);
  memcpy(s + a.n, b.s, b.n);
  return (seg_t) {
    .s = s,
    .n = a.n + b.n
  };
}

// dummy formatters
FORMAT(cell, 'C') {}
FORMAT(entry, 'E') {}
FORMAT(entry_short, 'e') {}
FORMAT(function, 'O') {}
FORMAT(trace_cell, 'T') {}
