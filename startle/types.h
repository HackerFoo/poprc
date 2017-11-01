#ifndef __STARTLE_TYPES__
#define __STARTLE_TYPES__

typedef struct pair_t {
  uintptr_t first, second;
} pair_t;

// string segment
typedef struct seg_t {
  const char *s;
  size_t n;
} seg_t;

#define UNUSED __attribute__((unused))

#endif
