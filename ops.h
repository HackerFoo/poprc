#ifndef __WORDS__
#define __WORDS__

#include "rt_types.h"

#define OP__ITEM(name) \
  extern reduce_t func_##name;

#include "op_list.h"

#undef OP__ITEM

#define OP__ITEM(name) \
  OP_##name,

typedef enum op {
  OP__ITEM(null)
  #include "op_list.h"
  OP_COUNT
} op;

#undef OP__ITEM

#endif
