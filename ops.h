#ifndef __WORDS__
#define __WORDS__

#include "rt_types.h"

#define OP_ITEM(name)  \
  extern reduce_t func_##name;

#include "op_list.h"

#undef OP_ITEM

#define OP_ITEM(name)  \
  OP_##name,

typedef enum op {
  OP_ITEM(null)
  #include "op_list.h"
  OP_COUNT
} op;

#undef OP_ITEM

#endif
