#ifndef __WORDS__
#define __WORDS__

#define OP_ITEM(name)  \
  extern reduce_t func_##name;

#include "op_list.h"

#undef OP_ITEM

#define OP_ITEM(name)  \
  OP_##name,

typedef enum op {
  #include "op_list.h"
} op;

#undef OP_ITEM

#endif
