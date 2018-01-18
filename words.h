#ifndef __WORDS__
#define __WORDS__

#define WORD_ITEM(__name, __func, __in, __out)  \
  extern reduce_t func_##__func;

#include "word_list.h"

#undef WORD_ITEM

#endif
