#ifndef __LLVM_H__
#define __LLVM_H__

#include "rt_types.h"

#ifdef __cplusplus
extern "C" {
#endif

  reduce_t *compile(cell_t *c, unsigned int *in, unsigned int *out);

#ifdef __cplusplus
}
#endif

#endif
