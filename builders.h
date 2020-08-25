#ifndef __BUILDERS__
#define __BUILDERS__

#define WORD__ITEM(__file, __line, __name, __func, __in, __out, ...)        \
  BUILDER##__in##__out(__func, __func)
#define WORD_ALIAS__ITEM(__file, __line, __name, __func, __in, __out, __builder, ...) \
  BUILDER##__in##__out(__builder, __func)

#define BUILDER01(__name, __op)                 \
  static inline cell_t *build_##__name() {      \
    return build01(OP_##__op);                  \
  }
#define BUILDER02(__name, __op)                         \
  static inline cell_t *build_##__name(cell_t **o1) {   \
    return build02(OP_##__op, o1);                      \
  }
#define BUILDER11(__name, __op)                         \
  static inline cell_t *build_##__name(cell_t *i0) {    \
    return build11(OP_##__op, i0);                      \
  }
#define BUILDER21(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1) {        \
    return build21(OP_##__op, i0, i1);                                  \
  }
#define BUILDER31(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t *i2) { \
    return build31(OP_##__op, i0, i1, i2);                              \
  }
#define BUILDER12(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t **o1) {       \
    return build12(OP_##__op, i0, o1);                                  \
  }
#define BUILDER22(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t **o1) { \
    return build22(OP_##__op, i0, i1, o1);                              \
  }
#define BUILDER32(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t *i2, cell_t **o1) { \
    return build32(OP_##__op, i0, i1, i2, o1);                          \
  }
#define BUILDER42(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t *i2, cell_t *i3, cell_t **o1) { \
    return build42(OP_##__op, i0, i1, i2, i3, o1);                      \
  }
#define BUILDER52(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t *i2, cell_t *i3, cell_t *i4, cell_t **o1) { \
    return build52(OP_##__op, i0, i1, i2, i3, i4, o1);                  \
  }
#define BUILDER23(__name, __op)                                         \
  static inline cell_t *build_##__name(cell_t *i0, cell_t *i1, cell_t **o1, cell_t **o2) { \
    return build23(OP_##__op, i0, i1, o1, o2);                          \
  }

#include "word_list.h"

#undef WORD__ITEM

#endif
