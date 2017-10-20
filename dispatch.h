#ifndef __DISPATCH__
#define __DISPATCH__

// DISPATCH ________________________________________________________________________________

// macro to allow handling optional macro arguments
// DISPATCH(MAC, n, x_0 .. x_m) will reduce to MAC_i(x_0 .. x_m), where i = n-m, so i is the number of missing arguments
#define DISPATCH(m, n, ...) \
  CONCAT(DISPATCH, n)(m, __VA_ARGS__, _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20)
#define  DISPATCH0(m, argc, ...) \
      CONCAT(m, argc)()
#define  DISPATCH1(m, x0, argc, ...) \
      CONCAT(m, argc)(x0)
#define  DISPATCH2(m, x0, x1, argc, ...) \
      CONCAT(m, argc)(x0, x1)
#define  DISPATCH3(m, x0, x1, x2, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2)
#define  DISPATCH4(m, x0, x1, x2, x3, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3)
#define  DISPATCH5(m, x0, x1, x2, x3, x4, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4)
#define  DISPATCH6(m, x0, x1, x2, x3, x4, x5, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5)
#define  DISPATCH7(m, x0, x1, x2, x3, x4, x5, x6, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6)
#define  DISPATCH8(m, x0, x1, x2, x3, x4, x5, x6, x7, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7)
#define  DISPATCH9(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8)
#define DISPATCH10(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
#define DISPATCH11(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
#define DISPATCH12(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
#define DISPATCH13(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
#define DISPATCH14(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
#define DISPATCH15(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
#define DISPATCH16(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
#define DISPATCH17(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
#define DISPATCH18(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
#define DISPATCH19(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
#define DISPATCH20(m, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, argc, ...) \
      CONCAT(m, argc)(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)


// FORARG ________________________________________________________________________________

#define FORARG_0(name, x0, x1, x2, x3, x4, x5, x6, x7, x8, ...) \
  CONCAT(name, _pre)                                            \
  CONCAT(name, _first)(CONCAT(name, _a8), x0))                  \
  CONCAT(name, _middle)(x1)                                     \
  CONCAT(name, _middle)(x2)                                     \
  CONCAT(name, _middle)(x3)                                     \
  CONCAT(name, _middle)(x4)                                     \
  CONCAT(name, _middle)(x5)                                     \
  CONCAT(name, _middle)(x6)                                     \
  CONCAT(name, _middle)(x7)                                     \
  CONCAT(name, _last)(x8)                                       \
  CONCAT(name, _post)
#define FORARG_1(name, x0, x1, x2, x3, x4, x5, x6, x7, ...)     \
  CONCAT(name, _pre)                                            \
  CONCAT(name, _first)(CONCAT(name, _a7), x0)                   \
  CONCAT(name, _middle)(x1)                                     \
  CONCAT(name, _middle)(x2)                                     \
  CONCAT(name, _middle)(x3)                                     \
  CONCAT(name, _middle)(x4)                                     \
  CONCAT(name, _middle)(x5)                                     \
  CONCAT(name, _middle)(x6)                                     \
  CONCAT(name, _last)(x7)                                       \
  CONCAT(name, _post)
#define FORARG_2(name, x0, x1, x2, x3, x4, x5, x6, ...) \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(CONCAT(name, _a6), x0)           \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _middle)(x3)                             \
  CONCAT(name, _middle)(x4)                             \
  CONCAT(name, _middle)(x5)                             \
  CONCAT(name, _last)(x6)                               \
  CONCAT(name, _post)
#define FORARG_3(name, x0, x1, x2, x3, x4, x5, ...)     \
  CONCAT(name, _pre)                                    \
  CONCAT(name, _first)(CONCAT(name, _a5), x0)           \
  CONCAT(name, _middle)(x1)                             \
  CONCAT(name, _middle)(x2)                             \
  CONCAT(name, _middle)(x3)                             \
  CONCAT(name, _middle)(x4)                             \
  CONCAT(name, _last)(x5)                               \
  CONCAT(name, _post)
#define FORARG_4(name, x0, x1, x2, x3, x4, ...) \
  CONCAT(name, _pre)                            \
  CONCAT(name, _first)(CONCAT(name, _a4), x0)   \
  CONCAT(name, _middle)(x1)                     \
  CONCAT(name, _middle)(x2)                     \
  CONCAT(name, _middle)(x3)                     \
  CONCAT(name, _last)(x4)                       \
  CONCAT(name, _post)
#define FORARG_5(name, x0, x1, x2, x3, ...)     \
  CONCAT(name, _pre)                            \
  CONCAT(name, _first)(CONCAT(name, _a3), x0)   \
  CONCAT(name, _middle)(x1)                     \
  CONCAT(name, _middle)(x2)                     \
  CONCAT(name, _last)(x3)                       \
  CONCAT(name, _post)
#define FORARG_6(name, x0, x1, x2, ...)         \
  CONCAT(name, _pre)                            \
  CONCAT(name, _first)(CONCAT(name, _a2), x0)   \
  CONCAT(name, _middle)(x1)                     \
  CONCAT(name, _last)(x2)                       \
  CONCAT(name, _post)
#define FORARG_7(name, x0, x1, ...)             \
  CONCAT(name, _pre)                            \
  CONCAT(name, _first)(CONCAT(name, _a1), x0)   \
  CONCAT(name, _last)(x1)                       \
  CONCAT(name, _post)
#define FORARG_8(name, x0, ...)                 \
  CONCAT(name, _pre)                            \
  CONCAT(name, _only)(CONCAT(name, _a0), x0)    \
  CONCAT(name, _post)

#define FORARG(name, ...) DISPATCH(FORARG, 10, name, __VA_ARGS__)

#endif
