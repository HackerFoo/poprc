#ifndef __cgen_primitives__
#define __cgen_primitives__

#define __primitive_add(x, y) x + y
#define __primitive_sub(x, y) x - y
#define __primitive_mul(x, y) x * y
#define __primitive_eq(x, y) x == y;
#define __primitive_eq_s(x, y) x == y;
#define __primitive_neq(x, y) x != y;
#define __primitive_neq_s(x, y) x != y;
#define __primitive_gt(x, y) x > y
#define __primitive_gte(x, y) x >= y
#define __primitive_lt(x, y) x < y
#define __primitive_lte(x, y) x <= y
#define __primitive_mod(x, y) x % y
#define __primitive_not(x) !x
#define __primitive_bitand(x, y) x & y
#define __primitive_bitor(x, y) x | y
#define __primitive_bitxor(x, y) x ^ y
#define __primitive_shiftl(x, y) x << y
#define __primitive_shiftr(x, y) x >> y
#define __primitive_complement(x) ~x

#endif
