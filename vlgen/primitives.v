`include "define.v"

`define primitive_op1(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  input  inT in0, \
  output outT out0 \
); \
    assign out0 = op in0; \
endmodule

`define primitive_op2(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  input  inT in1, \
  input  inT in0, \
  output outT out0 \
); \
    assign out0 = in1 op in0; \
endmodule

`primitive_op2(eq, ==, `intT, `symT, yii)
`primitive_op2(neq, !=, `intT, `symT, yii)
`primitive_op2(mod, %, `intT, `intT, iii)
`primitive_op1(not, !, `symT, `symT, yy)
`primitive_op2(mul, *, `intT, `intT, iii)
`primitive_op2(add, +, `intT, `intT, iii)
`primitive_op2(sub, -, `intT, `intT, iii)
`primitive_op2(shiftl, <<, `intT, `intT, iii)
`primitive_op2(shiftr, >>, `intT, `intT, iii)
`primitive_op2(gt, >, `intT, `symT, yii)
`primitive_op2(gte, >=, `intT, `symT, yii)
`primitive_op2(lt, >, `intT, `symT, yii)
`primitive_op2(lte, >=, `intT, `symT, yii)
`primitive_op1(complement, ~, `intT, `intT, ii)
`primitive_op2(bitand, &, `intT, `intT, iii)
`primitive_op2(bitor, |, `intT, `intT, iii)
`primitive_op2(bitxor, ^, `intT, `intT, iii)
