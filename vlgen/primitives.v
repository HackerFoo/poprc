`include "define.v"

`define primitive_op1(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  `input(inT, 0), \
  `output(outT, 0) \
); \
    assign out0 = op in0; \
endmodule

`define primitive_op2(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  `input(inT, 0), \
  `input(inT, 1), \
  `output(outT, 0) \
); \
    assign out0 = in0 op in1; \
endmodule

`primitive_op2(eq, ==, int, sym, yii)
`primitive_op2(neq, !=, int, sym, yii)
`primitive_op2(mod, %, int, int, iii)
`primitive_op2(div, /, int, int, iii)
`primitive_op1(not, !, sym, sym, yy)
`primitive_op2(mul, *, int, int, iii)
`primitive_op2(add, +, int, int, iii)
`primitive_op2(sub, -, int, int, iii)
`primitive_op2(shiftl, <<, int, int, iii)
`primitive_op2(shiftr, >>, int, int, iii)
`primitive_op2(gt, >, int, sym, yii)
`primitive_op2(gte, >=, int, sym, yii)
`primitive_op2(lt, >, int, sym, yii)
`primitive_op2(lte, >=, int, sym, yii)
`primitive_op1(complement, ~, int, int, ii)
`primitive_op2(bitand, &, int, int, iii)
`primitive_op2(bitor, |, int, int, iii)
`primitive_op2(bitxor, ^, int, int, iii)

// make these chainable to get ap0N
// stream end signal should be in-band, just use 0 for now
module __primitive_ap01_lli(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1)
);

    reg done = 0;

    assign out0 = in0;
    assign out1 = in0;
    assign out0_valid = done;
    assign out_valid = in0_valid & `valid(in0);

    always @(posedge clk) begin
        done <= in0_valid;
    end

endmodule

module __primitive_ap02_llii(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1),
  `output(int, 2)
);

    reg done = 0;
    reg  `intT data2 = 0; reg data2_valid = 0;

    assign out0 = in0;
    assign out1 = in0;
    assign out2 = data2;
    assign out0_valid = done;
    assign out_valid = data2_valid & `valid(in0) & `valid(data2);

    always @(posedge clk) begin
        done <= data2_valid;
        data2 <= in0;
        data2_valid <= in0_valid;
    end
endmodule
