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
// stream end signal is in-band
module __primitive_ap01_lli(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1)
);

    assign out0 = in0;
    assign out1 = in0;
    assign out0_valid = in0_valid & ~in_valid;
    assign out_valid = in0_valid & in_valid & `valid(in0);

    assign in_ready = in0_valid & in_valid;
    assign in0_ready = out_ready & (out0_ready | in_valid);

endmodule

module __primitive_ap02_llii(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1),
  `output(int, 2)
);

    reg done = `false;
    reg  `intT data2 = `false; reg data2_valid = `false;

    assign in_ready = `true;
    assign in0_ready = ~done | out0_ready;

    assign out0 = in0;
    assign out1 = in0;
    assign out2 = data2;
    assign out0_valid = done;
    assign out_valid = data2_valid & `valid(in0) & `valid(data2) & ~done;

    always @(posedge clk) begin
        if(in_valid) begin
            `reset(data2_valid);
            `reset(done);
        end
        else if(in0_valid) begin
            done <= data2_valid;
            data2 <= in0;
            data2_valid <= in0_valid;
        end
    end
endmodule

// TODO stream should preempt other inputs
module __primitive_ap20_liil(
  `sync_ports,
  `input(int, 0),
  `input(int, 1),
  `input(stream, 2),
  `output(stream, 0)
);

    reg `intT data0 = 0; reg data0_valid = 0;
    assign out0 = in_valid ? (data0_valid ? data0 : in1) : in2;
    assign out0_valid = in_valid | data0_valid | in2_valid;
    assign in2_ready = ~in_valid & ~data0_valid;
    assign in_ready = ~data0_valid;
    assign out_valid = `true;

    always @(posedge clk) begin
        if(in_valid) begin
            data0 <= in0;
            `set(data0_valid);
        end
        if(data0_valid) `reset(data0_valid);
    end

endmodule

module __primitive_pushr1_lli(
  `sync_ports,
  `input(stream, 0),
  `input(int, 1),
  `output(stream, 0)
);

    assign out0 = in_valid ? in1 : in0;
    assign out0_valid = in_valid | in0_valid;
    assign in0_ready = ~in_valid;
    assign in_ready = out0_ready;
    assign out_valid = `true;

endmodule
