`include "define.v"

`define primitive_op1(name, op, inN, outN, suf) \
module __primitive_``name``_``suf``( \
  input  [inN:0] in0, \
  output [outN:0] out0 \
); \
    assign out0 = {in0[inN], op in0[inN-1:0]}; \
endmodule

`define primitive_op2(name, op, inN, outN, suf) \
module __primitive_``name``_``suf``( \
  input  [inN:0] in1, \
  input  [inN:0] in0, \
  output [outN:0] out0 \
); \
    assign out0 = { \
      in1[inN] & in0[inN], \
      in1[inN-1:0] op in0[inN-1:0] \
    }; \
endmodule

`primitive_op2(eq, ==, `intN, `symN, yii)
`primitive_op2(neq, !=, `intN, `symN, yii)
`primitive_op2(mod, %, `intN, `intN, iii)
`primitive_op2(div, /, `intN, `intN, iii)
`primitive_op1(not, !, `symN, `symN, yy)
`primitive_op2(mul, *, `intN, `intN, iii)
`primitive_op2(add, +, `intN, `intN, iii)
`primitive_op2(sub, -, `intN, `intN, iii)
`primitive_op2(shiftl, <<, `intN, `intN, iii)
`primitive_op2(shiftr, >>, `intN, `intN, iii)
`primitive_op2(gt, >, `intN, `symN, yii)
`primitive_op2(gte, >=, `intN, `symN, yii)
`primitive_op2(lt, >, `intN, `symN, yii)
`primitive_op2(lte, >=, `intN, `symN, yii)
`primitive_op1(complement, ~, `intN, `intN, ii)
`primitive_op2(bitand, &, `intN, `intN, iii)
`primitive_op2(bitor, |, `intN, `intN, iii)
`primitive_op2(bitxor, ^, `intN, `intN, iii)

// make these chainable to get ap0N
// stream end signal should be in-band, just use 0 for now
module __primitive_ap01_lli(
  input  clk,
  input  `intT stream_in,
  output reg `intT stream_out,
  output reg `intT data0_out
);

    initial begin
        stream_out = 0;
        data0_out = 0;
    end

    always @(posedge clk) begin
        if(stream_in`intR) begin
            data0_out <= stream_in;
        end
        else if(data0_out`intR) begin
            stream_out <= {`true, stream_in`intD};
            `reset(data0_out`intR);
        end
        else begin
            stream_out <= stream_in;
        end
    end
endmodule

module __primitive_ap02_llii(
  input  clk,
  input  `intT stream_in,
  output reg `intT stream_out,
  output reg `intT data1_out,
  output reg `intT data0_out
);

    initial begin
        stream_out = 0;
        data1_out = 0;
        data0_out = 0;
    end

    always @(posedge clk) begin
        if(stream_in`intR) begin
            data1_out <= {`true, stream_in`intD};
        end
        else if(data1_out`intR && !data0_out`intR) begin
            data0_out <= {`true, stream_in`intD};
        end
        else if(data0_out`intR) begin
            stream_out <= {`true, stream_in`intD};
            `reset(data1_out`intR);
            `reset(data0_out`intR);
        end
        else begin
            stream_out <= stream_in;
        end
    end
endmodule
