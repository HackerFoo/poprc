`include "define.v"

`define primitive_op1(name, op, inT, outT) \
module __primitive_``name``( \
  `input(simple, in0N, 0), \
  `output(simple, out0N, 0) \
); \
    parameter in0N = `inT``N; \
    parameter out0N = `outT``N; \
    assign out0 = op in0; \
endmodule

`define primitive_op2(name, op, inT, outT) \
module __primitive_``name``( \
  `input(simple, in0N, 0), \
  `input(simple, in1N, 1), \
  `output(simple, out0N, 0) \
); \
    parameter in0N = `inT``N; \
    parameter in1N = `inT``N; \
    parameter out0N = `outT``N; \
    assign out0 = in0 op in1; \
endmodule

`primitive_op2(eq, ==, int, sym)
`primitive_op2(neq, !=, int, sym)
`primitive_op2(mod, %, int, int)
`primitive_op2(div, /, int, int)
`primitive_op1(not, !, sym, sym)
`primitive_op2(mul, *, int, int)
`primitive_op2(add, +, int, int)
`primitive_op2(sub, -, int, int)
`primitive_op2(shiftl, <<, int, int)
`primitive_op2(shiftr, >>, int, int)
`primitive_op2(gt, >, int, sym)
`primitive_op2(gte, >=, int, sym)
`primitive_op2(lt, <, int, sym)
`primitive_op2(lte, <=, int, sym)
`primitive_op1(complement, ~, int, int)
`primitive_op2(bitand, &, int, int)
`primitive_op2(bitor, |, int, int)
`primitive_op2(bitxor, ^, int, int)

// make these chainable to get ap0N
// stream end signal is in-band
module __primitive_ap01(
  `sync_ports,
  `input(stream, in0N, 0),
  `output(stream, out0N, 0),
  `output(simple, out1N, 1)
);
    parameter in0N = `intN;
    parameter out1N = `intN;
    parameter out0N = in0N - out1N;

    assign out0 = in0 >> out1N;
    assign out1 = in0;
    assign out0_valid = in0_valid;
    assign out_valid = in0_valid & `valid(out1);

    assign in_ready = `true;
    assign in0_ready = out0_ready & out_ready;

endmodule

module __primitive_ap02(
  `sync_ports,
  `input(stream, in0N, 0),
  `output(stream, out0N, 0),
  `output(simple, out1N, 1),
  `output(simple, out2N, 2)
);
    parameter in0N = `intN;
    parameter out0N = `intN;
    parameter out1N = `intN;
    parameter out2N = `intN;

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

/* ------------------------------------------------------ *
     NOTE ON READY/VALID DEPENDENCIES
 * ------------------------------------------------------ *

   `in_ready` can depend on `in_valid` asynchronously,
   but `out_valid` cannot depend on `out_ready`.

   This is to prevent loops, so that the
   flow is from source to sink and back.

 * ------------------------------------------------------ */

module __primitive_ap20(
  `sync_ports,
  `input(simple, in0N, 0),
  `input(simple, in1N, 1),
  `input(stream, in2N, 2),
  `output(stream, out0N, 0)
);
    parameter in0N = `intN;
    parameter in1N = `intN;
    parameter in2N = `intN;
    parameter out0N = `intN;

    reg `intT data0 = 0; reg data0_valid = 0;
    assign out0 = in_valid ? (data0_valid ? data0 : in1) : in2;
    assign out0_valid = in_valid | data0_valid | in2_valid;
    assign in2_ready = ~in_valid & ~data0_valid;
    assign in_ready = in_valid & ~data0_valid;
    assign out_valid = `true;

    always @(posedge clk) begin
        if(in_valid) begin
            data0 <= in0;
            `set(data0_valid);
        end
        if(data0_valid) `reset(data0_valid);
    end

endmodule

module __primitive_pushr1(
  `sync_ports,
  `input(stream, in0N, 0),
  `input(simple, in1N, 1),
  `output(stream, out0N, 0)
);
    parameter in0N = `intN;
    parameter in1N = `intN;
    parameter out0N = `intN;

    assign out0 = { in0, in1 };
    assign out0_valid = in_valid & in0_valid;
    assign in0_ready = out0_ready;
    assign in_ready = in_valid & out0_ready;
    assign out_valid = `true;

endmodule

module __primitive_pushr2(
  `sync_ports,
  `input(stream, in0N, 0),
  `input(simple, in1N, 1),
  `input(simple, in2N, 2),
  `output(stream, out0N, 0)
);
    parameter in0N = `intN;
    parameter in1N = `intN;
    parameter in2N = `intN;
    parameter out0N = in0N + in1N + in2N;
    assign out0 = { in0, in1, in2 };
    assign out0_valid = in_valid & in0_valid;
    assign in0_ready = out0_ready;
    assign in_ready = in_valid & out0_ready;
    assign out_valid = `true;

endmodule

/*
module __primitive_pushr2(
  `sync_ports,
  `input(stream, in0N, 0),
  `input(simple, in1N, 1),
  `input(simple, in2N, 2),
  `output(stream, out0N, 0)
);
    parameter in0N = `intN;
    parameter in1N = `intN;
    parameter in2N = `intN;
    parameter out0N = `intN;

    reg select = `false;
    assign out0 = in_valid ? (select ? in2 : in1) : in0;
    assign out0_valid = in_valid | in0_valid;
    assign in0_ready = ~in_valid;
    assign in_ready = out0_ready & ~select;
    assign out_valid = `true;

    always @(posedge clk) begin
        if(in_valid) begin
            select <= ~select;
        end
        else begin
            `reset(select);
        end
    end

endmodule
*/

module __primitive_read_array(
  `sync_ports,
  `input(Array, (in0AN, in0DN), 0),
  `input(simple, in1N, 1),
  `output(Array, (out0AN, out0DN), 0),
  `output(simple, out1N, 1)
);
    parameter in0AN = `addrN;
    parameter in0DN = `intN;
    parameter in1N = `intN;
    parameter out1N = `intN;
    parameter out0AN = `addrN;
    parameter out0DN = `intN;

    reg [out1N-1:0] out1_reg; // "empty" when !out_valid

    reg out_valid = `false;
    assign out1 = in_valid ? in0_do : out1_reg; // latch the data returned
    assign in_ready = in_valid & (!out_valid | out_ready) & in0_ready;

    assign in0_addr = in_valid ? in1 : out0_addr;
    assign in0_we = !in_valid & out0_we;
    assign in0_di = out0_di;
    assign out0_do = in0_do;
    assign in0_valid = in_valid | out0_valid;
    assign out0_ready = in0_ready;

    always @(posedge clk) begin
        if(in0_valid & in0_ready) begin
            out1_reg <= in0_do;
            `set(out_valid);
        end
        else if(out_ready) begin
            `reset(out_valid);
        end
    end

endmodule

module __primitive_write_array(
  `sync_ports,
  `input(Array, (in0AN, in0DN), 0),
  `input(simple, in1N, 1),
  `input(simple, in2N, 2),
  `output(Array, (out0AN, out0DN), 0)
);
    parameter in0AN = `addrN;
    parameter in0DN = `intN;
    parameter in1N = `intN;
    parameter in2N = `intN;
    parameter out0AN = `addrN;
    parameter out0DN = `intN;

    reg out_valid = `false;
    assign in_ready = in_valid & in0_ready;

    assign in0_addr = in_valid ? in1 : out0_addr;
    assign in0_we = in_valid | out0_we;
    assign in0_di = in_valid ? in2 : out0_di;
    assign out0_do = in0_do;
    assign in0_valid = in_valid | out0_valid;
    assign out0_ready = in0_ready;

    always @(posedge clk) begin
        if(in0_valid & in0_ready) begin
            `set(out_valid);
        end
        else if(out_ready) begin
            `reset(out_valid);
        end
    end

endmodule

// arbitrates two masters out0 and out1 to slave in0
// out0 has priority
module __primitive_dup_array(
  `sync_ports,
  `input(Array, (in0AN, in0DN), 0),
  `output(Array, (out0AN, out0DN), 0),
  `output(Array, (out1AN, out1DN), 1)
);
    parameter in0AN = `addrN;
    parameter in0DN = `intN;
    parameter out0AN = `addrN;
    parameter out0DN = `intN;
    parameter out1AN = `addrN;
    parameter out1DN = `intN;

    assign in0_addr   = out0_valid ? out0_addr  : out1_addr;
    assign in0_we     = out0_valid ? out0_we    : out1_we;
    assign in0_di     = out0_valid ? out0_di    : out1_di;
    assign out0_do    = in0_do;
    assign out1_do    = in0_do;
    assign in0_valid  = out0_valid | out1_valid;
    assign out0_ready = in0_ready;
    assign out1_ready = in0_ready;
    assign in_ready   = `true;
    assign out_valid  = `true;

endmodule
