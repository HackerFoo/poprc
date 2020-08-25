`include "define.v"

`define primitive_op1(name, op, inT, outT) \
module __primitive_``name`` #( \
  parameter in0N = `inT``N, \
  parameter out0N = `outT``N \
)( \
  `input(simple, in0N, 0), \
  `output(simple, out0N, 0) \
); \
    assign out0 = op in0; \
endmodule

`define primitive_op2(name, op, inT, outT) \
module __primitive_``name`` #( \
  parameter in0N = `inT``N, \
  parameter in1N = `inT``N, \
  parameter out0N = `outT``N \
)( \
  `input(simple, in0N, 0), \
  `input(simple, in1N, 1), \
  `output(simple, out0N, 0) \
); \
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

module __primitive_ap01 #(
  parameter in0N = `intN,
  parameter out1N = `intN,
  parameter out0N = 0
)(
  `sync_ports,
  `input(stream, in0N, 0),
  `output(stream, out0N, 0),
  `output(simple, out1N, 1)
);
    assign out0 = in0 >> out1N;
    assign out1 = in0;
    assign out0_valid = in0_valid;
    assign out_valid = in0_valid && `valid(out1);
    assign in0_ready = out0_ready & out_ready;
    assign in_ready = `true;

endmodule

module __primitive_ap02 #(
  parameter in0N = `intN,
  parameter out1N = `intN,
  parameter out2N = `intN,
  parameter out0N = 0
)(
  `sync_ports,
  `input(stream, in0N, 0),
  `output(stream, out0N, 0),
  `output(simple, out1N, 1),
  `output(simple, out2N, 2)
);
    assign out0 = in0 >> (out1N + out2N);
    assign out1 = in0 >> out2N;
    assign out2 = in0;
    assign out0_valid = in0_valid;
    assign out_valid = in0_valid && `valid(out1) && `valid(out2);
    assign in0_ready = out0_ready & out_ready;
    assign in_ready = `true;

endmodule

/* ------------------------------------------------------ *
     NOTE ON READY/VALID DEPENDENCIES
 * ------------------------------------------------------ *

   `in_ready` can depend on `in_valid` asynchronously,
   but `out_valid` cannot depend on `out_ready`.

   This is to prevent loops, so that the
   flow is from source to sink and back.

 * ------------------------------------------------------ */

module __primitive_ap20 #(
  parameter in0N = `intN,
  parameter in1N = `intN,
  parameter in2N = `intN,
  parameter out0N = in0N + in1N + in2N
)(
  `sync_ports,
  `input(simple, in0N, 0),
  `input(simple, in1N, 1),
  `input(stream, in2N, 2),
  `output(stream, out0N, 0)
);
    assign out0 = { in0, in1, in2 };
    assign out0_valid = in_valid & in2_valid;
    assign in2_ready = out0_ready;
    assign in_ready = in_valid & out0_ready;
    assign out_valid = `true;

endmodule

module __primitive_pushr1 #(
  parameter in0N = `intN,
  parameter in1N = `intN,
  parameter out0N = `intN
)(
  `sync_ports,
  `input(stream, in0N, 0),
  `input(simple, in1N, 1),
  `output(stream, out0N, 0)
);
    assign out0 = { in0, in1 };
    assign out0_valid = in_valid & in0_valid;
    assign in0_ready = out0_ready;
    assign in_ready = in_valid & out0_ready;
    assign out_valid = `true;

endmodule

module __primitive_pushr2 #(
  parameter in0N = `intN,
  parameter in1N = `intN,
  parameter in2N = `intN,
  parameter out0N = in0N + in1N + in2N
)(
  `sync_ports,
  `input(stream, in0N, 0),
  `input(simple, in1N, 1),
  `input(simple, in2N, 2),
  `output(stream, out0N, 0)
);
    assign out0 = { in0, in1, in2 };
    assign out0_valid = in_valid & in0_valid;
    assign in0_ready = out0_ready;
    assign in_ready = in_valid & out0_ready;
    assign out_valid = `true;

endmodule

module transparent_buffer #(
  parameter N = `intN
)(
  input wire clk,
  input wire nrst,
  `input(stream, N, 0),
  `output(stream, N, 0)
);
    reg [N-1:0] data;
    reg         data_valid;

    assign in0_ready = !data_valid | out0_ready;
    assign out0_valid = data_valid | in0_valid;
    assign out0 = !data_valid ? in0 : data; // transparent when ready & valid

    always @(posedge clk) begin
        if(!nrst) begin // reset
            `reset(data_valid);
        end
        else begin
            if(!in0_valid && out0_ready) begin // drain
                `reset(data_valid);
            end
            else if(in0_valid && !out0_ready) begin // fill
                data <= in0;
                if(!out0_ready) `set(data_valid);
            end
        end
    end
endmodule

module __primitive_read_array #(
  parameter in0AN = `addrN,
  parameter in0DN = `intN,
  parameter in1N = `intN,
  parameter out1N = `intN,
  parameter out0AN = `addrN,
  parameter out0DN = `intN
)(
  `sync_ports,
  `input(Array, (in0AN, in0DN), 0),
  `input(simple, in1N, 1),
  `output(Array, (out0AN, out0DN), 0),
  `output(simple, out1N, 1)
);
    reg pending;
    wire buf_ready;

    // three-way setup:
    //   1. <- ready
    //   2. -> valid (active)
    //   3. <- addr, we, di (success)
    wire active = in_valid & buf_ready & !pending & nrst;
    wire success = active & in0_valid;

    assign in0_addr = success ? in1 : out0_addr;
    assign in0_we = !success & out0_we;
    assign in0_di = out0_di;
    assign out0 = in0;

    assign in0_ready = active | out0_ready;
    assign out0_valid = !active & in0_valid;
    assign in_ready = active & in0_valid;

    transparent_buffer #(.N(out1N))
      buffer(.clk(clk),
             .nrst(nrst),
             .in0(in0),
             .in0_valid(in0_valid & !pending),
             .in0_ready(buf_ready),
             .out0(out1),
             .out0_valid(out_valid),
             .out0_ready(out_ready));

    always @(posedge clk) begin
        if(!nrst) `reset(pending);
        else if(in_ready) pending <= out0_ready;
    end

endmodule

module __primitive_write_array #(
  parameter in0AN = `addrN,
  parameter in0DN = `intN,
  parameter in1N = `intN,
  parameter in2N = `intN,
  parameter out0AN = `addrN,
  parameter out0DN = `intN
)(
  `sync_ports,
  `input(Array, (in0AN, in0DN), 0),
  `input(simple, in1N, 1),
  `input(simple, in2N, 2),
  `output(Array, (out0AN, out0DN), 0)
);
    reg pending;

    // three-way setup:
    //   1. <- ready
    //   2. -> valid (active)
    //   3. <- addr, we, di (success)
    wire active = in_valid & !pending & nrst;
    wire success = active & in0_valid;

    assign in0_addr = success ? in1 : out0_addr;
    assign in0_we = success | out0_we;
    assign in0_di = success ? in2 : out0_di;
    assign out0 = in0;
    assign out_valid = `true;

    assign in0_ready = active | out0_ready;
    assign out0_valid = !active & in0_valid;
    assign in_ready = active & in0_valid;

    always @(posedge clk) begin
        if(!nrst) `reset(pending);
        else if(in_ready) pending <= out0_ready;
    end

endmodule

module dup_stream #(
  parameter N = 1
)(
  input wire          clk,
  input wire          in_valid,
  output wire         in_ready,
  output wire [N-1:0] out_valid,
  input wire [N-1:0]  out_ready
);
    generate
        if(N == 1) begin
            assign in_ready = out_ready[0];
            assign out_valid[0] = in_valid;
        end
        else begin
            reg [0:N-1] pending_prev;
            assign in_ready = ~| pending;
            assign out_valid = in_valid ? pending_prev : 0;
            wire [0:N-1] pending = pending_prev & ~out_ready;

            always @(posedge clk) begin
                pending_prev <= in_ready ? ~0 : pending;
            end
        end
    endgenerate
endmodule

module dup_Array #(
  parameter N = 1
)(
  input wire          clk,
  input wire          in_valid,
  output wire         in_ready,
  output wire [0:N-1] out_valid,
  input wire [0:N-1]  out_ready
);
    generate
        if(N == 1) begin
            assign in_ready = out_ready;
            assign out_valid = in_valid;
        end
        else begin
            reg [0:N-1] done;
            assign in_ready = | out_ready;
            assign out_valid = in_valid ? pending & ~(pending - 1) : 0; // select MSB
            wire [0:N-1] pending = out_ready & ~done;

            always @(posedge clk) begin
                done <= pending & ~out_valid ? done | out_valid : 0;
            end
        end
    endgenerate
endmodule
