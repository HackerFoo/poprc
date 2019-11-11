`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module top(
           input         clk,
           input [15:0]  in,
           output [15:0] out
           );

    reg                  `intT  a;
    reg                  `intT  b;
    wire                 `intT  a_in = in[7:0];
    wire                 `intT  b_in = in[15:8];
    wire                 `intT  c;
    reg                  in_valid;

    always @(posedge clk) begin
        if(a != a_in || b != b_in)
          begin
              a <= a_in;
              b <= b_in;
              `set(in_valid);
          end
        else
          begin
              `reset(in_valid);
          end
    end

    wire gcd_in_ready;
    `inst_sync(algorithm_gcd, gcd, #())(`sync(in_valid, `true), .in0(a), .in1(b), .out0(c));

    assign out = {in_valid, gcd_out_valid, ~gcd_in_ready, 5'd0, c};

endmodule
