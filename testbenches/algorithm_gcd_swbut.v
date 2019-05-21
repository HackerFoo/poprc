`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module top(
  input clk,
  input [15:0]  in,
  output [15:0] out
);

   reg  `intT  a;
   reg  `intT  b;
   wire  `intT  a_in = in[7:0];
   wire  `intT  b_in = in[15:8];
   wire  `intT  c;
   reg          start;
   wire         busy;

   assign out = {start, busy, 6'b0000000, c};

   always @(posedge clk) begin
      if(!busy && (a != a_in || b != b_in))
         begin
            a <= a_in;
            b <= b_in;
            `set(start);
         end
      else
         begin
            `reset(start);
         end
   end // always @ (posedge clk)

   algorithm_gcd gcd(clk, start, a, b, c, busy);

endmodule // top
