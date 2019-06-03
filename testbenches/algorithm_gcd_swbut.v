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

   assign out = {a`intR, b`intR, c`intR, 5'd0, c`intD};

   always @(posedge clk) begin
      if(a`intD != a_in || b`intD != b_in)
         begin
            a <= `read(`intN, a_in);
            b <= `read(`intN, b_in);
         end
      else
         begin
            `reset(a`intR);
            `reset(b`intR);
         end
   end // always @ (posedge clk)

   algorithm_gcd gcd(clk, a, b, c);

endmodule // top
