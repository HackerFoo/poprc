`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module top(
  input clk,
  input [15:0]  in,
  output [15:0] out
);

   reg  `intT  a;
   wire  `intT  a_in = in[4:0];
   wire  `intT  b;
   reg          start;
   wire         busy;

   assign out = b;

   always @(posedge clk) begin
      if(!busy && a != a_in && a_in <= 24)
         begin
            a <= a_in;
            `set(start);
         end
      else
         begin
            `reset(start);
         end
   end // always @ (posedge clk)

   tests_fibl fibl(clk, start, a, b, busy);

endmodule // top
