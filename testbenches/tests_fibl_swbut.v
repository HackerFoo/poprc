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
   wire  `intD  a_in = in[4:0];
   wire  `intT  b;
   wire         read;
   wire         write;
   reg [26:0]   div;

   assign out = b`intD;
   initial div = 0;

   always @(posedge clk) begin
      if(in[15]) begin
         div <= div + 1;
         if(div == 0) begin
            if(a`intD == 24) begin
               a`intD <= 0;
            end
            else begin
               a`intD <= a`intD + 1;
            end
            `set(a`intR);
         end
         else begin
            `reset(a`intR);
         end // else: !if(div == 0)
      end
      else if(a != a_in && a_in <= 24)
         begin
            a <= `read(`intN, a_in);
         end
      else
         begin
            `reset(a`intR);
         end
   end // always @ (posedge clk)

   tests_fibl fibl(clk, a, b);

endmodule // top
