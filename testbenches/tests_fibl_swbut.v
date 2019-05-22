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
   reg          read;
   wire         write;
   reg [26:0]   div;

   assign out = b;
   initial div = 0;

   always @(posedge clk) begin
      if(in[15]) begin
         div <= div + 1;
         if(div == 0) begin
            if(a == 24) begin
               a <= 0;
            end
            else begin
               a <= a + 1;
            end
            `set(read);
         end
         else begin
            `reset(read);
         end // else: !if(div == 0)
      end
      else if(a != a_in && a_in <= 24)
         begin
            a <= a_in;
            `set(read);
         end
      else
         begin
            `reset(read);
         end
   end // always @ (posedge clk)

   tests_fibl fibl(clk, read, a, b, write);

endmodule // top
