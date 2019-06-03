`timescale 1ns / 1ps
`define intN 27
`include "primitives.v"
`include "tests_collatz.v"

module top(
  input clk,
  input [15:0]  in,
  output [15:0] out
);

   reg  `intT  a;
   wire  `intD  a_in = {12'h0, in[14:0]};
   wire  `intT  b;
   reg [26:0]   div;
   reg          busy = 1'b0;
   reg [14:0]   out_reg;

   initial div       = 0;
   initial out_reg   = 0;
   assign out = {busy, out_reg};

   always @(posedge clk) begin
      if(write) begin
         out_reg <= b[14:0];
         `reset(busy);
      end
      if(busy) begin
         if(a`intR) begin
            `reset(a`intR);
         end
      end
      else if(!busy) begin
         if(in[15]) begin
            div <= div + 1;
            if(div[26:12] == in[14:0]) begin
               a`intD <= a`intD + 1;
               div <= 0;
               `set(a`intR);
               `set(busy);
            end
         end
         else if(a != a_in) begin
            a <= read(`intN, a_in);
            `set(busy);
         end
      end
   end

   tests_collatz collatz(clk, a, b);

endmodule // top
