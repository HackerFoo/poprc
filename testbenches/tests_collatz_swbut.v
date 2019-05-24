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
   wire  `intT  a_in = {13'h0, in[14:0]};
   wire  `intT  b;
   reg          read;
   wire         write;
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
         if(read) begin
            `reset(read);
         end
      end
      else if(!busy) begin
         if(in[15]) begin
            div <= div + 1;
            if(div[26:12] == in[14:0]) begin
               a <= a + 1;
               div <= 0;
               `set(read);
               `set(busy);
            end
         end
         else if(a != a_in) begin
            a <= a_in;
            `set(read);
            `set(busy);
         end
      end
   end

   tests_collatz collatz(clk, read, a, b, write);

endmodule // top
