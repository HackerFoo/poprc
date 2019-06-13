`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module algorithm_gcd_tb;

   reg clk;
   reg `intT a;
   reg `intT b;
   wire `intT c;
   reg in_valid;
   wire out_valid;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("algorithm_gcd_tb.vcd");
      $dumpvars(0, algorithm_gcd_tb);

      a         = 21;
      b         = 35;

      clk       = 0;
      in_valid  = `true;

      #5;
      in_valid   = `false;

      #20;
      $display("c = %b (%d)", c, c);
      $finish;
   end

   algorithm_gcd gcd(`sync_top, .in0(a), .in1(b), .out0(c));

endmodule // gcd_tb
