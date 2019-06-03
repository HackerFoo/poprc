`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module algorithm_gcd_tb;

   reg clk;
   reg `intT a;
   reg `intT b;
   wire `intT c;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("algorithm_gcd_tb.vcd");
      $dumpvars(0, algorithm_gcd_tb);

      a         = 21;
      b         = 35;

      clk       = 0;
      a`intR    = `true;
      b`intR    = `true;

      #5;
      a`intR    = `false;
      b`intR    = `false;

      #20;
      $display("c = %b (%d)", c, c);
      $finish;
   end

   algorithm_gcd gcd(clk, a, b, c);

endmodule // gcd_tb
