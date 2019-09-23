`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module algorithm_gcd_tb;

   reg clk;
   reg `intT a;
   reg `intT b;
   wire `intT c;
   reg  in_valid;
   reg  out_ready;

   always begin
      #0.5 clk = !clk;
   end

   initial begin
      $dumpfile(`dumpfile);
      $dumpvars(0, algorithm_gcd_tb);

      a         = 21;
      b         = 35;

      clk       = 0;
      in_valid  = `true;
      out_ready = `true;

      #1;
      in_valid   = `false;

      #10;
      $display("c = %b (%d)", c, c);
      $finish;
   end

   `inst_sync(algorithm_gcd, gcd)(`sync(in_valid, out_ready), .in0(a), .in1(b), .out0(c));

endmodule // gcd_tb
