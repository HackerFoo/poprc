`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module tests_fibl_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   reg  in_valid;
   reg  out_ready;
   wire tests_fibl_in_ready;

   always begin
      #0.5 clk = !clk;
   end

   initial begin
      $dumpfile(`dumpfile);
      $dumpvars(0, tests_fibl_tb);

      a    = 21;
      in_valid = `true;
      out_ready = `true;
      clk  = 0;

      #1;
      in_valid = `false;

      #22;
      $display("b = %b (%d)", b, b);
      $finish;
   end

   `inst_sync(tests_fibl, tests_fibl, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

endmodule // tests_fibl_tb
