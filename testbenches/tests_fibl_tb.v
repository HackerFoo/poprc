`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module tests_fibl_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   reg  in_valid;
   wire out_valid;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("tests_fibl_tb.vcd");
      $dumpvars(0, tests_fibl_tb);

      a    = 21;
      in_valid = `true;
      clk  = 0;

      #3;
      in_valid = `false;

      #200;
      $display("b = %b (%d)", b, b);
      $finish;
   end

   tests_fibl tests_fibl(`sync_top, .in0(a), .out0(b));

endmodule // tests_fibl_tb
