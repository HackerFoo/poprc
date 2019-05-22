`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module tests_fibl_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   reg  read;
   wire write;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("tests_fibl_tb.vcd");
      $dumpvars(0, tests_fibl_tb);

      a    = 21;
      clk  = 0;
      read = 1;

      #3;
      read = 0;

      #200;
      $display("b = %b (%d)", b, b);
      $finish;
   end

   tests_fibl tests_fibl(clk, read, a, b, write);

endmodule // tests_fibl_tb
