`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_collatz.v"

module tests_collatz_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   reg  read;
   wire write;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("tests_collatz_tb.vcd");
      $dumpvars(0, tests_collatz_tb);

      a    = 27;
      clk  = 0;
      read = 1;

      #3;
      read = 0;

      #300;
      $display("b = %b (%d)", b, b);
      $finish;
   end

   tests_collatz tests_collatz(clk, read, a, b, write);

endmodule // tests_collatz_tb
