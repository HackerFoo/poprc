`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module tests_fibl_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   `top_sync

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("tests_fibl_tb.vcd");
      $dumpvars(0, tests_fibl_tb);

      a    = 21;
      in_valid = `true;
      out_ready = `true;
      clk  = 0;

      #3;
      in_valid = `false;

      #200;
      $display("b = %b (%d)", b, b);
      $finish;
   end

   `inst_sync(tests_fibl, tests_fibl)(`sync, .in0(a), .out0(b));

endmodule // tests_fibl_tb
