`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fib.v"

module tests_fib_tb;

   reg clk;
   reg `intT a;
   wire `intT b;
   reg  in_valid;
   reg  out_ready;

   always begin
      #0.5 clk = !clk;
   end

   initial begin
      $dumpfile(`dumpfile);
      $dumpvars(0, tests_fib_tb);

      a    = 10;
      in_valid = `true;
      out_ready = `true;
      clk  = 0;

      #1;
      in_valid = `false;

      #2000;
      $display("timed out");
      $finish;
   end

   `inst_sync(tests_fib, tests_fib)(`sync(in_valid, out_ready), .in0(a), .out0(b));

   always @(posedge clk) begin
       if(tests_fib_out_valid) begin
           $display("b = %b (%d)", b, b);
           $finish;
       end
   end

endmodule // tests_fib_tb
