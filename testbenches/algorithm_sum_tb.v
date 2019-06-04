`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_sum.v"

module algorithm_sum_tb;

   reg clk;
   reg `intT sIn;
   wire `intT sum;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("algorithm_sum_tb.vcd");
      $dumpvars(0, algorithm_sum_tb);

      sIn     = `read(`intN, 0);
      clk     = 0;

      #2; sIn = 1;
      #2; sIn = 2;
      #2; sIn = 3;
      #2; sIn = 8'hff;
      #10;
      $display("sum = %b (%d)", sum, sum`intD);
      $finish;
   end

   algorithm_sum sum_inst(clk, sIn, sum);

endmodule
