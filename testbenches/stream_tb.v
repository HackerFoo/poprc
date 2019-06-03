`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"

module stream_tb;

   parameter chained = `true;

   reg clk;
   reg `intT sIn;
   wire `intT sOut;
   wire `intT dOut1;
   wire `intT dOut0;

   always begin
      #1 clk = !clk;
   end

   initial begin
      $dumpfile("stream_tb.vcd");
      $dumpvars(0, stream_tb);

      sIn       = `read(`intN, 1);
      clk       = 0;

      #2;
      sIn`intR = `false;
      #8;
      sIn`intR = `true;
      #2;
      sIn`intR = `false;

      #10;
      $finish;
   end

   always @(posedge clk) begin
       sIn <= sIn + 1;
       $display("sIn = %b (%d), sOut = %b (%d), dOut1 = %b (%d), dOut0 = %b (%d)",
                sIn, sIn`intD, sOut, sOut`intD,
                dOut1, dOut1`intD, dOut0, dOut0`intD);
   end

   if(chained) begin
       __primitive_ap02_llii ap02(clk, sIn, sOut, dOut1, dOut0);
   end
   else begin
       wire `intT s;
       __primitive_ap01_lli apA(clk, sIn, s, dOut1);
       __primitive_ap01_lli apB(clk, s, sOut, dOut0);
   end

endmodule // gcd_tb
