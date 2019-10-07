`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "tests_add_array_at.v"
`include "array.v"

module tests_add_array_at_tb;

   reg clk;

   reg `intT addr;
   reg `intT val;
   wire `addrT tests_add_array_at_arr_addr;
   wire tests_add_array_at_arr_we;
   wire `intT tests_add_array_at_arr_di;
   wire `intT arr_do;
   wire tests_add_array_at_arr_valid;
   wire arr_ready;
   wire `intT res;

   reg  in_valid;
   reg  out_ready;

   always begin
      #0.5 clk = !clk;
   end

   array arr(.clk(clk),
             .addr(tests_add_array_at_arr_addr),
             .we(tests_add_array_at_arr_we),
             .di(tests_add_array_at_arr_di),
             .do(arr_do),
             .valid(tests_add_array_at_arr_valid),
             .ready(arr_ready));

   initial begin
      $dumpfile(`dumpfile);
      $dumpvars(0, tests_add_array_at_tb);

      in_valid = `true;
      out_ready = `true;
      clk  = 0;
      addr = 3;
      val = 42;

      #100;
      $display("timed out");
      $finish;
   end

   `inst_sync(tests_add_array_at, tests_add_array_at)(
     `sync(in_valid, out_ready),
     `intf(Array, 0, arr),
     `in(int, 1, addr),
     `in(int, 2, val),
     `out(int, 1, res));

   always @(posedge clk) begin
       if(tests_add_array_at_out_valid) begin
           $display("res = %d", res);
           $finish;
       end
       if(tests_add_array_at_in_ready) begin
           in_valid <= `false;
       end
   end

endmodule // tests_add_array_at_tb
