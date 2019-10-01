`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "tests_add_array_at.v"

module array (
  input clk,
  input  `addrT addr,
  input  we,
  input  `intT di,
  output reg `intT do
);
    parameter N = 16;

    reg  `intT data[0:N-1];

    integer i;
    initial begin
      for(i = 0; i < 16; i++)
        data[i] = i;
    end

    always @(posedge clk) begin
        do <= data[addr];
        if(we)
          data[addr] <= di;
    end
endmodule

module tests_add_array_at_tb;

   reg clk;

   reg `intT addr;
   reg `intT val;
   wire `addrT tests_add_array_at_arr_addr;
   wire tests_add_array_at_arr_we;
   wire `intT tests_add_array_at_arr_di;
   wire `intT arr_do;
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
             .do(arr_do));

   initial begin
      $dumpfile(`dumpfile);
      $dumpvars(0, tests_add_array_at_tb);

      in_valid = `true;
      out_ready = `true;
      clk  = 0;
      addr = 3;
      val = 42;

      #1;
      in_valid = `false;

      #10;
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
   end

endmodule // tests_add_array_at_tb
