`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "tests_add_array_at.v"
`include "array.v"

module tests_add_array_at_tb;

   reg `intT addr;
   reg `intT val;

   `wire(Array, (`addrN, `intN), arr_in);
   `wire(Array, (`addrN, `intN), arr_out);

   wire inst_in_ready;
   wire `intT res;

   `testbench(tests_add_array_at_tb, 100)

   array arr(.clk(clk),
             .addr(arr_in_addr),
             .we(arr_in_we),
             .di(arr_in_di),
             .do(arr_in_do),
             .valid(arr_in_valid),
             .ready(arr_in_ready));

   `inst_sync(tests_add_array_at, inst, #())(
     `sync(in_valid, out_ready),
     `in(Array, 0, arr_in),
     `in(simple, 1, addr),
     `in(simple, 2, val),
     `out(Array, 0, arr_out),
     `out(simple, 1, res));

   initial begin
      #1;
      nrst = `true;
      in_valid = `true;
      clk  = 0;
      addr = 3;
      val = 42;
   end

   always @(posedge clk) begin
       if(inst_out_valid) begin
           $display("res = %d", res);
           $finish;
       end
       if(inst_in_ready) begin
           in_valid <= `false;
       end
   end

endmodule // tests_add_array_at_tb
