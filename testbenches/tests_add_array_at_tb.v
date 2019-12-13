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

   wire `intT res;
   assign `to_bus(arr_out) = 0;

   `testbench(tests_add_array_at_tb, 100)

   array arr(.clk(clk),
             `out(Array, 0, arr_in));

   `in_ready(inst);
   `inst_sync(tests_add_array_at, inst, #())(
     `sync(in_valid, out_ready),
     `in(Array, 0, arr_in),
     `in(simple, 1, addr),
     `in(simple, 2, val),
     `out(Array, 0, arr_out),
     `out(simple, 1, res));

   initial begin
      addr = 3;
      val = 42;
      `start;

      `wait_for(inst_out_valid);
       $display("res = %d", res);
       $finish;
   end

endmodule // tests_add_array_at_tb
