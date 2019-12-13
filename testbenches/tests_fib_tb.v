`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fib.v"

module tests_fib_tb;

    reg `intT a;
    wire `intT b;

    `testbench(tests_fib_tb, 2000)

    `in_ready(tests_fib);
    `inst_sync(tests_fib, tests_fib, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        a    = 10;
        `start;

        `wait_for(tests_fib_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_fib_tb
