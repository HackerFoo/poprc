`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fib.v"

module tests_fib_tb;

    reg `intT a;
    wire `intT b;
    wire tests_fib_in_ready;

    `testbench(tests_fib_tb, 2000)

    `inst_sync(tests_fib, tests_fib, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        #1;
        nrst = `true;
        a    = 10;
        in_valid = `true;

        #1;
        in_valid = `false;

        `wait_for(tests_fib_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_fib_tb
