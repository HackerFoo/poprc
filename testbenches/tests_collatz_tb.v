`timescale 1ns / 1ps
`define intN 27
`include "primitives.v"
`include "tests_collatz.v"

module tests_collatz_tb;

    reg `intT a;
    wire `intT b;

    `testbench(tests_collatz_tb, 200)

    `in_ready(tests_collatz);
    `inst_sync(tests_collatz, tests_collatz, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        a    = 27;
        `start;

        `wait_for(tests_collatz_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_collatz_tb
