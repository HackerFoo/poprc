`timescale 1ns / 1ps
`define intN 27
`include "primitives.v"
`include "tests_collatz.v"

module tests_collatz_tb;

    reg `intT a;
    wire `intT b;
    wire tests_collatz_in_ready;

    `testbench(tests_collatz_tb, 200)

    `inst_sync(tests_collatz, tests_collatz, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        #1;
        nrst = `true;
        a    = 27;
        in_valid = `true;
        out_ready = `true;
        clk  = 0;

        #1;
        in_valid = `false;

        `wait_for(tests_collatz_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_collatz_tb
