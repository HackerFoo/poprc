`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_collatz.v"

module tests_collatz_tb;

    reg clk;
    reg `intT a;
    wire `intT b;
    `top_sync

    always begin
        #1 clk = !clk;
    end

    initial begin
        $dumpfile("tests_collatz_tb.vcd");
        $dumpvars(0, tests_collatz_tb);

        a    = 27;
        in_valid = `true;
        out_ready = `true;
        clk  = 0;

        #3;
        in_valid = `false;

        #300;
        $display("b = %b (%d)", b, b);
        $finish;
    end

    tests_collatz tests_collatz(`sync, .in0(a), .out0(b));

endmodule // tests_collatz_tb
