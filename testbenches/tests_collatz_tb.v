`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_collatz.v"

module tests_collatz_tb;

    reg clk;
    reg `intT a;
    wire `intT b;
    reg in_valid;
    reg out_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, tests_collatz_tb);

        a    = 27;
        in_valid = `true;
        out_ready = `true;
        clk  = 0;

        #1;
        in_valid = `false;

        #115;
        $display("b = %b (%d)", b, b);
        $finish;
    end

    `inst_sync(tests_collatz, tests_collatz)(`sync(in_valid, out_ready), .in0(a), .out0(b));

endmodule // tests_collatz_tb
