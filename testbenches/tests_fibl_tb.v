`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module tests_fibl_tb;

    reg `intT a;
    wire `intT b;

    `testbench(tests_fibl_tb, 30)

    `in_ready(tests_fibl);
    `inst_sync(tests_fibl, tests_fibl, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        a    = 21;
        `start;

        `wait_for(tests_fibl_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_fibl_tb
