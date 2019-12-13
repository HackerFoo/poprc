`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fact.v"

module tests_fact_tb;

    reg `intT a;
    wire `intT b;

    `testbench(tests_fact_tb, 50)

    `in_ready(tests_fact);
    `inst_sync(tests_fact, tests_fact, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        a    = 8;
        `start;

        `wait_for(tests_fact_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_fact_tb
