`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fact.v"

module tests_fact_tb;

    reg `intT a;
    wire `intT b;
    wire tests_fact_in_ready;

    `testbench(tests_fact_tb, 50)

    `inst_sync(tests_fact, tests_fact, #())(`sync(in_valid, out_ready), .in0(a), .out0(b));

    initial begin
        #1;
        nrst = `true;
        a    = 8;
        in_valid = `true;
        out_ready = `true;
        clk  = 0;

        #1;
        in_valid = `false;

        `wait_for(tests_fact_out_valid);
        $display("b = %b (%d)", b, b);
        $finish;
    end

endmodule // tests_fact_tb
