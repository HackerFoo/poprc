`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_cycle.v"

module tests_cycle_tb;

    reg clk;
    `reg(int, dIn);
    `wire(stream, sOut);
    reg in_valid;
    reg out_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile("tests_cycle_tb.vcd");
        $dumpvars(0, tests_cycle_tb);

        dIn     = 0;
        clk     = 0;
        in_valid = `true;
        out_ready = `true;

        #1; in_valid = `false;

        #5; $display("sOut = %b (%d)", sOut, sOut);

        dIn = 1;
        in_valid = `true;
        #1; in_valid = `false;

        #5; $display("sOut = %b (%d)", sOut, sOut);

        $finish;
    end

    `inst_sync(tests_cycle, cycle)(`sync(in_valid, out_ready), `in(int, 0, dIn), `out(stream, 0, sOut));

endmodule
