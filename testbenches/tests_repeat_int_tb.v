`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_repeat_int.v"

module tests_repeat_int_tb;

    reg clk;
    `reg(int, dIn);
    `wire(stream, sOut);
    reg in_valid;
    reg out_ready;
    assign sOut_ready = out_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile("tests_repeat_int_tb.vcd");
        $dumpvars(0, tests_repeat_int_tb);

        dIn     = 42;
        clk     = 0;
        in_valid = `true;
        out_ready = `true;

        #1; in_valid = `false; dIn = 0;

        #5; $display("sOut = %b (%d)", sOut, sOut);

        $finish;
    end

    `inst_sync(tests_repeat_int, repeat_int)(`sync(in_valid, out_ready), `in(int, 0, dIn), `out(stream, 0, sOut));

endmodule
