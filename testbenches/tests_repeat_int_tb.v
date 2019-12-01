`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_repeat_int.v"

module tests_repeat_int_tb;

    `reg(simple, `intN, dIn);
    `wire(stream, `intN, sOut);
    wire repeat_int_in_ready;
    assign sOut_ready = out_ready;

    `testbench(tests_repeat_int_tb, 100)

    `inst_sync(tests_repeat_int, repeat_int, #())(`sync(in_valid, out_ready), `in(simple, 0, dIn), `out(stream, 0, sOut));

    initial begin
        #1;
        nrst = `true;
        dIn  = 42;
        in_valid = `true;

        #1; in_valid = `false; dIn = 0;

        #5; $display("sOut = %b (%d)", sOut, sOut);

        $finish;
    end

endmodule
