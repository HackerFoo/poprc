`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_map_add1.v"

module tests_map_add1_tb;

    reg clk;
    `reg(stream, sIn);
    `wire(stream, sOut);
    reg in_valid;
    reg out_ready;
    wire map_add1_in_ready;

    assign sOut_ready = out_ready;

    always begin
        #0.5 clk = !clk;
    end

    always @(posedge clk) begin
        sIn <= sIn + 1;
        $display("sIn = %b (%d), sOut = %b (%d)", sIn, sIn, sOut, sOut);
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, tests_map_add1_tb);

        sIn     = 0;
        clk     = 0;
        sIn_valid = `true;
        out_ready = `true;
        in_valid = `true;

        #1; in_valid = `false;

        #10;

        $finish;
    end

    `inst_sync(tests_map_add1, map_add1)(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

endmodule
