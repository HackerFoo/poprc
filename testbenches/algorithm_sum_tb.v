`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_sum.v"

module algorithm_sum_tb;

    reg clk;
    `reg(stream, sIn);
    `wire(int, sum);
    reg in_valid;
    reg out_ready;
    wire sum_inst_in_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, algorithm_sum_tb);

        sIn     = 0;
        clk     = 0;
        in_valid = `true;
        out_ready = `true;
        sIn_valid = `true;

        #1; sIn = 1; in_valid = `false; sIn_valid = `true;
        #1; sIn = 2;
        #1; sIn = 3;
        #1; sIn = 8'hff;
        #1; sIn_valid = `false;
        #1;
        $display("sum = %b (%d)", sum, sum);
        $finish;
    end

    `inst_sync(algorithm_sum, sum_inst)(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(int, 0, sum));

endmodule
