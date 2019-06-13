`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_sum.v"

module algorithm_sum_tb;

    reg clk;
    `reg(stream, sIn);
    `wire(int, sum);
    reg in_valid;
    wire out_valid;

    always begin
        #1 clk = !clk;
    end

    initial begin
        $dumpfile("algorithm_sum_tb.vcd");
        $dumpvars(0, algorithm_sum_tb);

        sIn     = 0;
        clk     = 0;
        in_valid = `true;
        sIn_valid = `false;

        #2; sIn = 1; in_valid = `false; sIn_valid = `true;
        #2; sIn = 2; sIn_valid = `false;
        #2; sIn = 3;
        #2; sIn = 8'hff;
        #10;
        $display("sum = %b (%d)", sum, sum);
        $finish;
    end

    algorithm_sum sum_inst(`sync_top, `in(stream, 0, sIn), `out(int, 0, sum));

endmodule
