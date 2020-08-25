`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_sum.v"

module algorithm_sum_tb;

    `reg(stream, `intN, sIn);
    `wire(simple, `intN, sum);

    `testbench(algorithm_sum_tb, 20)

    `in_ready(sum_inst);
    `inst_sync(algorithm_sum, sum_inst, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(simple, 0, sum));

    initial begin
        sIn = 0;
        `start;

        #1; sIn = 1; sIn_valid = `true;
        #1; sIn = 2;
        #1; sIn = 3;
        #1; sIn = 8'hff;

        `wait_for(sum_inst_out_valid);
        $display("sum = %b (%d)", sum, sum);
        $finish;
    end

endmodule
