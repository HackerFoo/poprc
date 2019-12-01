`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_sum.v"

module algorithm_sum_tb;

    `reg(stream, `intN, sIn);
    `wire(simple, `intN, sum);
    wire sum_inst_in_ready;

    `testbench(algorithm_sum_tb, 20)

    `inst_sync(algorithm_sum, sum_inst, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(simple, 0, sum));

    initial begin
        sIn     = 0;
        in_valid = `true;
        sIn_valid = `true;

        #1.5; nrst = `true; // *** with #1, valid drops before posedge clk
        #1; sIn = 1; in_valid = `false; sIn_valid = `true;
        #1; sIn = 2;
        #1; sIn = 3;
        #1; sIn = 8'hff;
        #1; sIn_valid = `false;

        `wait_for(sum_inst_out_valid);
        $display("sum = %b (%d)", sum, sum);
        $finish;
    end

endmodule
