`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_map_add1.v"

module tests_map_add1_tb;

    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    wire map_add1_in_ready;
    assign sOut_ready = out_ready;

    `testbench(tests_map_add1_tb, 20)

    `inst_sync(tests_map_add1, map_add1, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

    always @(posedge clk) begin
        sIn <= sIn + 1;
        if(nrst) $display("sIn = %b (%d), sOut = %b (%d)", sIn, sIn, sOut, sOut);
    end

    initial begin
        #1;
        nrst = `true;
        sIn = 0;
        sIn_valid = `true;
        in_valid = `true;

        #1; in_valid = `false;

        #10;
        $finish;
    end

endmodule
