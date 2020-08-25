`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_map_add1.v"

module tests_map_add1_tb;

    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    assign sOut_ready = out_ready;

    `testbench(tests_map_add1_tb, 20)

    `in_ready(map_add1);
    `inst_sync(tests_map_add1, map_add1, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

    always @(posedge clk) begin
        if(sIn_ready) sIn <= sIn + 1;
        if(sOut_valid) $display("sIn = %b (%d), sOut = %b (%d)", sIn, sIn, sOut, sOut);
    end

    initial begin
        `start;
        sIn = 0;
        sIn_valid = `true;

        #10;
        $finish;
    end

endmodule
