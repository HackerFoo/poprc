`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_dup_map.v"

module tests_dup_map_tb;

    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    assign sOut_ready = out_ready;

    `testbench(tests_dup_map_tb, 100)

    `in_ready(dup_map);
    `inst_sync(tests_dup_map, dup_map, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

    always @(posedge clk) begin
        if(sIn_ready) sIn <= sIn + 1;
    end

    initial begin
        `start;
        sIn     = 0;
        sIn_valid = `true;

        #20;
        // TODO: print a result
        $finish;
    end

endmodule
