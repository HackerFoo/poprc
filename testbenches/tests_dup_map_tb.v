`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_dup_map.v"

module tests_dup_map_tb;

    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    wire dup_map_in_ready;
    assign sOut_ready = out_ready;

    `testbench(tests_dup_map_tb, 100)

    `inst_sync(tests_dup_map, dup_map, #())(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

    always @(posedge clk) begin
        if(sIn_ready) sIn <= sIn + 1;
    end

    initial begin
        #1;
        nrst = `true;
        sIn     = 0;
        in_valid = `true;
        sIn_valid = `true;

        #1; in_valid = `false;

        #20;
        // TODO: print a result
        $finish;
    end

endmodule
