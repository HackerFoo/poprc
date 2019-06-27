`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_dup_map.v"

module tests_dup_map_tb;

    reg clk;
    `reg(stream, sIn);
    `wire(stream, sOut);
    reg in_valid;
    reg out_ready;

    always begin
        #0.5 clk = !clk;
    end

    always @(posedge clk) begin
        if(sIn_ready) sIn <= sIn + 1;
    end

    initial begin
        $dumpfile("tests_dup_map_tb.vcd");
        $dumpvars(0, tests_dup_map_tb);

        sIn     = 0;
        clk     = 0;
        in_valid = `true;
        out_ready = `true;
        sIn_valid = `true;

        #1; in_valid = `false;

        #20;
        $finish;
    end

    `inst_sync(tests_dup_map, dup_map)(`sync(in_valid, out_ready), `in(stream, 0, sIn), `out(stream, 0, sOut));

endmodule
