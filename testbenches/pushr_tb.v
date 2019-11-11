`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"

module pushr_tb;

    reg clk;
    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    `reg(simple, `intN, dIn1);

    reg in_valid;
    reg out_ready;
    wire pushr1_in_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, pushr_tb);

        sIn       = 1;
        clk       = 0;
        dIn1      = 42;
        sIn_valid = `true;
        in_valid = `true;
        out_ready = `true;

        #1;
        in_valid = `false;

        #5;
        $finish;
    end

    always @(posedge clk) begin
        if(sIn_ready) sIn <= sIn + 1;
        $display("sIn = %b (%d), sOut = %b (%d)", sIn, sIn, sOut, sOut);
    end

    `inst_sync(__primitive_pushr1_lli, pushr1, #())
      (`sync(in_valid, out_ready),
       `in(stream, 0, sIn),
       `in(simple, 1, dIn1),
       `out(stream, 0, sOut));

endmodule
