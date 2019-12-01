`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "algorithm_gcd.v"

module algorithm_gcd_tb;

    reg `intT a;
    reg `intT b;
    wire `intT c;
    wire gcd_in_ready;

    `testbench(algorithm_gcd_tb, 20)

    `inst_sync(algorithm_gcd, gcd, #())(`sync(in_valid, out_ready), .in0(a), .in1(b), .out0(c));

    initial begin
        a         = 21;
        b         = 35;

        #1;
        nrst      = `true;
        in_valid  = `true;

        #1;
        in_valid   = `false;

        `wait_for(gcd_out_valid);
        $display("c = %b (%d)", c, c);
        $finish;
    end

endmodule // gcd_tb
