`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"

module stream_tb;

    parameter chained = `false;

    reg clk;
    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    `wire(simple, `intN, dOut1);
    `wire(simple, `intN, dOut2);
     reg in_valid;
     reg out_ready;
     wire ap_valid;
     wire ap02_in_ready;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, stream_tb);

        sIn       = 1;
        clk       = 0;
        sIn_valid = `true;
        in_valid = `true;
        out_ready = `true;

        #1;
        in_valid = `false;
        #4;
        sIn_valid = `true;

        #5;
        $finish;
    end

    always @(posedge clk) begin
        sIn <= sIn + 1;
        $display("sIn = %b (%d), sOut = %b (%d), dOut1 = %b (%d), dOut2 = %b (%d), out_valid = %b, sOut_valid = %b",
                 sIn, sIn, sOut, sOut,
                 dOut1, dOut1, dOut2, dOut2,
                 ap_valid, sOut_valid);
    end

    if(!chained) begin
        `inst_sync(__primitive_ap02_llii, ap02, #())(
                   `sync(in_valid, out_ready),
                   `in(stream, 0, sIn),
                   `out(stream, 0, sOut),
                   `out(simple, 1, dOut1),
                   `out(simple, 2, dOut2));
        assign ap_valid = ap02_out_valid;
    end
    else begin
        `wire(stream, `intN, s);
        wire apB_in_ready;
        `inst_sync(__primitive_ap01_lli, apA, #())(`sync(in_valid, apB_in_ready), `in(stream, 0, sIn), `out(stream, 0, s), `out(simple, 1, dOut1));
        `inst_sync(__primitive_ap01_lli, apB, #())(`sync(apA_out_valid, out_ready), `in(stream, 0, s), `out(stream, 0, sOut), `out(simple, 1, dOut2));
        assign ap_valid = apA_out_valid | apB_out_valid;
    end

endmodule
