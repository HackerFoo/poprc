`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_zip_add.v"

module tests_zip_add_tb;

    reg clk;
    `reg(stream, `intN, sInA);
    `reg(stream, `intN, sInB);
    `wire(stream, `intN, sOut);
    reg in_valid;
    reg out_ready;
    reg toggle = 0;
    wire zip_add_in_ready;

    assign sOut_ready = out_ready;

    always begin
        #0.5 clk = !clk;
    end

    always @(posedge clk) begin
        if(!toggle)
          sInA <= sInA + 1;

        sInB_valid <= !toggle;

        if(sInB_ready)
          sInB <= sInB + 2;

        toggle <= !toggle;
        if(sOut_valid) begin
          $display("sInA = %b (%d), sInB = %b (%d), sOut = %b (%d)",
                   sInA, sInA, sInB, sInB, sOut, sOut);
        end
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, tests_zip_add_tb);

        sInA    = 0;
        sInB    = 0;
        clk     = 0;
        sInA_valid = `true;
        sInB_valid = `true;
        out_ready = `true;
        in_valid = `true;

        #1; in_valid = `false;

        #10;

        $finish;
    end

    `inst_sync(tests_zip_add, zip_add, #())(
      `sync(in_valid, out_ready),
      `in(stream, 0, sInA),
      `in(stream, 1, sInB),
      `out(stream, 0, sOut));

endmodule
