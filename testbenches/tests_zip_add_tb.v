`timescale 1ns / 1ps
`define intN 8
`include "primitives.v"
`include "tests_zip_add.v"

module tests_zip_add_tb;

    `reg(stream, `intN, sInA);
    `reg(stream, `intN, sInB);
    `wire(stream, `intN, sOut);
    assign sOut_ready = out_ready;

    `testbench(tests_zip_add_tb, 100)

    `in_ready(zip_add);
    `inst_sync(tests_zip_add, zip_add, #())(
      `sync(in_valid, out_ready),
      `in(stream, 0, sInA),
      `in(stream, 1, sInB),
      `out(stream, 0, sOut));

    always @(posedge clk) begin
        if(sInA_valid) begin
            if(sInA_ready) sInA <= sInA + 1;
            if(sInB_ready) sInB <= sInB + 2;
            sInB_valid <= !sInB_valid;
        end
        if(sOut_valid) begin
          $display("sInA = %b (%d), sInB = %b (%d), sOut = %b (%d)",
                   sInA, sInA, sInB, sInB, sOut, sOut);
        end
    end

    initial begin
        `start;
        sInA = 0;
        sInB = 0;
        sInA_valid = `true;
        sInB_valid = `true;

        #10;
        $finish;
    end

endmodule
