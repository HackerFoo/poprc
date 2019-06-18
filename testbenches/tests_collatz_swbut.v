`timescale 1ns / 1ps
`define intN 27
`include "primitives.v"
`include "tests_collatz.v"

module top(
           input         clk,
           input [15:0]  in,
           output [15:0] out
           );

    reg                  `intT  a;
    wire                 `intT  a_in = {12'h0, in[14:0]};
    wire                 `intT  b;
    reg [26:0]           div = 0;
    reg [14:0]           out_reg = 0;
    `top_sync

    assign out = {~top_ready, b[14:0]};
    initial out_ready = `true;
    initial in_valid = `false;

    always @(posedge clk) begin
        if(top_valid) begin // TODO fix ready/valid
            out_reg <= b[14:0];
        end
        else /*if(top_ready)*/ begin
            if(in[15]) begin
                div <= div + 1;
                if(div[26:12] == in[14:0]) begin
                    a <= a + 1;
                    div <= 0;
                    `set(in_valid);
                end
            end
            else if(a != a_in) begin
                a <= a_in;
                `set(in_valid);
            end
        end
        if(in_valid) begin
           `reset(in_valid);
        end
    end

    `inst_sync(tests_collatz, collatz)(`sync, .in0(a), .out0(b));

endmodule
