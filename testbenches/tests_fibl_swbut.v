`timescale 1ns / 1ps
`define intN 16
`include "primitives.v"
`include "tests_fibl.v"

module top(
           input         clk,
           input [15:0]  in,
           output [15:0] out
           );

    reg                  `intT  a = 0;
    wire                 `intT  a_in = in[4:0];
    wire                 `intT  b;
    reg [26:0]           div = 0;
    reg                  in_valid;

    assign out = b;

    always @(posedge clk) begin
        if(in[15]) begin
            div <= div + 1;
            if(div == 0) begin
                if(a == 24) begin
                    a <= 0;
                end
                else begin
                    a <= a + 1;
                end
                `set(in_valid);
            end
            else begin
                `reset(in_valid);
            end
        end
        else if(a != a_in && a_in <= 24)
          begin
              a <= a_in;
              `set(in_valid);
          end
        else
          begin
              `reset(in_valid);
          end
    end

    `inst_sync(tests_fibl, fibl, #())(`sync(in_valid, `true), .in0(a), .out0(b));

endmodule
