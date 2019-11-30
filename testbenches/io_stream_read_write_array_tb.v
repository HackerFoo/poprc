`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "io_stream_read_write_array.v"
`include "array.v"

module io_stream_read_write_array_tb;

    reg clk;

    `wire(Array, (`addrN, `intN), arr);
    wire `intT res;
    wire inst_in_ready;

    `reg(stream, `intN, sRA);
    `wire(stream, `intN, sR);
    `reg(stream, `intN, sWA);
    `reg(stream, `intN, sW);
    `wire(stream, 1, sB);
    reg in_valid;
    reg out_ready;
    assign sR_ready = out_ready;
    assign sB_ready = out_ready;

    always begin
        #0.5 clk = !clk;
    end

    array arr(.clk(clk),
              .addr(arr_addr),
              .we(arr_we),
              .di(arr_di),
              .do(arr_do),
              .valid(arr_valid),
              .ready(arr_ready));

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, io_stream_read_write_array_tb);

        #40;
        $display("timed out");
        $finish;
    end

    reg `addrT i;
    initial begin
        in_valid = `true;
        out_ready = `true;
        clk  = 0;
        sRA  = 0;
        sRA_valid = `false;
        sWA  = 0;
        sWA_valid = `false;
        sW = 0;
        sW_valid = `false;

        // write some data
        for(i = 0; i < 8; i = i + 1) begin
            sWA = i;
            sWA_valid = `true;
            sW = i * 7;
            sW_valid = `true;
            @(posedge clk);
            while(!sWA_ready || !sW_ready) begin
                @(posedge clk);
            end
        end
        sWA_valid = `false;
        sW_valid = `false;

        // read it back
        for(i = 0; i < 8; i = i + 1) begin
            sRA = i;
            sRA_valid = `true;
            @(posedge clk);
            while(!sRA_ready) begin
                @(posedge clk);
                $display("arr(%d) = %d", i, sR);
            end
        end
        sRA_valid = `false;
        $display("done");
        $finish;
    end

    `inst_sync(io_stream_read_write_array, inst, #())(
      `sync(in_valid, out_ready),
      `in(Array, 0, arr),
      `in(stream, 1, sRA),
      `in(stream, 2, sWA),
      `in(stream, 3, sW),
      `out(stream, 0, sR),
      `out(null_stream, 1, sB));

endmodule // io_stream_read_write_array_tb
