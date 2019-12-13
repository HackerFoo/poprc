`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "io_stream_read_write_array.v"
`include "array.v"

module io_stream_read_write_array_tb;

    `wire(Array, (`addrN, `intN), arr);
    wire `intT res;

    `reg(stream, `intN, sRA);
    `wire(stream, `intN, sR);
    `reg(stream, `intN, sWA);
    `reg(stream, `intN, sW);
    `wire(stream, 1, sB);
    assign sR_ready = out_ready;
    assign sB_ready = out_ready;

    `testbench(io_stream_read_write_array_tb, 40)

    array arr(.clk(clk),
              `out(Array, 0, arr));

    `in_ready(inst);
    `inst_sync(io_stream_read_write_array, inst, #())(
      `sync(in_valid, out_ready),
      `in(Array, 0, arr),
      `in(stream, 1, sRA),
      `in(stream, 2, sWA),
      `in(stream, 3, sW),
      `out(stream, 0, sR),
      `out(null_stream, 1, sB));

    reg `addrT i;
    initial begin
        sRA  = 0;
        sRA_valid = `false;
        sWA  = 0;
        sWA_valid = `false;
        sW = 0;
        sW_valid = `false;
        `start;

        // write some data
        for(i = 0; i < 8; i = i + 1) begin
            sWA = i;
            sWA_valid = `true;
            sW = i * 7;
            sW_valid = `true;
            `wait_for(sWA_ready && sW_ready);
        end
        sWA_valid = `false;
        sW_valid = `false;

        // read it back
        sRA_valid = `true;
        for(i = 0; i < 8; i = i + 1) begin
            sRA = i;
            @(posedge clk);
            `wait_for(sR_valid);
            $display("arr(%d) = %d", i, sR);
        end
        sRA_valid = `false;
        nrst = `false;
        #2;
        $display("done");
        $finish;
    end

endmodule // io_stream_read_write_array_tb
