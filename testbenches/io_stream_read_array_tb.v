`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "io_stream_read_array.v"
`include "array.v"

module io_stream_read_array_tb;

    `wire(Array, (`addrN, `intN), arr);
    `reg(stream, `intN, sIn);
    `wire(stream, `intN, sOut);
    wire inst_in_ready;
    assign sOut_ready = out_ready;

    `testbench(io_stream_read_array_tb, 100)

    array arr(.clk(clk),
              .addr(arr_addr),
              .we(arr_we),
              .di(arr_di),
              .do(arr_do),
              .valid(arr_valid),
              .ready(arr_ready));

    `inst_sync(io_stream_read_array, inst, #())(
      `sync(in_valid, out_ready),
      `in(Array, 0, arr),
      `in(stream, 1, sIn),
      `out(stream, 0, sOut));

    initial begin
      sIn  = 0;

      #1;
      nrst = `true;
      in_valid = `true;
      sIn_valid = `true;
   end

    always @(posedge clk) begin
        if(inst_in_ready) begin
            `reset(in_valid);
        end
        if(sIn_ready) begin
            sIn <= (sIn + 1) % 16;
        end
        if(sOut_valid) begin
            $display("out0 = %d", sOut);
            if(sOut == 15) begin
                $display("done");
                $finish;
            end
        end
    end

endmodule // io_stream_read_array_tb
