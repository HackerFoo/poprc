`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "io_stream_read_array.v"
`include "array.v"

module io_stream_read_array_tb;

   reg clk;

   `wire(Array, (`addrN, `intN), arr);
   `reg(stream, `intN, sIn);
   `wire(stream, `intN, sOut);
   reg in_valid;
   reg out_ready;
   wire inst_in_ready;
   assign sOut_ready = out_ready;

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
      $dumpvars(0, io_stream_read_array_tb);

      in_valid = `true;
      out_ready = `true;
      clk  = 0;
      sIn  = 0;
      sIn_valid = `true;

      #100;
      $display("timed out");
      $finish;
   end

   `inst_sync(io_stream_read_array, inst, #())(
     `sync(in_valid, out_ready),
     `in(Array, 0, arr),
     `in(stream, 1, sIn),
     `out(stream, 0, sOut));

   always @(posedge clk) begin
       // if(io_stream_read_array_out_valid) begin
       //     $display("res = %d", res);
       //     $finish;
       // end
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
