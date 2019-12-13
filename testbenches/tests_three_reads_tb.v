`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "tests_three_reads.v"
`include "array.v"

module tests_three_reads_tb;

    `wire(Array, (`addrN, `intN), arr_in);
    `wire(Array, (`addrN, `intN), arr_out);
    `reg(simple, `addrN, addr1);
    `reg(simple, `addrN, addr2);
    `reg(simple, `addrN, addr3);
    `wire(simple, `intN, data1);
    `wire(simple, `intN, data2);
    `wire(simple, `intN, data3);

    `testbench(tests_three_reads_tb, 100)

    array arr(.clk(clk),
              `out(Array, 0, arr_in));
    assign `to_bus(arr_out) = 0;

    `in_ready(inst);
    `inst_sync(tests_three_reads, inst, #())(
      `sync(in_valid, out_ready),
      `in(Array, 0, arr_in),
      `in(simple, 1, addr1),
      `in(simple, 2, addr2),
      `in(simple, 3, addr3),
      `out(Array, 0, arr_out),
      `out(simple, 1, data1),
      `out(simple, 2, data2),
      `out(simple, 3, data3));

    initial begin
      addr1 = 1;
      addr2 = 2;
      addr3 = 3;
      out_ready = `false;
      `start;

      `wait_for(inst_out_valid);
      $display("data = %d, %d, %d", data1, data2, data3);
      @(posedge clk);
      $finish;
    end

endmodule // tests_three_reads_tb
