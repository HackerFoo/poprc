`timescale 1ns / 1ps
`define intN 8
`define addrN 8
`include "primitives.v"
`include "tests_three_writes.v"
`include "array.v"

module tests_three_writes_tb;

    `wire(Array, (`addrN, `intN), arr_in);
    `wire(Array, (`addrN, `intN), arr_out);
    `reg(simple, `addrN, addr1);
    `reg(simple, `addrN, addr2);
    `reg(simple, `addrN, addr3);
    `reg(simple, `intN, data1);
    `reg(simple, `intN, data2);
    `reg(simple, `intN, data3);

    `testbench(tests_three_writes_tb, 20)

    array arr(.clk(clk),
              `out(Array, 0, arr_in));
    assign `to_bus(arr_out) = 1;

    always @(posedge clk) begin
       if(arr_in_valid & arr_in_we & arr_in_ready) begin
          $display("arr[%d] <= %d", arr_in_addr, arr_in_di);
       end
    end

    `in_ready(inst);
    `inst_sync(tests_three_writes, inst, #())(
      `sync(in_valid, out_ready),
      `in(Array, 0, arr_in),
      `in(simple, 1, addr1),
      `in(simple, 2, data1),
      `in(simple, 3, addr2),
      `in(simple, 4, data2),
      `in(simple, 5, addr3),
      `in(simple, 6, data3),
      `out(Array, 0, arr_out));

    initial begin
      addr1 = 1;
      data1 = 10;
      addr2 = 2;
      data2 = 20;
      addr3 = 3;
      data3 = 30;
      `start;

      `wait_for(arr_out_valid);
      @(posedge clk);
      $finish;
    end

endmodule // tests_three_writes_tb
