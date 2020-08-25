`timescale 1ns / 1ps
`define intN 32
`define addrN 9
`include "primitives.v"

`define axi_module tests_axil_map_w
`include "tests_axil_map_w.v"

`include "array.v"

module tests_axi_lite_slave_top
(
 input  wire        s_axi_aclk,
 input  wire        s_axi_aresetn,
 input  wire [8:0]  s_axi_awaddr,
 input  wire        s_axi_awvalid,
 output wire        s_axi_awready,
 input  wire [31:0] s_axi_wdata,
 input  wire [3:0]  s_axi_wstrb,
 input  wire        s_axi_wvalid,
 output wire        s_axi_wready,
 output wire [1:0]  s_axi_bresp,
 output wire        s_axi_bvalid,
 input  wire        s_axi_bready,
 input  wire [8:0]  s_axi_araddr,
 input  wire        s_axi_arvalid,
 output wire        s_axi_arready,
 output wire [31:0] s_axi_rdata,
 output wire [1:0]  s_axi_rresp,
 output wire        s_axi_rvalid,
 input  wire        s_axi_rready
);

    wire       clk = s_axi_aclk;
    wire       nrst = s_axi_aresetn;

    `wire(Array, (`addrN, `intN), arr);

    array #(.N(64))
      arr(.clk(clk),
          `out(Array, 0, arr));

    `wire(stream, `addrN, ar);
    `wire(stream, `addrN, aw);
    `wire(stream, `intN , w);
    `wire(stream, `intN , r);
    `wire(null_stream, 0, b);

    wire inst_in_ready;
    `inst_sync(`id(`axi_module), inst, #())(
      `sync(`true, `true),
      `in(Array, 0, arr),
      `in(stream, 1, ar),
      `in(stream, 2, aw),
      `in(stream, 3, w),
      `out(stream, 0, r),
      `out(null_stream, 1, b));

`define from_axi_hs(stream) \
    assign stream``_valid = s_axi_``stream``valid; \
    assign s_axi_``stream``ready = stream``_ready

`define from_axi(stream, signal, shift) \
    assign stream = s_axi_``stream``signal >> shift; \
    `from_axi_hs(stream)

`define to_axi_hs(stream) \
    assign s_axi_``stream``valid = stream``_valid; \
    assign stream``_ready = s_axi_``stream``ready

`define to_axi(stream, signal) \
    assign s_axi_``stream``signal = stream; \
    `to_axi_hs(stream)

    `from_axi(ar, addr, 2);
    `from_axi(aw, addr, 2);
    `from_axi(w, data, 0);
    `to_axi(r, data);
    `to_axi_hs(b);

    assign s_axi_wstrb = 4'b1111;
    assign s_axi_bresp = 2'b00; // OKAY
    assign s_axi_rresp = 2'b00; // OKAY

endmodule // tests_axi_lite_slave_top
