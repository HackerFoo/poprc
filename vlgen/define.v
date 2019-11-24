`ifndef __DEFINE__
`define __DEFINE__

`ifndef intN
 `define intN 32
`endif

`ifndef symN
 `define symN 1
`endif

`ifndef anyN
 `define anyN `intN
`endif

`ifndef addrN
 `define addrN 16
`endif

`define intT [`intN-1:0]
`define symT [`symN-1:0]
`define anyT [`anyN-1:0]
`define addrT [`anyN-1:0]

`define sync_ports \
  input clk, \
  input in_valid, output in_ready, \
  output out_valid, input out_ready

`define id(x) x

`define concat_(a, b) `id(a)``_``b

`define stringify(x) `"x`"

`define rename(dst, src) wire src; assign dst = src

`define top_sync(ready) \
  assign in_ready = ready

`define loop_sync(ready) \
  reg active = `false; \
  assign in_ready = ~active & ready

`define sync_wire(name) `concat_(`current_inst, name)

`define inst(t, n, p) t p t``_``n

`define apply(f, x) `f x

`define inst_sync(t, n, p) \
  `define current_inst n \
  wire `sync_wire(out_valid); \
  `inst(t, n, p)

`define start_block(name) \
`define block name \

`define end_block(name) \
`undef block \

`define sync(valid, ready) \
      .clk(clk), \
      .in_valid(valid), \
      .out_valid(`sync_wire(out_valid)), \
      .in_ready(`sync_wire(in_ready)), \
      .out_ready(ready)

`define input(type, N, index) `input_``type(N, index)
`define input_simple(N, index) input [N-1:0] `id(in)``index
`define output(type, N, index) `output_``type(N, index)
`define output_simple(N, index) output [N-1:0] `id(out)``index
`define in(type, index, name) `in_``type(index, name)
`define out(type, index, name) `out_``type(index, name)
`define in_simple(index, name) .`id(in)``index(name)
`define out_simple(index, name) .`id(out)``index(name)
`define alias(type, N, name, index) `alias_``type(N, name, index)
`define alias_simple(N, name, other) wire [N-1:0] name = other
`define variable(type, N, name, in) `variable_``type(N, name, in)
`define variable_simple(N, name, in) reg [N-1:0] name = 0
`define wire(type, N, name) `wire_``type(N, name)
`define wire_simple(N, name) wire [N-1:0] name
`define reg(type, N, name) `reg_``type(N, name)
`define reg_simple(N, name) reg [N-1:0] name
`define const(type, N, name, val) `const_``type(N, name, val)
`define const_simple(N, name, val) localparam [N-1:0] name = val

`define input_stream(N, index) input [N-1:0] in``index, input in``index``_valid, output in``index``_ready
`define output_stream(N, index) output [N-1:0] out``index, output out``index``_valid, input out``index``_ready
`define in_stream(index, name) .in``index(name), .in``index``_valid(name``_valid), .in``index``_ready(name``_ready)
`define in_null_stream(index, name) .in``index(), .in``index``_valid(name``_valid), .in``index``_ready(name``_ready)
`define out_stream(index, name) .out``index(name), .out``index``_valid(name``_valid), .out``index``_ready(name``_ready)
`define out_null_stream(index, name) .out``index(), .out``index``_valid(name``_valid), .out``index``_ready(name``_ready)
`define alias_stream(N, name, other) \
  wire [N-1:0] name = other; \
  wire name``_valid = other``_valid; wire name``_ready; \
  assign other``_ready = name``_ready
`define variable_stream(N, name, in) \
  wire [N-1:0] name = in; \
  reg name``_valid_reg; \
  wire name``_valid = in``_valid; \
  `rename(in``_ready, name``_ready)
`define wire_stream(N, name) wire [N-1:0] name; wire name``_valid; wire name``_ready
`define wire_null_stream(N, name) wire name``_valid; wire name``_ready
`define reg_stream(N, name) reg [N-1:0] name; reg name``_valid; wire name``_ready
`define const_stream(N, name, val) localparam [N-1:0] name = val; localparam name``_valid = `true
`define null_const_nil(name) localparam name``_valid = `true; wire name``_ready

`define interface(type, AN, DN, index) `interface_``type(AN, DN, index)
`define intf(type, index, name) `intf_``type(index, name)
`define bus(type, AN, DN, index, inst) `bus_``type``(AN, DN, index, inst)

/* ------------------------------------------------------ *
     ARRAY BUS
 * ------------------------------------------------------ *
     addr:  address
     we:    write enable
     di:    data in
     do:    data out
     valid: address is valid
     ready: ready for a new address

     NOTES:
      -> do is assumed valid once the address
         has been transmitted (ready & valid)
      -> all inputs to the bus must be low
         if valid is low, allowing the bus to be OR'ed
 * ------------------------------------------------------ */
`define interface_Array(AN, DN, index) \
  output [AN-1:0] intf``index``_addr, \
  output intf``index``_we, \
  output [DN-1:0] intf``index``_di, \
  input [DN-1:0] intf``index``_do, \
  output intf``index``_valid, \
  input intf``index``_ready
`define intf_Array(index, name) \
      .intf``index``_addr(`concat_(`current_inst, name``_addr)), \
      .intf``index``_we(`concat_(`current_inst, name``_we)), \
      .intf``index``_di(`concat_(`current_inst, name``_di)), \
      .intf``index``_do(name``_do), \
      .intf``index``_valid(`concat_(`current_inst, name``_valid)), \
      .intf``index``_ready(name``_ready)
`define bus_Array(AN, DN, index, inst) \
  wire [AN-1:0] inst``_intf``index``_addr; \
  wire inst``_intf``index``_we; \
  wire [DN-1:0] inst``_intf``index``_di; \
  wire inst``_intf``index``_valid

`define to_bus(pre) {pre``_addr, pre``_we, pre``_di, pre``_valid}

`define true 1'b1
`define false 1'b0
`define nil (~(`intN'd0))

`define assert(n, x) \
  `define current_inst n \
    wire `sync_wire(out_valid) = (x)

`define set(x) x <= `true
`define reset(x) x <= `false

`define valid(x) (! (& x))

`define returned_to(name) (returned && return_addr == label_``name)

// make sure all labels are declared
`default_nettype none

`endif
