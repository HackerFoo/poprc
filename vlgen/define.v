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
`define addrT [`addrN-1:0]

`define sync_ports \
  input wire clk, input wire nrst, \
  input wire in_valid, output wire in_ready, \
  output wire out_valid, input wire out_ready

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

`define inst(t, n, p) \
  `define current_inst n \
  t p t``_``n

`define inst_sync(t, n, p) \
  `define current_inst n \
  wire `sync_wire(out_valid); \
  t p t``_``n

`define apply(f, x) `f x

`define start_block(name) \
`define block name \

`define end_block(name) \
`undef block \

`define sync(valid, ready) \
      .clk(clk), \
      .nrst(nrst), \
      .in_valid(valid), \
      .out_valid(`sync_wire(out_valid)), \
      .in_ready(`sync_wire(in_ready)), \
      .out_ready(ready)

`define fst_(x, y) (x)
`define snd_(x, y) (y)
`define fst(p) `fst_``p
`define snd(p) `snd_``p

`define input(type, N, index) `input_``type(N, index)
`define input_simple(N, index) input wire [N-1:0] `id(in)``index
`define output(type, N, index) `output_``type(N, index)
`define output_simple(N, index) output wire [N-1:0] `id(out)``index
`define in(type, index, name) `in_``type(index, name)
`define out(type, index, name) `out_``type(index, name)
`define in_simple(index, name) .`id(in)``index(name)
`define out_simple(index, name) .`id(out)``index(name)
`define alias(type, N, name, index) `alias_``type(N, name, index)
`define alias_simple(N, name, other) wire [N-1:0] name = other
`define variable(type, N, name, in) `variable_``type(N, name, in)
`define variable_simple(N, name, in) reg [N-1:0] name
`define wire(type, N, name) `wire_``type(N, name)
`define wire_simple(N, name) wire [N-1:0] name
`define reg(type, N, name) `reg_``type(N, name)
`define reg_simple(N, name) reg [N-1:0] name
`define const(type, N, name, val) `const_``type(N, name, val)
`define const_simple(N, name, val) localparam [N-1:0] name = val

`define input_stream(N, index) input wire [N-1:0] in``index, input wire in``index``_valid, output wire in``index``_ready
`define output_stream(N, index) output wire [N-1:0] out``index, output wire out``index``_valid, input wire out``index``_ready
`define output_null_stream(N, index) output wire out``index``_valid, input wire out``index``_ready
`define in_stream(index, name) .in``index(name), .in``index``_valid(name``_valid), .in``index``_ready(name``_ready)
`define in_dup_stream(index, name) .in``index(name), .in``index``_valid(`concat_(`current_inst, name``_valid)), .in``index``_ready(`concat_(`current_inst, name``_ready))
`define in_null_stream(index, name) .in``index``_valid(name``_valid), .in``index``_ready(name``_ready)
`define in_dup_null_stream(index, name) .in``index``_valid(`concat_(`current_inst, name``_valid)), .in``index``_ready(`concat_(`current_inst, name``_ready))
`define out_stream(index, name) .out``index(name), .out``index``_valid(name``_valid), .out``index``_ready(name``_ready)
`define out_null_stream(index, name) .out``index``_valid(name``_valid), .out``index``_ready(name``_ready)
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
`define const_nil(name) localparam name = 0; localparam name``_valid = `true; wire name``_ready
`define null_const_nil(name) localparam name``_valid = `true; wire name``_ready

`define in_alias(name, other) wire name; assign other = name
`define in_alias_vec(name, other) wire [$bits(other)-1:0] name; assign other = name

`define assign_stream(inst, name) \
`in_alias(inst``_``name``_ready, name``_ready); \
  wire inst``_``name``_valid = name``_valid

`define assign_Array(inst, name) \
  `assign_stream(inst, name); \
  `in_alias_vec(inst``_``name``_addr, name``_addr); \
  `in_alias_vec(inst``_``name``_di, name``_di); \
  `in_alias(inst``_``name``_we, name``_we)

/* ------------------------------------------------------ *
     ARRAY BUS
 * ------------------------------------------------------ *
     addr:        address      (*)
     we:          write enable (*)
     di:          data in      (*)
     (no suffix): data out
     valid: data out is valid
     ready: ready for data

     NOTES:
      -> when not ready, pass back signals:
         addr, we, di, and ready
      -> (*) => must be valid when ready
      -> addr, we & di must be terminated low
         to allow OR'ing and selecting with valid
 * ------------------------------------------------------ */

// master
`define input_Array(N, index) \
  output wire [`fst(N)-1:0] in``index``_addr, \
  output wire in``index``_we, \
  output wire [`snd(N)-1:0] in``index``_di, \
  input  wire [`snd(N)-1:0] in``index, \
  input  wire in``index``_valid, \
  output wire in``index``_ready

// slave
`define output_Array(N, index) \
  input  wire [`fst(N)-1:0] out``index``_addr, \
  input  wire  out``index``_we, \
  input  wire [`snd(N)-1:0] out``index``_di, \
  output wire [`snd(N)-1:0] out``index, \
  output wire out``index``_valid, \
  input  wire out``index``_ready

`define in_Array(index, name) \
      .in``index``_addr(name``_addr), \
      .in``index``_we(name``_we), \
      .in``index``_di(name``_di), \
      .in``index(name), \
      .in``index``_valid(name``_valid), \
      .in``index``_ready(name``_ready)

`define in_dup_Array(index, name) \
      .in``index``_addr(`concat_(`current_inst, name``_addr)), \
      .in``index``_we(`concat_(`current_inst, name``_we)), \
      .in``index``_di(`concat_(`current_inst, name``_di)), \
      .in``index(name), \
      .in``index``_valid(`concat_(`current_inst, name``_valid)), \
      .in``index``_ready(`concat_(`current_inst, name``_ready))

`define out_Array(index, name) \
      .out``index``_addr(name``_addr), \
      .out``index``_we(name``_we), \
      .out``index``_di(name``_di), \
      .out``index(name), \
      .out``index``_valid(name``_valid), \
      .out``index``_ready(name``_ready)

`define wire_Array(N, name) \
  wire [`fst(N)-1:0] name``_addr; \
  wire name``_we; \
  wire [`snd(N)-1:0] name``_di; \
  wire [`snd(N)-1:0] name; \
  wire name``_valid; \
  wire name``_ready

`define alias_Array(N, name, other) \
  wire [`fst(N)-1:0] name``_addr; \
  assign other``_addr = name``_addr; \
  wire name``_we; \
  assign other``_we = name``_we; \
  wire [`snd(N)-1:0] name``_di; \
  assign other``_di = name``_di; \
  wire [`snd(N)-1:0] name = other; \
  wire name``_valid = other``_valid; \
  wire name``_ready; \
  assign other``_ready = name``_ready

`define to_bus(name) {name``_addr, name``_we, name``_di, name``_ready}

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

`define testbench(tb_name, timeout) \
   reg in_valid = 0; \
   reg out_ready = 1; \
   reg nrst = 0; \
   reg clk = 0; \
   reg start = 0; \
   always begin \
      #0.5 clk = !clk; \
   end \
   initial begin \
      $dumpfile(`dumpfile); \
      $dumpvars(0, tb_name); \
      # timeout; \
      $display("timed out"); \
      $finish; \
   end \
   always @(posedge clk) begin \
      if(in_ready & in_valid) begin \
        `reset(in_valid); \
      end \
   end

`define wait_for(valid) \
    @(posedge clk); \
    while(!(valid)) @(posedge clk)

`define in_ready(inst) \
    wire inst``_in_ready; \
    wire in_ready = inst``_in_ready

`define start \
    @(posedge clk); \
    nrst = `false; \
    @(posedge clk); \
    nrst = `true; \
    @(posedge clk); \
    #0.01 in_valid = `true

`endif

`define dup_signals_stream(N, inst, name) \
    wire inst``_``name``_valid; \
    wire inst``_``name``_ready

`define dup_signals_Array(N, inst, name) \
    wire inst``_``name``_valid; \
    wire inst``_``name``_ready; \
    wire [`fst(N)-1:0] inst``_``name``_addr; \
    wire [`snd(N)-1:0] inst``_``name``_di; \
    wire inst``_``name``_we

`define dup_signals(type, N, inst, name) `dup_signals_``type(N, inst, name)
