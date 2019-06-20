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

`define intT [`intN-1:0]
`define symT [`symN-1:0]
`define anyT [`anyN-1:0]

`define sync_ports \
  input clk, \
  input in_valid, output in_ready, \
  output out_valid, input out_ready

`define id(x) x

`define concat_(a, b) `id(a)``_``b

`define top_sync \
  reg in_valid; \
  wire active = in_valid; \
  wire top_valid; \
  wire top_ready; \
  reg out_ready; \
  `define block top

`define sync_wire(name) `concat_(`concat_(`block, `current_inst), name)

`define inst(t, n) t n

`define inst_sync(t, n) \
  `define current_inst n \
  wire `sync_wire(valid); \
  wire `sync_wire(ready); \
  `inst(t, n)

`define sync .clk(clk), .in_valid(active), .out_valid(`sync_wire(valid)), .in_ready(`sync_wire(ready)), .out_ready(out_ready)
`define input(type, index) `input_``type(index)
`define output(type, index) `output_``type(index)
`define define_simple_input(type) `define input_``type``(index) input `type``T `id(in)``index
`define define_simple_output(type) `define output_``type``(index) output `type``T `id(out)``index
`define in(type, index, name) `in_``type(index, name)
`define out(type, index, name) `out_``type(index, name)
`define define_simple_in(type) `define in_``type(index, name) .`id(in)``index(name)
`define define_simple_out(type) `define out_``type(index, name) .`id(out)``index(name)
`define alias(type, name, index) `alias_``type(name, index)
`define variable(type, name, in) `variable_``type(name, in)
`define define_simple_alias(type) `define alias_``type(name, other) wire `type``T name = other
`define define_simple_variable(type) `define variable_``type(name, in) reg `type``T name = 0
`define wire(type, name) `wire_``type(name)
`define define_simple_wire(type) `define wire_``type(name) wire `type``T name
`define reg(type, name) `reg_``type(name)
`define define_simple_reg(type) `define reg_``type(name) reg `type``T name

`define define_simple_type(type) \
  `define_simple_input(type) \
  `define_simple_output(type) \
  `define_simple_in(type) \
  `define_simple_out(type) \
  `define_simple_alias(type) \
  `define_simple_variable(type) \
  `define_simple_wire(type) \
  `define_simple_reg(type)

`define_simple_type(int)
`define_simple_type(sym)

`define input_stream(index) input `intT in``index, input in``index``_valid, output in``index``_ready
`define output_stream(index) output `intT out``index, output out``index``_valid, input out``index``_ready
`define in_stream(index, name) .in``index(name), .in``index``_valid(name``_valid), .in``index``_ready(name``_ready)
`define out_stream(index, name) .out``index(name), .out``index``_valid(name``_valid), .out``index``_ready(name``_ready)
`define alias_stream(name, other) wire `intT name = other; wire name``_valid = other``_valid; wire name``_ready; assign other``_ready = name``_ready
`define variable_stream(name, in) \
  wire `intT name = in; \
  reg name``_valid_reg; \
  wire name``_valid = in``_valid; \
  wire name``_ready; \
  assign in``_ready = name``_ready
`define wire_stream(name) wire `intT name; wire name``_valid; wire name``_ready
`define reg_stream(name) reg `intT name; reg name``_valid; wire name``_ready

`define true 1'b1
`define false 1'b0

`define assert(n, x) \
  `define current_inst n \
wire `sync_wire(valid) = (x)

`define set(x) x <= `true
`define reset(x) x <= `false

`define valid(x) (! (& x))

`endif
