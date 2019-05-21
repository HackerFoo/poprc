`ifndef __DEFINE__
`define __DEFINE__

`ifndef intN
 `define intN 32
`endif

`ifndef symN
 `define symN 1
`endif

`define intT [`intN-1:0]
`define symT [`symN-1:0]
`define set(x) x <= 1'b1
`define reset(x) x <= 1'b0

`endif
