module algorithm_sum (
  input clk,
  input `streamT lst1_in,
  output `intT int0_out
);

  wire `streamT lst1 = lst1_in;
  localparam `intT int2 = `read(`intN, 0);
  wire `intT int3;

  // block1:
  algorithm_sum_r0 inst3(clk, int2, lst1, int3);

  wire block1_cond = 1'b1;

  assign int0_out = (int3);

endmodule

module algorithm_sum_r0 (
  input clk,
  input `intT int2_in,
  input `streamT lst1_in,
  output `intT int0_out
);

  reg active = 1'b0;
  wire read = int2_in`intR
    & lst1_in`streamR;

  reg `streamT lst1;
  reg `intT int2;
  wire `streamT lst3;
  wire `intT int4;
  wire `intT int6;
  wire `intT int9;

  // block1:
  __primitive_ap_lli inst3(lst1, lst3, int4);
  __primitive_add_iii inst6(int4, int2, int6);

  wire block1_cond = 1'b1;
  wire block9_cond = !block1_cond;

  wire ready = block9_cond;
  wire write = active && ready;

  always @(posedge clk) begin
    if(read) begin
      lst1 <= lst1_in;
      int2 <= int2_in;
      `set(active);
    end
    else if(ready) begin
       `reset(active);
    end
    else begin
      if(`true) begin
        int2 <= int6;
        lst1 <= lst3;
      end
    end
  end

  assign int0_out = `on_write(`intN, int2);

endmodule
