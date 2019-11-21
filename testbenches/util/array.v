module array (
  input clk,
  input  `addrT addr,
  input  we,
  input  `intT di,
  output reg `intT do,
  input valid,
  output ready
);
    parameter N = 16;

    reg  `intT data[0:N-1];
    reg  ready = `false;

    integer i;
    initial begin
      for(i = 0; i < 16; i = i + 1)
        data[i] = i;
    end

    always @(posedge clk) begin

        // to test valid/ready logic
        ready <= ~ready;

        do <= data[addr];
        if(we)
          data[addr] <= di;
    end
endmodule
