module array (
  input  wire        clk,
  input  wire `addrT addr,
  input  wire        we,
  input  wire `intT  di,
  output reg  `intT  do,
  input  wire        valid,
  output reg         ready
);
    parameter N = 16;

    reg  `intT data[0:N-1];

    integer i;
    initial begin
      ready = `false;
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
