module array (
  input wire clk,
  `output(Array, (`addrN, `intN), 0)
);
    parameter N = 16;
    parameter INIT_ADDR = 1;
    parameter INIT = 0;

    reg              `intT data[0:N-1];

    integer          i;
    initial begin
        for(i = 0; i < N; i = i + 1) begin
            data[i] = INIT_ADDR ? i : INIT;
        end
    end

    assign out0_valid = out0_ready;

    // async read
    assign out0 = data[out0_addr];

    // sync write
    always @(posedge clk) begin
        if(out0_valid && out0_we) begin
            data[out0_addr] <= out0_di;
        end
    end
endmodule
