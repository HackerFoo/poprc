module array (
  input wire clk,
  `output(Array, (`addrN, `addrN), 0)
);
    parameter N = 16;

    reg              `intT data[0:N-1];

    integer          i;
    initial begin
        for(i = 0; i < 16; i = i + 1) begin
            data[i] = i;
        end
    end

    assign out0_ready = `true;

    // async read
    assign out0_do = data[out0_addr];

    // sync write
    always @(posedge clk) begin
        if(out0_valid && out0_we) begin
            data[out0_addr] <= out0_di;
        end
    end
endmodule
