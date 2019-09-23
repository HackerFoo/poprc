`include "tests_collatz_swbut.v"

module tests_collatz_swbut_tb;
    reg clk = 0;
    wire [15:0] in = 16'b1000_0000_0000_0001;
    wire [15:0] out;

    always begin
        #0.5 clk = !clk;
    end

    initial begin
        $dumpfile(`dumpfile);
        $dumpvars(0, tests_collatz_swbut_tb);

        #100000;
        $finish;
    end

    top top(.clk(clk), .in(in), .out(out));
endmodule
