`include "define.v"

`define primitive_op1(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  `input(inT, 0), \
  `output(outT, 0) \
); \
    assign out0 = op in0; \
endmodule

`define primitive_op2(name, op, inT, outT, suf) \
module __primitive_``name``_``suf``( \
  `input(inT, 0), \
  `input(inT, 1), \
  `output(outT, 0) \
); \
    assign out0 = in0 op in1; \
endmodule

`primitive_op2(eq, ==, int, sym, yii)
`primitive_op2(neq, !=, int, sym, yii)
`primitive_op2(mod, %, int, int, iii)
`primitive_op2(div, /, int, int, iii)
`primitive_op1(not, !, sym, sym, yy)
`primitive_op2(mul, *, int, int, iii)
`primitive_op2(add, +, int, int, iii)
`primitive_op2(sub, -, int, int, iii)
`primitive_op2(shiftl, <<, int, int, iii)
`primitive_op2(shiftr, >>, int, int, iii)
`primitive_op2(gt, >, int, sym, yii)
`primitive_op2(gte, >=, int, sym, yii)
`primitive_op2(lt, <, int, sym, yii)
`primitive_op2(lte, <=, int, sym, yii)
`primitive_op1(complement, ~, int, int, ii)
`primitive_op2(bitand, &, int, int, iii)
`primitive_op2(bitor, |, int, int, iii)
`primitive_op2(bitxor, ^, int, int, iii)

// make these chainable to get ap0N
// stream end signal is in-band
module __primitive_ap01_lli(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1)
);

    assign out0 = in0;
    assign out1 = in0;
    assign out0_valid = in0_valid & ~in_valid;
    assign out_valid = in0_valid & in_valid & `valid(in0);

    assign in_ready = in0_valid & in_valid;
    assign in0_ready = out_ready & (out0_ready | in_valid);

endmodule

module __primitive_ap02_llii(
  `sync_ports,
  `input(stream, 0),
  `output(stream, 0),
  `output(int, 1),
  `output(int, 2)
);

    reg done = `false;
    reg  `intT data2 = `false; reg data2_valid = `false;

    assign in_ready = `true;
    assign in0_ready = ~done | out0_ready;

    assign out0 = in0;
    assign out1 = in0;
    assign out2 = data2;
    assign out0_valid = done;
    assign out_valid = data2_valid & `valid(in0) & `valid(data2) & ~done;

    always @(posedge clk) begin
        if(in_valid) begin
            `reset(data2_valid);
            `reset(done);
        end
        else if(in0_valid) begin
            done <= data2_valid;
            data2 <= in0;
            data2_valid <= in0_valid;
        end
    end
endmodule

// TODO stream should preempt other inputs
module __primitive_ap20_liil(
  `sync_ports,
  `input(int, 0),
  `input(int, 1),
  `input(stream, 2),
  `output(stream, 0)
);

    reg `intT data0 = 0; reg data0_valid = 0;
    assign out0 = in_valid ? (data0_valid ? data0 : in1) : in2;
    assign out0_valid = in_valid | data0_valid | in2_valid;
    assign in2_ready = ~in_valid & ~data0_valid;
    assign in_ready = ~data0_valid;
    assign out_valid = `true;

    always @(posedge clk) begin
        if(in_valid) begin
            data0 <= in0;
            `set(data0_valid);
        end
        if(data0_valid) `reset(data0_valid);
    end

endmodule

module __primitive_pushr1_lli(
  `sync_ports,
  `input(stream, 0),
  `input(int, 1),
  `output(stream, 0)
);

    assign out0 = in_valid ? in1 : in0;
    assign out0_valid = in_valid | in0_valid;
    assign in0_ready = ~in_valid;
    assign in_ready = out0_ready;
    assign out_valid = `true;

endmodule

module __primitive_pushr2_llii(
  `sync_ports,
  `input(stream, 0),
  `input(int, 1),
  `input(int, 2),
  `output(stream, 0)
);

    reg select = `false;
    assign out0 = in_valid ? (select ? in2 : in1) : in0;
    assign out0_valid = in_valid | in0_valid;
    assign in0_ready = ~in_valid;
    assign in_ready = out0_ready & ~select;
    assign out_valid = `true;

    always @(posedge clk) begin
        if(in_valid) begin
            select <= ~select;
        end
        else begin
            `reset(select);
        end
    end

endmodule

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
`define interface_Array(index) \
  output `addrT intf``index``_addr, \
  output intf``index``_we, \
  output `intT intf``index``_di, \
  input `intT intf``index``_do, \
  output intf``index``_valid, \
  input intf``index``_ready
`define intf_Array(index, name) \
      .intf``index``_addr(`concat_(`current_inst, name``_addr)), \
      .intf``index``_we(`concat_(`current_inst, name``_we)), \
      .intf``index``_di(`concat_(`current_inst, name``_di)), \
      .intf``index``_do(name``_do), \
      .intf``index``_valid(`concat_(`current_inst, name``_valid)), \
      .intf``index``_ready(name``_ready)
`define bus_Array(index, inst) \
  wire `addrT inst``_intf``index``_addr; \
  wire inst``_intf``index``_we; \
  wire `intT inst``_intf``index``_di; \
  wire inst``_intf``index``_valid

module __primitive_read_array_ooii(
  `sync_ports,
  `interface(Array, 0),
  `input(int, 1),
  `output(int, 1)
);

    reg out_valid = `false;
    assign out1 = intf0_do;
    assign in_ready = out_ready & intf0_ready;

    // intf0 is an OR'ed bus, so must be low if not valid
    assign intf0_addr = in_valid ? in1 : 0;
    assign intf0_di = 0;
    assign intf0_we = `false;
    assign intf0_valid = in_valid;

    always @(posedge clk) begin
        if(intf0_valid & intf0_ready) begin
            `set(out_valid);
        end
        else if(out_ready) begin
            `reset(out_valid);
        end
    end

endmodule

module __primitive_write_array_ooii(
  `sync_ports,
  `interface(Array, 0),
  `input(int, 1),
  `input(int, 2)
);

    reg out_valid = `false;
    assign in_ready = intf0_ready;

    // intf0 is an OR'ed bus, so must be low if not valid
    assign intf0_addr = in_valid ? in1 : 0;
    assign intf0_di = in_valid ? in2 : 0;
    assign intf0_we = in_valid;
    assign intf0_valid = in_valid;

    always @(posedge clk) begin
        if(intf0_valid & intf0_ready) begin
            `set(out_valid);
        end
        else if(out_ready) begin
            `reset(out_valid);
        end
    end

endmodule
