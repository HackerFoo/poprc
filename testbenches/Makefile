.PHONY: all
all: $(patsubst %_tb.v, %_tb.vcd, $(wildcard *_tb.v))

%_tb.vcd: %_tb.vvp
	vvp $<

%_tb.vvp: %_tb.v %.v
	iverilog -o $@ -I../vlgen $<

.PHONY: clean
clean:
	rm -f *.vvp *.vcd
