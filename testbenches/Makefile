.PHONY: all
all: $(patsubst %_tb.v, %_tb.vcd, $(wildcard *_tb.v))

%_tb.vcd: %_tb.vvp
	vvp $<

%_tb.vvp: %_tb.v %.v
	iverilog -o $@ -I../vlgen $<

tests_fibl.v: ../lib.ppr ../tests.ppr ../eval
	(cd ..; ./eval -lo lib.ppr tests.ppr -cv tests.fibl) > $@

tests_collatz.v: ../lib.ppr ../tests.ppr ../eval
	(cd ..; ./eval -lo lib.ppr tests.ppr -cv tests.collatz) > $@

algorithm_gcd.v: ../lib.ppr ../eval
	(cd ..; ./eval -lo lib.ppr -cv algorithm.gcd) > $@

../eval:
	make -C ..

.PHONY: clean
clean:
	rm -f *.vvp *.vcd tests_fibl.v tests_collatz.v algorithm_gcd.v
