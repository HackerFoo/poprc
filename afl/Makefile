SHELL := bash

FUZZ_N := $(strip $(shell pgrep afl-fuzz | wc -l))
FUZZ_FLAGS := -i input -o output -t 1000 -x popr.dict
ifeq ($(FUZZ_N),0)
	FUZZ_FLAGS += -M master
else
	FUZZ_FLAGS += -S slave$(FUZZ_N)
endif

CC := afl-clang

.PHONY: all
all: eval input output

eval:
	make -C .. OPT_FLAG=-O3 CC=$(CC)
	ln -s ../build/$(CC)/debug/eval .

input: eval ../tests.txt
	@mkdir input
	cd input && ../eval -lex < ../../tests.txt | split -l 1 - test_

output:
	@mkdir output

.PHONY: fuzz
fuzz: eval input output popr.dict
	afl-fuzz $(FUZZ_FLAGS)  -- ./eval -commands no $(POPRC_FLAGS)

crashes.txt: $(wildcard output/*/crashes)
	(shopt -s nullglob; \
         for i in output/*/crashes/id*; \
         do \
	   if ! (./eval -commands no $(POPRC_FLAGS) < "$$i" &> tmp.txt || false) &> /dev/null; \
           then \
             echo "___ $$i"; \
             ./eval -lex < "$$i"; \
             cat tmp.txt; \
	   fi; \
         done) \
         | tee crashes.txt
	rm tmp.txt

hangs.txt: $(wildcard output/*/hangs)
	(shopt -s nullglob; \
         for i in output/*/hangs/id*; \
         do \
           echo "___ $$i"; \
           ./eval -lex < "$$i"; \
         done) \
         | tee hangs.txt

.PHONY: clean
clean:
	rm -f eval crashes.txt hangs.txt
	rm -rf input output
