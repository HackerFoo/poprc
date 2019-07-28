SHELL := bash

FUZZ_N := $(strip $(shell pgrep afl-fuzz | wc -l))

FUZZ_FLAGS := -t 10000 -o output -x popr.dict

ifeq ($(FUZZ_N),0)
	FUZZ_ID := master
	FUZZ_FLAGS += -M $(FUZZ_ID)
else
	FUZZ_ID := slave$(FUZZ_N)
	FUZZ_FLAGS += -S $(FUZZ_ID)
endif

ifneq ("$(wildcard output/$(FUZZ_ID))","")
	FUZZ_FLAGS += -i -
else
	FUZZ_FLAGS += -i input
endif

CC := afl-clang

.PHONY: all
all: eval input

eval:
	make -C .. OPT_FLAG=-O3 CC=$(CC)
	ln -s ../build/$(CC)/debug/eval .

input: eval ../tests.txt
	@mkdir -p input
	cd input && ../eval -lex < ../../tests.txt | split -l 1 - test_

.PHONY: fuzz
fuzz: eval input popr.dict
	afl-fuzz $(FUZZ_FLAGS)  -- ./eval -single $(POPRC_FLAGS)

crashes.txt: $(wildcard output/*/crashes)
	(shopt -s nullglob; \
         for i in `ls -brS1 output/*/crashes/id*`; \
         do \
	   if ! (./eval -single $(POPRC_FLAGS) < $$i &> tmp.txt || false) &> /dev/null; \
           then \
             echo "___ $$i"; \
             ./eval -lex < "$$i"; \
             cat tmp.txt; \
	   fi; \
         done) \
         | tee crashes.txt
	rm -f tmp.txt

errors.txt: crashes.txt
	grep -o 'eval:.*' crashes.txt | sort | uniq -c | sort -nr | tee errors.txt

queue.txt: $(wildcard output/*/queue)
	(shopt -s nullglob; \
         for i in `ls -brS1 output/*/queue/id*`; \
         do \
	   if ! (./eval -single $(POPRC_FLAGS) < $$i &> tmp.txt || false) &> /dev/null; \
           then \
             echo "___ $$i"; \
             ./eval -lex < "$$i"; \
             cat tmp.txt; \
	   fi; \
         done) \
         | tee queue.txt
	rm tmp.txt

hangs.txt: $(wildcard output/*/hangs)
	(shopt -s nullglob; \
         for i in `ls -brS1 output/*/hangs/id*`; \
         do \
           echo "___ $$i"; \
           ./eval -lex < "$$i"; \
         done) \
         | tee hangs.txt

.PHONY: clean
clean:
	rm -f eval crashes.txt hangs.txt
	rm -rf input output
