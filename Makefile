# defaults
ifndef $(USE_LINENOISE)
	USE_LINENOISE=n
endif

ifndef $(USE_READLINE)
	USE_READLINE=y
endif

ifndef $(USE_LLVM)
	USE_LLVM=n
endif

OS := $(shell uname)

ifeq ($(CC),cc)
	ifeq ($(OS), Darwin)
		CC=clang
	else
#default to gcc for better gdb supported
		CC=gcc
	endif
endif
ifeq ($(CC),gcc)
	CFLAGS = -falign-functions=4 -Wall -g -std=gnu99 $(COPT)
	CXXFLAGS = -xc++ -falign-functions=4 -Wall -g -std=c++98 $(COPT)
endif
ifeq ($(CC),clang)
	CFLAGS = -Wall -Wextra -pedantic -g -std=gnu11 $(COPT)
	CXXFLAGS = -xc++ -Wall -Wextra -pedantic -g -std=c++98 $(COPT)
endif
ifeq ($(CC),emcc)
	CFLAGS = -Wall -DNDEBUG -DEMSCRIPTEN $(COPT)
	USE_LINENOISE=n
	USE_LLVM=n
	USE_READLINE=n
endif

BUILD := build/$(CC)
DIAGRAMS := diagrams
DIAGRAMS_FILE := diagrams.pdf

SRC := $(wildcard *.c)
OBJS := $(patsubst %.c, $(BUILD)/%.o, $(SRC))
GEN := $(patsubst %.c, gen/%.h, $(SRC))
DOT := $(wildcard *.dot)
DOTPDF := $(patsubst %.dot, $(DIAGRAMS)/%.pdf, $(DOT))

.PHONY: all
all: test

include Makefile.gen

ifneq "$(wildcard /opt/local/lib)" ""
	LIBS += -L/opt/local/lib
endif

ifeq ($(USE_READLINE),y)
	LIBS += -lreadline
	CFLAGS += -DUSE_READLINE
endif

ifeq ($(USE_LINENOISE),y)
	OBJS += $(BUILD)/linenoise.o
	CFLAGS += -DUSE_LINENOISE
endif

ifeq ($(USE_LLVM),y)
	OBJS += $(BUILD)/llvm.o $(BUILD)/llvm_ext.o
	CFLAGS += -DUSE_LLVM
	LLVM_COMPONENTS = engine #core mcjit native
	LLVM_CONFIG = llvm-config
	LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags | sed -e s/-I/-isystem\ /g)
	LDFLAGS += $(shell $(LLVM_CONFIG) --ldflags)
	LIBS += $(shell $(LLVM_CONFIG) --libs --system-libs $(LLVM_COMPONENTS)) -lc++
	CXXFLAGS += -isystem $(shell llvm-config --includedir)/c++/v1
endif

UNAME_S := $(shell uname -s)

ifneq ($(UNAME_S),Darwin)
	LDFLAGS += -Wl,-Teval.ld
endif

print-%:
	@echo $* = $($*)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# link
eval: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $@

eval.js: EMCC_OBJS := $(patsubst %.c, build/emcc/%.o, $(SRC))
eval.js:
	make CC=emcc $(EMCC_OBJS)
	emcc $(EMCC_OBJS) -o eval.js -s EXPORTED_FUNCTIONS="['_eval', '_cells_init']"

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

$(BUILD)/llvm_ext.o: llvm_ext.cpp llvm_ext.h
	@mkdir -p $(BUILD)
	$(CC) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm_ext.cpp -o $(BUILD)/llvm_ext.o

$(BUILD)/llvm.o: llvm.cpp llvm.h llvm_ext.h rt_types.h
	@mkdir -p $(BUILD)
	$(CC) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm.cpp -o $(BUILD)/llvm.o

# compile and generate dependency info;
$(BUILD)/%.o: %.c
	@mkdir -p $(BUILD)
	@$(CC) -MM $(CFLAGS) $*.c -MG -MP -MT $(BUILD)/$*.o -MF $(BUILD)/$*.d
	@make -f Makefile.gen OBJS="$(OBJS)" $(BUILD)/$*.o > /dev/null
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD)/$*.o

.SECONDARY: $(GEN)

$(BUILD)/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c linenoise/linenoise.c -o $(BUILD)/linenoise.o

$(DIAGRAMS)/%.pdf: %.dot
	@mkdir -p $(DIAGRAMS)
	dot $^ -Tpdf > $@

$(DIAGRAMS_FILE): $(DOTPDF)
ifeq ($(DOTPDF),)
	$(error "no dot files")
else
	pdfunite $(sort $^) $@
endif

.PHONY: scan
scan: clean
	make $(BUILD)/linenoise.o
	scan-build make

.PHONY: test
test: eval
	./eval -t test | diff test_output/test.log -
	./eval -r tests.peg | diff test_output/tests.peg.log -

test_output/test.log: eval
	@mkdir -p test_output
	./eval -t test > $@

test_output/tests.peg.log: eval tests.peg
	@mkdir -p test_output
	./eval -r tests.peg > $@

.PHONY: test_output
test_output: test_output/test.log test_output/tests.peg.log

.PHONY: rtags
rtags: make-eval.log
	-rc -c - < make-eval.log; true

make-eval.log: $(SRC) Makefile Makefile.gen
	make clean
	make eval | tee make-eval.log

compile_commands.json: make-eval.log
	make rtags
	rc --dump-compilation-database > compile_commands.json

.PHONY: diagrams
diagrams: $(DIAGRAMS_FILE)
	open $^

.PHONY: graph
graph: eval
	rm -f *.dot
	lldb ./eval -b -s lldb/make_graph.lldb
	make diagrams

# remove compilation products
.PHONY: clean
clean:
	rm -f eval eval.js
	rm -rf build gen diagrams
	rm -f make-eval.log compile_commands.json
	rm -f $(DIAGRAMS_FILE)
	rm -f *.dot

.PHONE: clean-dot
clean-dot:
	rm -f *.dot
