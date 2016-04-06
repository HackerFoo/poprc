-include config.mk

# defaults
BUILD ?= debug

ifeq ($(BUILD),release)
	USE_LINENOISE ?= y
	USE_READLINE ?= n
else
	USE_LINENOISE ?= n
	USE_READLINE ?= n
endif

ifeq ($(USE_LINENOISE),y)
	USE_READLINE = n
endif

USE_LLVM ?= n

OS := $(shell uname)

ifeq ($(CC),cc)
	CC=clang
endif

ifeq ($(findstring gcc, $(CC)),gcc)
	SANITIZE := -fsanitize=undefined
	CFLAGS = -falign-functions=4 -Wall -std=gnu99
	CXXFLAGS = -xc++ -falign-functions=4 -Wall -std=c++98
	OPT_FLAG=-O3
endif
ifeq ($(findstring clang, $(CC)),clang)
	CFLAGS = -Wall -Wextra -pedantic -std=gnu11
	LINENOISE_CFLAGS = -Wno-gnu-zero-variadic-macro-arguments
	CXXFLAGS = -xc++ -Wall -Wextra -pedantic -std=c++98
	OPT_FLAG=-O3
endif
ifeq ($(findstring emcc, $(CC)),emcc)
	CFLAGS = -Wall -DNDEBUG -DEMSCRIPTEN -s ALIASING_FUNCTION_POINTERS=0
	USE_LINENOISE=n
	USE_LLVM=n
	USE_READLINE=n
	OPT_FLAG=-Os
endif

ifeq ($(BUILD),debug)
	CFLAGS += -g -O0 $(SANITIZE)
	CXXFLAGS += -g -O0 $(SANITIZE)
	LIBS += $(SANITIZE)
endif

ifeq ($(BUILD),lldb-debug)
	CFLAGS += -g -O0 $(SANITIZE)
	CXXFLAGS += -g -O0 $(SANITIZE)
	LIBS += $(SANITIZE)
	USE_LINENOISE = n
	USE_READLINE = n
endif

ifeq ($(BUILD),release)
	CFLAGS += -DNDEBUG $(OPT_FLAG)
	CXXFLAGS += -DNDEBUG $(OPT_FLAG)
endif

ifeq ($(BUILD),profile)
	CFLAGS += -DNDEBUG $(OPT_FLAG)
	CXXFLAGS += -DNDEBUG $(OPT_FLAG)
	LIBS += -lprofiler
endif

CFLAGS += $(COPT)
CXXFLAGS += $(COPT)

BUILD_DIR := build/$(CC)/$(BUILD)
DIAGRAMS := diagrams
DIAGRAMS_FILE := diagrams.pdf

SRC := $(wildcard *.c)
OBJS := $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRC))
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
	OBJS += $(BUILD_DIR)/linenoise.o
	CFLAGS += -DUSE_LINENOISE
endif

ifeq ($(USE_LLVM),y)
	OBJS += $(BUILD_DIR)/llvm.o $(BUILD_DIR)/llvm_ext.o
	CFLAGS += -DUSE_LLVM
	LLVM_COMPONENTS = engine #core mcjit native
	LLVM_CONFIG = llvm-config
	LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags | sed -e s/-I/-isystem\ /g)
	LDFLAGS += $(shell $(LLVM_CONFIG) --ldflags)
	LIBS += $(shell $(LLVM_CONFIG) --libs --system-libs $(LLVM_COMPONENTS)) -lc++
	CXXFLAGS += -isystem $(shell llvm-config --includedir)/c++/v1
endif

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	OPENPDF := open
else
	LDFLAGS += -Wl,-Teval.ld
	OPENPDF := $(shell command -v termux-share 2> /dev/null)
	OPENPDF ?= $(shell command -v evince 2> /dev/null)
endif

#DIFF_TEST := diff -u -F '^@ '
DIFF_TEST := diff -U 3

print-%:
	@echo $* = $($*)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

.PHONY: eval
eval: $(BUILD_DIR)/eval
	ln -fs $(BUILD_DIR)/eval $@

# link
$(BUILD_DIR)/eval: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $@

js/eval.js: EMCC_OBJS := $(patsubst %.c, build/emcc/$(BUILD)/%.o, $(SRC))
js/eval.js:
	make CC=emcc $(EMCC_OBJS)
	emcc $(EMCC_OBJS) -o js/eval.js -s EXPORTED_FUNCTIONS="['_eval_command', '_cells_init']"

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

$(BUILD_DIR)/llvm_ext.o: llvm_ext.cpp llvm_ext.h
	@mkdir -p $(BUILD_DIR)
	$(CC) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm_ext.cpp -o $(BUILD_DIR)/llvm_ext.o

$(BUILD_DIR)/llvm.o: llvm.cpp llvm.h llvm_ext.h rt_types.h
	@mkdir -p $(BUILD_DIR)
	$(CC) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm.cpp -o $(BUILD_DIR)/llvm.o

# compile and generate dependency info;
$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(BUILD_DIR)
	@$(CC) -MM $(CFLAGS) $*.c -MG -MP -MT $(BUILD_DIR)/$*.o -MF $(BUILD_DIR)/$*.d
	@make -f Makefile.gen OBJS="$(OBJS)" $(BUILD_DIR)/$*.o > /dev/null
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD_DIR)/$*.o

.SECONDARY: $(GEN)

$(BUILD_DIR)/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $(LINENOISE_CFLAGS) -c linenoise/linenoise.c -o $(BUILD_DIR)/linenoise.o

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
	make $(BUILD_DIR)/linenoise.o
	scan-build make

.PHONY: test
test: eval
	./eval -t test | $(DIFF_TEST) test_output/test.log -
	./eval -r tests.peg | $(DIFF_TEST) test_output/tests.peg.log -

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
	$(OPENPDF) $^

.PHONY: graph
graph: eval
	rm -f *.dot
	lldb ./eval -b -s lldb/make_graph.lldb
	make diagrams

.PHONY: profile
profile:
	make BUILD=profile test
	CPUPROFILE=eval_prof.out ./eval
	pprof --web eval eval_prof.out

.PHONY: lldb
lldb:
	make BUILD=lldb-debug eval
	lldb eval

# remove compilation products
.PHONY: clean
clean:
	rm -f eval js/eval.js
	rm -rf build gen diagrams
	rm -f make-eval.log compile_commands.json
	rm -f $(DIAGRAMS_FILE)
	rm -f *.dot
	rm -f eval_prof.out

.PHONY: clean-dot
clean-dot:
	rm -f *.dot
