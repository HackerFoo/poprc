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
	CFLAGS = -Wall -Wextra -pedantic -std=gnu11 -Wno-gnu-zero-variadic-macro-arguments
	CXXFLAGS = -xc++ -Wall -Wextra -pedantic -std=c++98 -m32
	OPT_FLAG=-O3
endif
ifeq ($(findstring emcc, $(CC)),emcc)
	CFLAGS = -Wall -DNDEBUG -DEMSCRIPTEN -s ALIASING_FUNCTION_POINTERS=0
	USE_LINENOISE=n
	USE_READLINE=n
	OPT_FLAG=-Os
endif

ifeq ($(FORCE_32_BIT),y)
	CFLAGS += -m32
	LDFLAGS += -m32
endif

ifeq ($(BUILD),debug)
	OPT_FLAG = -O0
	CFLAGS += -g  $(OPT_FLAG) $(SANITIZE)
	CXXFLAGS += -g  $(OPT_FLAG) $(SANITIZE)
	LIBS += $(SANITIZE)
endif

ifeq ($(BUILD),debugger)
	OPT_FLAG = -O0
	CFLAGS += -g  $(OPT_FLAG) $(SANITIZE)
	CXXFLAGS += -g  $(OPT_FLAG) $(SANITIZE)
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
DIAGRAMS_FILE := diagrams.html

SRC := $(wildcard *.c)
OBJS := $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRC))
GEN := $(patsubst %.c, gen/%.h, $(SRC)) gen/word_table.h gen/test_table.h
DOT := $(wildcard *.dot)
DOTSVG := $(patsubst %.dot, $(DIAGRAMS)/%.svg, $(DOT))

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

UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Darwin)
	OPEN_DIAGRAMS := open
else
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v termux-share 2> /dev/null)
endif
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v google-chrome 2> /dev/null)
endif
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v chromium-browser 2> /dev/null)
endif
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v firefox 2> /dev/null)
endif
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
	emcc $(EMCC_OBJS) -o js/eval.js -s EXPORTED_FUNCTIONS="['_main', '_emscripten_eval']" --embed-file lib.peg

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

# compile and generate dependency info;
$(BUILD_DIR)/%.o: %.c
	@mkdir -p $(BUILD_DIR)
	@$(CC) -MM $(CFLAGS) $*.c -MG -MP -MT $(BUILD_DIR)/$*.o -MF $(BUILD_DIR)/$*.d
	@make -f Makefile.gen SRC="$(SRC)" OBJS="$(OBJS)" $(BUILD_DIR)/$*.o -s
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD_DIR)/$*.o

.SECONDARY: $(GEN)

$(BUILD_DIR)/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -c linenoise/linenoise.c -o $(BUILD_DIR)/linenoise.o

$(DIAGRAMS)/%.svg: %.dot
	@mkdir -p $(DIAGRAMS)
	dot $^ -Tsvg > $@

$(DIAGRAMS_FILE): $(DOTSVG)
ifeq ($(DOTSVG),)
	$(error "no dot files")
else
	bash scripts/wrap-svgs.sh $(sort $^) > $@
endif

.PHONY: scan
scan: clean
	make $(BUILD_DIR)/linenoise.o
	scan-build make

.PHONY: test
test: eval
	./eval -test '' -check_free no -stats no -quit | $(DIFF_TEST) test_output/test.log -
	./eval -echo yes -stats no < tests.txt | $(DIFF_TEST) test_output/tests.txt.log -
	./eval -quiet yes -stats no -lo lib.peg -lo tests.peg -all -q | $(DIFF_TEST) test_output/bytecode`./eval -pointer-bits -q`.log -

test_output/test.log: eval
	@mkdir -p test_output
	./eval -test '' -check_free no -stats no -quit > $@

test_output/tests.txt.log: eval tests.txt
	@mkdir -p test_output
	./eval -echo yes -stats no < tests.txt > $@

test_output/bytecode32.log: eval lib.peg tests.peg
	@mkdir -p test_output
	if [[ `./eval -pointer-bits -q` = 32 ]]; then ./eval -quiet yes -stats no -lo lib.peg -lo tests.peg -all -q > $@; fi

test_output/bytecode64.log: eval lib.peg tests.peg
	@mkdir -p test_output
	if [[ `./eval -pointer-bits -q` = 64 ]]; then ./eval -quiet yes -stats no -lo lib.peg -lo tests.peg -all -q > $@; fi

.PHONY: test_output
test_output: test_output/test.log test_output/tests.txt.log test_output/bytecode32.log test_output/bytecode64.log

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
	$(OPEN_DIAGRAMS) $^

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

.PHONY: dbg
dbg:
	make BUILD=debugger eval
	lldb eval || gdb --nx eval

.PHONY: autostash
autostash:
	git config pull.rebase true
	git config rebase.autoStash true

.PHONY: git-pull
git-pull:
	git pull

.PHONY: update
update: git-pull all
	./eval -git -q

# remove compilation products
.PHONY: clean
clean:
	rm -f eval js/eval.js
	rm -rf build gen diagrams
	rm -f make-eval.log compile_commands.json
	rm -f $(DIAGRAMS_FILE)
	rm -f *.dot
	rm -f eval_prof.out
	rm -rf poprc_out

.PHONY: clean-dot
clean-dot:
	rm -rf diagrams
	rm -f $(DIAGRAMS_FILE)
	rm -f *.dot
