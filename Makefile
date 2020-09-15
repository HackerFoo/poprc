-include config.mk

SHELL := bash
ROOT := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

# defaults
BUILD ?= release-with-asserts

ifeq ($(BUILD),release)
	USE_LINENOISE ?= y
	USE_READLINE ?= n
else
	USE_LINENOISE ?= n
	USE_READLINE ?= n
endif

# ensure that USE_LINENOISE and USE_READLINE are exclusive
ifeq ($(USE_READLINE),y)
	USE_LINENOISE = n
	USE_READLINE = y
else ifeq ($(USE_LINENOISE),y)
	USE_LINENOISE = y
	USE_READLINE = n
else
	USE_LINENOISE = n
	USE_READLINE = n
endif

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	UNAME_O := $(shell uname -o)
endif

ifeq ($(CC),cc)
	CC=clang
endif

ifeq ($(findstring gcc, $(CC)),gcc)
	SANITIZE := -fsanitize=undefined
	CFLAGS = -falign-functions=4 -Wall -std=gnu99 \
                 -Wno-address-of-packed-member \
                 -Wno-unused-but-set-variable \
                 -Wno-unused-value \
                 -Wno-pointer-to-int-cast
	CXXFLAGS = -xc++ -falign-functions=4 -Wall -std=c++98
	OPT_FLAG = -O3
	LDFLAGS += -rdynamic
endif
ifeq ($(findstring clang, $(CC)),clang)
ifneq ($(UNAME_O),Android) # ubsan doesn't work on Termux
	SANITIZE := -fsanitize=undefined -fno-sanitize=bounds
endif
	CFLAGS = -Wall -Wextra -pedantic -std=gnu11 \
                 -Wno-gnu-zero-variadic-macro-arguments -Wno-address-of-packed-member \
                 -Wno-unknown-warning-option -Wno-zero-length-array -Wno-array-bounds \
                 -Werror=implicit-function-declaration -Werror=int-conversion
	CXXFLAGS = -xc++ -Wall -Wextra -pedantic -std=c++98
	OPT_FLAG = -O3
	LDFLAGS += -rdynamic
endif

ifeq ($(findstring emcc, $(CC)),emcc)
	CFLAGS = -Wall -DEMSCRIPTEN -s ALIASING_FUNCTION_POINTERS=0
	USE_LINENOISE=n
	USE_READLINE=n
	OPT_FLAG = -Os
endif

ifeq ($(FORCE_32_BIT),y)
	CFLAGS += -m32
	CXXFLAGS += -m32
	LDFLAGS += -m32
endif

ifeq ($(BUILD),debug)
	OPT_FLAG = -O0
	CFLAGS += -g3 $(OPT_FLAG) $(SANITIZE)
	CXXFLAGS += -g3 $(OPT_FLAG) $(SANITIZE)
	LIBS += $(SANITIZE)
endif

ifeq ($(BUILD),release)
	CFLAGS += -DNDEBUG $(OPT_FLAG)
	CXXFLAGS += -DNDEBUG $(OPT_FLAG)
endif

ifeq ($(BUILD),release-with-asserts)
	CFLAGS += $(OPT_FLAG)
	CXXFLAGS += $(OPT_FLAG)
endif

ifeq ($(BUILD),profile)
	CFLAGS += -g -DNDEBUG $(OPT_FLAG)
	CXXFLAGS += -g -DNDEBUG $(OPT_FLAG)
	LIBS += -lprofiler
endif

ifeq ($(BUILD),gprof)
	CFLAGS += -DNDEBUG -pg $(OPT_FLAG)
	CXXFLAGS += -DNDEBUG -pg $(OPT_FLAG)
	LDFLAGS += -pg
endif

ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

INCLUDE += -I.gen
CFLAGS += $(COPT) $(INCLUDE)
CXXFLAGS += $(COPT) $(INCLUDE)
LIBS += -lm

BUILD_DIR := build/$(CC)/$(BUILD)
DIAGRAMS := diagrams
DIAGRAMS_FILE := diagrams.html

SRC := $(wildcard *.c) \
       $(wildcard startle/*.c) \
       $(wildcard cgen/*.c) \
       $(wildcard primitive/*.c) \
       $(wildcard ir/*.c) \
       $(wildcard parse/*.c) \
       $(wildcard debug/*.c) \
       $(wildcard gen/*.c)
OBJS := $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRC))
EMCC_OBJS := $(patsubst %.c, build/emcc/$(BUILD)/%.o, $(SRC))
DEPS := $(patsubst %.c, $(BUILD_DIR)/%.d, $(SRC))
LISTS := command format op test word counter
GEN := $(patsubst %.c, .gen/%.h, $(SRC)) $(patsubst %, .gen/%_list.h, $(LISTS))
DOT := $(wildcard *.dot)
DOTSVG := $(patsubst %.dot, $(DIAGRAMS)/%.svg, $(DOT))

#PROFILE_COMMAND := -ev '426 [0 1] [[dup2 + 0xffff &b] .] swap2 1- times head'
PROFILE_COMMAND := -ana assoc100
PPROF := ~/go/bin/pprof

CFLAGS += -DPREFIX="\"$(PREFIX)\"" $(EXTRA_CFLAGS)

.PHONY: fast
fast:
	make -j all

.PHONY: all
all: test

.PHONY: testbenches
testbenches: fast
	MAKEFLAGS="" make -C testbenches all

# prevent makeheaders from trying to generate this
LOCAL_HEADERS := ./linenoise/linenoise.h

include startle/startle.mk

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

ifeq ($(UNAME_S),Darwin)
	OPEN_DIAGRAMS := open
else
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v termux-share 2> /dev/null)
endif
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v xdg-open 2> /dev/null)
endif
ifndef OPEN_DIAGRAMS
	OPEN_DIAGRAMS := $(shell command -v qutebrowser 2> /dev/null)
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

POPR_SRC := lib.ppr tests.ppr gen_ed.ppr
POPRC_RC := poprc_rc

print-%:
	@echo $* = $($*)

.PHONY: eval
eval: gen $(BUILD_DIR)/eval
	ln -fs $(BUILD_DIR)/eval $@

# link
$(BUILD_DIR)/eval: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS) $(LIBS) -o $@

# Emscripten
js/eval.js js/eval.wasm: gen
	@mkdir -p js
	make CC=emcc $(EMCC_OBJS)
	emcc $(EMCC_OBJS) -o js/eval.js \
		-Os \
		-s WASM=1 \
		-s EXPORTED_FUNCTIONS="['_main', '_emscripten_eval']" \
		-s EXTRA_EXPORTED_RUNTIME_METHODS="['ccall']" \
		--embed-file lib.ppr --embed-file tests.ppr

# fetch linenoise if it's missing
.NOTPARALLEL linenoise/linenoise.h:
	git submodule init
	git submodule update

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

.PHONY: test test_test test_tests_txt test_lib_tests_txt test_bytecode test_irc test_cgen
test: test_test test_tests_txt test_lib_tests_txt test_bytecode test_irc test_cgen

test_test: eval
	./eval -test | $(DIFF_TEST) test_output/test.log -
test_output/test.log: eval
	@mkdir -p test_output
	./eval -test > $@

test_tests_txt: eval
	./eval -param echo on < tests.txt | $(DIFF_TEST) test_output/tests.txt.log -
test_output/tests.txt.log: eval tests.txt
	@mkdir -p test_output
	./eval -param echo on < tests.txt > $@

test_lib_tests_txt: eval
	./eval -rc $(POPRC_RC) -lo lib.ppr -im -param echo on < lib_tests.txt | $(DIFF_TEST) test_output/lib_tests.txt.log -
test_output/lib_tests.txt.log: eval lib_tests.txt
	@mkdir -p test_output
	./eval -rc $(POPRC_RC) -lo lib.ppr -im -param echo on < lib_tests.txt > $@

test_bytecode: eval
	./eval -rc $(POPRC_RC) -lo $(POPR_SRC) -bc | $(DIFF_TEST) test_output/bytecode`./eval -bits -q`.log -
test_output/bytecode32.log: eval $(POPR_SRC)
	@mkdir -p test_output
	if [[ `./eval -bits` = 32 ]]; then ./eval -rc $(POPRC_RC) -lo $(POPR_SRC) -bc > $@; fi
test_output/bytecode64.log: eval $(POPR_SRC)
	@mkdir -p test_output
	if [[ `./eval -bits` = 64 ]]; then ./eval -rc $(POPRC_RC) -lo $(POPR_SRC) -bc > $@; fi
test_irc: eval
	echo ": PRIVMSG test :popr 1 2 +" | ./eval -irc popr test | $(DIFF_TEST) test_output/test_irc.log -
test_output/test_irc.log: eval
	@mkdir -p test_output
	echo ": PRIVMSG test :popr 1 2 +" | ./eval -irc popr test > $@
test_cgen: eval
	bash poprc # prepare
	sh cgen/tests.sh | $(DIFF_TEST) test_output/cgen_tests.log -
test_output/cgen_tests.log: eval
	@mkdir -p test_output
	bash poprc # prepare
	sh cgen/tests.sh > $@

.PHONY: test_output
test_output: test_output/test.log test_output/tests.txt.log test_output/lib_tests.txt.log test_output/bytecode32.log test_output/bytecode64.log test_output/test_irc.log test_output/cgen_tests.log

.PHONY: all_test_output
all_test_output:
	make clean
	make test_output
	make clean
	make FORCE_32_BIT=y test_output

.PHONY: rtags
rtags: make-eval.log
	-rc -c - < make-eval.log; true

make-eval.log: $(SRC) Makefile
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
	CPUPROFILE=eval_prof.out CPUPROFILE_FREQUENCY=10000 ./eval -lo $(POPR_SRC) -im $(PROFILE_COMMAND) -st -q
	$(PPROF) --pdf eval eval_prof.out > eval_prof.pdf

.PHONY: dbg
dbg:
	make -j BUILD=debug eval
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
	./eval -git

.PHONY: gen
gen: $(GEN)

# remove compilation products
.PHONY: clean
clean:
	rm -f eval js/eval.js js/eval.wasm
	rm -rf build .gen diagrams
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

.PHONY: wc
wc:
	wc -l {cgen,startle,primitive,ir,parse,debug,gen}/*.[ch] *.[ch] | sort -nr

.PHONY: install
install:
	install -d $(PREFIX)/share/poprc/
	install -d $(PREFIX)/bin/
	install -m 644 $(POPR_SRC) $(PREFIX)/share/poprc/
	install eval $(PREFIX)/bin/poprc
	ln -sf $(PREFIX)/bin/poprc $(PREFIX)/bin/popr

.PHONY: node2nix
node2nix:
	@mkdir -p nix/node
	cd nix/node; node2nix -i ../../package.json
