ifndef $(USE_LINENOISE)
	USE_LINENOISE=y
endif

ifndef $(USE_LLVM)
	USE_LLVM=y
endif

ifeq ($(CC),cc)
	CC=clang
endif
ifeq ($(CC),gcc)
	CXX=g++
	CFLAGS=-falign-functions=4 -Wall -g $(COPT)
	CXXFLAGS=-falign-functions=4 -Wall -g $(COPT)
endif
ifeq ($(CC),clang)
	CXX=clang++
	CFLAGS=-Wall -g $(COPT)
	CXXFLAGS=-Wall -g -std=c++11 $(COPT)
endif
ifeq ($(CC),emcc)
	CFLAGS = -Wall -DNDEBUG -DEMSCRIPTEN $(COPT)
	USE_LINENOISE=n
	USE_LLVM=n
endif

BUILD := build/$(CC)

OBJS := $(patsubst %.c, $(BUILD)/%.o, $(wildcard *.c))
GEN := $(patsubst %.c, gen/%.h, $(wildcard *.c))

.PHONY: all
all: eval

include Makefile.gen

ifeq ($(USE_LINENOISE),y)
	OBJS += $(BUILD)/linenoise.o
	CFLAGS += -DUSE_LINENOISE
endif

ifeq ($(USE_LLVM),y)
	OBJS += $(BUILD)/llvm.o $(BUILD)/llvm_ext.o
	CFLAGS += -DUSE_LLVM
	LLVM_COMPONENTS = core mcjit native
	LLVM_CONFIG = llvm-config
	LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
	LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
	LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs $(LLVM_COMPONENTS)) -lz -lcurses
endif

debug:
	echo $(OBJS:.o=.d)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# link
eval: $(OBJS)
	$(CXX) $(OBJS) $(LLVM_LDFLAGS) $(LLVM_LIBS) -o eval

eval.js: EMCC_OBJS := $(patsubst %.c, build/emcc/%.o, $(wildcard *.c))
eval.js:
	make CC=emcc $(EMCC_OBJS)
	emcc $(EMCC_OBJS) -o eval.js -s EXPORTED_FUNCTIONS="['_eval', '_cells_init']"

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

$(BUILD)/llvm_ext.o: llvm_ext.cpp llvm_ext.h
	@mkdir -p $(BUILD)
	$(CXX) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm_ext.cpp -o $(BUILD)/llvm_ext.o

$(BUILD)/llvm.o: llvm.cpp llvm.h llvm_ext.h rt_types.h
	@mkdir -p $(BUILD)
	$(CXX) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 llvm.cpp -o $(BUILD)/llvm.o

# compile and generate dependency info;
$(BUILD)/%.o: %.c
	@mkdir -p $(BUILD)
	@gcc -MM $(CFLAGS) $*.c -MG -MP -MT $(BUILD)/$*.o -MF $(BUILD)/$*.d
	make -f Makefile.gen OBJS="$(OBJS)" $(BUILD)/$*.o > /dev/null
	$(CC) -c $(CFLAGS) $*.c -o $(BUILD)/$*.o

.SECONDARY: $(GEN)

$(BUILD)/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p $(BUILD)
	$(CC) $(CFLAGS) -c linenoise/linenoise.c -o $(BUILD)/linenoise.o

.PHONY: scan
scan: clean
	make $(BUILD)/linenoise.o
	scan-build make

# remove compilation products
clean:
	rm -f eval eval.js
	rm -rf build gen
