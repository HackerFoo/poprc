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
endif

OBJS := $(patsubst %.c, build/%.o, $(wildcard *.c))
OBJS += $(patsubst %.cpp, build/%.o, $(wildcard *.cpp))
GEN := $(patsubst %.c, gen/%.h, $(wildcard *.c))

.PHONY: all
all: eval

include Makefile.gen

ifeq ($(USE_LINENOISE),y)
	OBJS += build/linenoise.o
	CFLAGS += -DUSE_LINENOISE
endif

LLVM_COMPONENTS = core jit native
LLVM_CONFIG = llvm-config-3.4
LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
#LLVM_INCLUDE = $(shell $(LLVM_CONFIG) --includedir)
LLVM_LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags)
LLVM_LIBS = $(shell $(LLVM_CONFIG) --libs $(LLVM_COMPONENTS))

debug:
	echo $(OBJS:.o=.d)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# link
eval: $(OBJS)
	$(CXX) $(OBJS) $(LLVM_LDFLAGS) $(LLVM_LIBS) -o eval

eval.js:
	make CC=emcc $(OBJS)
	emcc $(OBJS) -o eval.js -s EXPORTED_FUNCTIONS="['_eval', '_cells_init']"

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

build/llvm_ext.o: llvm_ext.cpp llvm_ext.h
	@mkdir -p build
	$(CXX) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 -Igen llvm_ext.cpp -o build/llvm_ext.o

build/llvm.o: llvm.cpp llvm.h llvm_ext.h rt_types.h
	@mkdir -p build
	$(CXX) -c $(CXXFLAGS) $(LLVM_CXXFLAGS) -O0 -Igen llvm.cpp -o build/llvm.o

# compile and generate dependency info;
build/%.o: %.c
	@mkdir -p build
	@gcc -MM $(CFLAGS) -Igen $*.c -MG -MP -MT build/$*.o -MF build/$*.d
	make -f Makefile.gen OBJS="$(OBJS)" build/$*.o > /dev/null
	$(CC) -c $(CFLAGS) -Igen $*.c -o build/$*.o

.SECONDARY: $(GEN)

build/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p build
	$(CC) $(CFLAGS) -c linenoise/linenoise.c -o build/linenoise.o

.PHONY: scan
scan: clean
	make build/linenoise.o
	scan-build make

# remove compilation products
clean:
	rm -f eval eval.js
	rm -rf build gen
