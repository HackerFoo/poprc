ifeq ($(CC),cc)
	CC=clang
endif
ifeq ($(CC),gcc)
	CFLAGS=-falign-functions=4 -Wall -g $(COPT)
else
	CFLAGS=-Wall $(COPT)
endif
ifeq ($(CC),emcc)
	CFLAGS += -DNDEBUG -DEMSCRIPTEN
endif

OBJS := $(patsubst %.c, build/%.o, $(wildcard *.c))
GEN := $(patsubst %.c, gen/%.h, $(wildcard *.c))

.PHONY: all
all: eval

include Makefile.gen

ifeq ($(USE_LINENOISE),y)
	OBJS += build/linenoise.o
	CFLAGS += -DUSE_LINENOISE
endif

debug:
	echo $(OBJS:.o=.d)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# link
eval: $(OBJS)
	$(CC) $(OBJS) -o eval

eval.js:
	make CC=emcc $(OBJS)
	emcc $(OBJS) -o eval.js -s EXPORTED_FUNCTIONS="['_eval']"

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

# compile and generate dependency info;
build/%.o: %.c
	@mkdir -p build
	@gcc -MM $(CFLAGS) -Igen $*.c -MG -MP -MT build/$*.o -MF build/$*.d
	@make -f Makefile.gen build/$*.o > /dev/null
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
	rm -f eval
	rm -rf build gen
