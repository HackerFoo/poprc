CFLAGS=-falign-functions=16 -Wall -g $(COPT)
OBJS := $(patsubst %.c, build/%.o, $(wildcard *.c))
GEN := $(patsubst %.c, gen/%.h, $(wildcard *.c))

.PHONY: all
all: eval

debug:
	echo $(OBJS:.o=.d)

# modified from http://scottmcpeak.com/autodepend/autodepend.html

# link
eval: $(OBJS) build/linenoise.o
	gcc $(OBJS) build/linenoise.o -o eval

# pull in dependency info for *existing* .o files
-include $(OBJS:.o=.d)

# compile and generate dependency info;
# more complicated dependency computation, so all prereqs listed
# will also become command-less, prereq-less targets
#   sed:    strip the target (everything before colon)
#   sed:    remove any continuation backslashes
#   fmt -1: list words one per line
#   sed:    strip leading spaces
#   sed:    add trailing colons
build/%.o: %.c
	@mkdir -p build
	@gcc -MM $(CFLAGS) -Igen $*.c -MG -MT build/$*.o -MF build/$*.d
	@make -f Makefile.gen build/$*.o > /dev/null
	gcc -c $(CFLAGS) -Igen $*.c -o build/$*.o
	@mv -f build/$*.d build/$*.d.tmp
	@sed -e 's|.*:|$*.o:|' < build/$*.d.tmp > build/$*.d
	@sed -e 's/.*://' -e 's/\\$$//' < build/$*.d.tmp | fmt -1 | \
	  sed -e 's/^ *//' -e 's/$$/:/' >> build/$*.d
	@rm -f build/$*.d.tmp

.SECONDARY: $(GEN)

gen/%.h: %.c makeheaders/makeheaders
	@mkdir -p gen
	./makeheaders/makeheaders $*.c:gen/$*.h

linenoise/linenoise.h:
	git submodule init
	git submodule update

build/linenoise.o: linenoise/linenoise.c linenoise/linenoise.h
	@mkdir -p build
	gcc $(CFLASGS) -c linenoise/linenoise.c -o build/linenoise.o


# remove compilation products
clean:
	rm -f eval
	rm -rf build gen

makeheaders/makeheaders: makeheaders/makeheaders.c
	gcc -O -w makeheaders/makeheaders.c -o makeheaders/makeheaders
