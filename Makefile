CFLAGS=-falign-functions=16 -Wall -g $(COPT)
OBJS := $(patsubst %.c, build/%.o, $(wildcard *.c))
GEN := $(patsubst %.c, gen/%.h, $(wildcard *.c))

.PHONY: all
all: eval

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
build/%.o: $(GEN) %.c
	@mkdir -p build
	gcc -MM $(CFLAGS) -Igen $*.c > build/$*.d
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

build/linenoise.o: linenoise/linenoise.c linenoise/linenoise.c
	@mkdir -p build
	gcc $(CFLASGS) -c linenoise/linenoise.c -o build/linenoise.o


# remove compilation products
clean:
	rm -f eval
	rm -rf build gen

makeheaders/makeheaders: makeheaders/makeheaders.c
	$(CC) -O -w makeheaders/makeheaders.c -o makeheaders/makeheaders

#obj/rt.o: rt.c rt.h rt_types.h
#	mkdir -p obj
#	$(CC) $(CFLAGS) -c rt.c -o obj/rt.o
#
#obj/primitive.o: primitive.c primitive.h rt.h rt_types.h
#	mkdir -p obj
#	$(CC) $(CFLAGS) -c primitive.c -o obj/primitive.o
#
#obj/eval.o: eval.c eval.h rt.h rt_types.h
#	mkdir -p obj
#	$(CC) $(CFLAGS) -c eval.c -o obj/eval.o
#
#eval: obj/eval.o obj/rt.o obj/primitive.o obj/linenoise.o
#	$(CC) $(CFLAGS) obj/rt.o obj/linenoise.o -o rt
#
#rt.h: rt.c makeheaders/makeheaders
#	./makeheaders/makeheaders rt.c
#
#.PHONY: clean
#clean:
#	rm -rf obj rt.h rt makeheaders/makeheaders
