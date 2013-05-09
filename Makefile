CC=gcc
CFLAGS=-falign-functions=16 -Wall -g $(COPT)

.PHONY: all
all: rt

obj/rt.o: rt.c rt.h rt_types.h
	mkdir -p obj
	$(CC) $(CFLAGS) -c rt.c -o obj/rt.o

obj/linenoise.o: linenoise/linenoise.c linenoise/linenoise.c
	mkdir -p obj
	$(CC) $(CFLASGS) -c linenoise/linenoise.c -o obj/linenoise.o

rt: obj/rt.o obj/linenoise.o
	$(CC) $(CFLAGS) obj/rt.o obj/linenoise.o -o rt

rt.h: rt.c
	makeheaders rt.c

.PHONY: clean
clean:
	rm -rf obj rt.h rt
