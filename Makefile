CC=gcc
CFLAGS=-falign-functions=16 -Wall -g

.PHONY: all
all: rt

obj:
	mkdir -p obj

obj/rt.o: obj rt.c rt.h rt_types.h
	$(CC) $(CFLAGS) -c rt.c -o obj/rt.o

obj/linenoise.o: obj linenoise/linenoise.c linenoise/linenoise.c
	$(CC) $(CFLASGS) -c linenoise/linenoise.c -o obj/linenoise.o

rt: obj/rt.o obj/linenoise.o
	$(CC) $(CFLAGS) obj/rt.o obj/linenoise.o -o rt

rt.h: rt.c
	makeheaders rt.c

.PHONY: clean
clean:
	rm -f obj rt.h rt
