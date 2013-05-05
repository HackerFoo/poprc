CC=gcc
CFLAGS=-falign-functions=16 -Wall -g

.PHONY: all
all: rt

rt.o: rt.c rt.h rt_types.h
	$(CC) $(CFLAGS) -c rt.c

linenoise/linenoise.o: linenoise/linenoise.c linenoise/linenoise.c
	$(CC) $(CFLASGS) -c linenoise/linenoise.c -o linenoise/linenoise.o

rt: rt.o linenoise/linenoise.o
	$(CC) $(CFLAGS) rt.o linenoise/linenoise.o -o rt

rt.h: rt.c
	makeheaders rt.c

.PHONY: clean
clean:
	rm -f *.o linenoise/*.o rt.h rt
