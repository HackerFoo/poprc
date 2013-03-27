rt: rt.c rt.h rt_types.h
	gcc -falign-functions=16 -g rt.c -o rt

rt.h: rt.c
	makeheaders rt.c
