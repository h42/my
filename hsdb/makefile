CFLAGS = -g -Wall
HSFLAGS = -fwarn-name-shadowing -dynamic

OBJS =
MODS=Hdb.hs

ALL:jpd hdb

hdb:hdb.hs $(MODS)
	ghc --make $(HSFLAGS) hdb.hs -o hdb

jpd : jpd.c jdb.h $(OBJS)

jdb.o jdb.h : jdb.c
	awkinc <jdb.c >jdb.h
	gcc -c $(CFLAGS) jdb.c -ojdb.o

clean:
	rm *.o *.h jpd
