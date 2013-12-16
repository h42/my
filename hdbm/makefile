MYDIR = $(HOME)/haskell
HSFLAGS = -O -fwarn-name-shadowing  -i$(MYDIR)
CLG = $(HSFLAGS) --make -O -dynamic

PROGS=tdbm jpd

%.o %.hi : %.hs
	ghc -c $(CLG) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

tdbm:hdbm.o tdbm.c
	cc -g -Wall -o tdbm tdbm.c hdbm.o -lgdbm

hdbm.o:hdbm.c
	awkinc <hdbm.c >hdbm.h
	cc -c -g -Wall hdbm.c -ohdbm.o

jpd  : jpd.hs # Hdbm.hs
	ghc $(CLG) hdbm.o -o $@ $<  -lgdbm

install:
	cp -v Hdbm.hs hdbm.c hdbm.h ../cabal/My/
	cp jpd.hs ../cabal/tests/hdbm.hs

clean:
		-rm *.hi *.o $(PROGS)
