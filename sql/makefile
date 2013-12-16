HSFLAGS = -O -fwarn-name-shadowing
--CLG = $(HSFLAGS) -dynamic --make -O
CLG = $(HSFLAGS) --make -O
DFLAGS = -dynamic -lsqlite3
SFLAGS = libsqlite3.a -static -optl-static -optl-pthread

PROGS=jpd

%.o %.hi : %.hs
	ghc -c $(CLG) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

jpd  : jpd.hs Sql.hs
	ghc $(CLG) -o $@ $<  $(DFLAGS)

install:
	cp -v Sql.hs ../cabal/My/
	cp -v jpd.hs ../cabal/tests/sql.hs

clean:
		-rm *.hi *.o $(PROGS)
