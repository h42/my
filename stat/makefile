HSFLAGS = -O -fwarn-name-shadowing
--CLG = $(HSFLAGS) -dynamic --make -O
CLG = $(HSFLAGS) --make -O
DFLAGS = -dynamic
SFLAGS = -static -optl-static -optl-pthread

PROGS=jpd preg

%.o %.hi : %.hs
	ghc -c $(CLG) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

jpd  : jpd.hs Stat.hs
	ghc $(CLG) -o $@ $<  $(DFLAGS)

preg:preg.hs

install:
	cp -v Stat.hs ../cabal/My/
	cp -v jpd.hs ../cabal/tests/stat.hs

clean:
	-rm *.hi *.o $(PROGS)
