HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Glob
PROGS=jpd

all:$(PROGS)

jpd:$(MODULE).hs jpd.hs

install:
	cp -v $(MODULE).hs ../cabal/My/

clean:
	-rm *.hi *.o $(PROGS)
