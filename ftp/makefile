HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Ftp
TESTPROG=ftp
PROGS=jpd

all:$(PROGS)

jpd:$(MODULE).hs jpd.hs
#jpd:jpd.hs

install:
	cp -v $(MODULE).hs ../cabal/My/
	cp -v jpd.hs ../cabal/tests/$(TESTPROG).hs

clean:
	-rm *.hi *.o $(PROGS)
