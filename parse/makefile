HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make -dynamic # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Parse
TESTPROG=
PROGS=jpd

all:$(PROGS)

jpd:$(MODULE).hs jpd.hs
#jpd:jpd.hs

install:
	cp -v $(MODULE).hs ../cabal/My/
	#cp -v jpd.hs ../cabal/tests/$(TESTPROG).hs

clean:
	-rm *.hi *.o $(PROGS)
