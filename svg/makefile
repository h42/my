HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Svg
TESTPROG=svg
PROGS=jpd t1 poly

all:$(PROGS)

poly:poly.hs $(MODULE).hs

jpd:$(MODULE).hs jpd.hs
#jpd:jpd.hs

install:
	cp -v $(MODULE).hs ../cabal/My/
	cp -v jpd.hs ../cabal/tests/$(TESTPROG).hs

clean:
	-rm *.hi *.o $(PROGS)
