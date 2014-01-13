HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make -dynamic # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Log
TESTPROG=
PROGS=log

all:$(PROGS)

log:log.hs Log.hs

install:
	cp -v $(MODULE).hs ../cabal/My/

clean:
	-rm *.hi *.o $(PROGS)
