HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make -dynamic # -threaded -rtsopts # -dynamic

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

MODULE=Rand
TESTPROG=xxx
PROGS=$(TESTPROG)

all:$(PROGS)

$(TESTPROG):$(TESTPROG).hs $(MODULE).hs

install:
	cp -v $(MODULE).hs ../cabal/My/

clean:
	-rm *.hi *.o $(PROGS)
