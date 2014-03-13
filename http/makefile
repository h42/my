HSFLAGS = -O -fwarn-name-shadowing -i/home/jerry/haskell/my
CLG = $(HSFLAGS) --make -threaded -rtsopts # -dynamic

PROGS=http

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

install:

clean:
	-rm *.hi *.o $(PROGS)
