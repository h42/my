HSFLAGS =  -fwarn-name-shadowing  #dynamic
DFLAGS = -dynamic
SFLAGS = -static -optl-static -optl-pthread


PROGS=jpd par passwd

.PHONY: ALL

ALL:$(PROGS)

jpd:jpd.hs
	ghc $(HSFLAGS) $(DFLAGS) --make -o jpd jpd.hs

% : %.hs
	ghc $(HSFLAGS) $(DFLAGS) --make -o $@ $<

clean:
	-rm -v *.hi *.o $(PROGS)
