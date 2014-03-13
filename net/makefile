HSFLAGS = -O -fwarn-name-shadowing -i/home/jerry/haskell/my
CLG = $(HSFLAGS) --make -threaded -rtsopts # -dynamic

PROGS=netc nets http

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)
cgi:cgi.cc
	g++ cgi.cc -o www/cgi-bin/cgi

http:Net.o
nets:nets.hs Net.o Net.hi
netc:netc.hs Net.o Net.hi
Net.o Net.hi:Net.hs
	ghc -c $(CLG) -o Net.o Net.hs

install:
	cp -v Net.hs ../cabal/My/

clean:
	-rm *.hi *.o $(PROGS)
