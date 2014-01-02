HSFLAGS = -O -fwarn-name-shadowing -i/home/jerry/haskell/my
CLG = $(HSFLAGS) --make -threaded -rtsopts # -dynamic

PROGS=netc nets demon http

#%.o : %.hs
#        ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)
cgi:cgi.cc
	g++ cgi.cc -o www/cgi-bin/cgi

nets:nets.hs Net.o Net.hi
netc:netc.hs Net.o Net.hi
Net.o Net.hi:Net.hs
	ghc -c $(CLG) -o Net.o Net.hs

demon:demon.hs Demon.o
Demon.o:Demon.hs
	ghc -c $(CLG) -o Demon.o Demon.hs

install:
	cp -v Net.hs ../cabal/My/

clean:
	-rm *.hi *.o $(PROGS)
