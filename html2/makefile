HSFLAGS = -O -fwarn-name-shadowing  #dynamic
SFLAGS = -static -optl-static -optl-pthread
OBJS=Html_base.o Html.o Html_def.o Cgi.o

PROGS=genx html

.PHONY: ALL

ALL:$(PROGS)

genx:genx.hs
	ghc $(HSFLAGS) --make -o genx genx.hs

html:html.hs Html.o
	ghc $(HSFLAGS) --make -o html html.hs

Html.o:Html.hs
	ghc $(HSFLAGS) -c Html.hs -o Html.o

Html.hs:genx
	genx > Html.hs

% : %.hs
	ghc $(HSFLAGS) --make -o $@ $<

clean:
	-rm -v *.hi *.o $(PROGS) Html.hs
