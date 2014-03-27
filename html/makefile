HSFLAGS =  -fwarn-name-shadowing  #dynamic
DFLAGS =  -dynamic
SFLAGS = -static -optl-pthread -optl-static
OBJS=Html_base.o Html.o Html_def.o Cgi.o


PROGS=genx html mkthumb form color

.PHONY: ALL

ALL:$(PROGS)

genx:genx.hs Html0.hs
	ghc $(HSFLAGS) --make -o genx genx.hs

mime:mime.cc
	g++ -Wall -g mime.cc -omime

Html.hs:genx.hs Html0.hs
	ghc $(HSFLAGS) --make -o genx genx.hs
	./genx > Html.hs

html:html.hs Html.o
	ghc $(HSFLAGS) --make -o html html.hs

form:form.hs Cgi.hs genx
	ghc $(HSFLAGS) --make -o form form.hs

color:color.hs
	ghc $(HSFLAGS) --make -o color color.hs

Html.o:Html.hs
	ghc $(HSFLAGS)  -c Html.hs -o Html.o

% : %.hs
	ghc $(HSFLAGS)  --make -o $@ $<

#HTTPDIR=/srv/http/cgi-bin
HTTPDIR=/www
install:html
	install -o jerry -g jerry -m555 html form color $(HTTPDIR)/
	#-ln -s /www/html /www/index.html
	#install -m 444 -o jerry -g jerry sara.jpg plumber.jpg  $(HTTPDIR)
	#sudo install -o apache -g apache -m755 html form color /var/www/cgi-bin/
	#sudo install -m 444 -o apache -g apache sara.jpg plumber.jpg  /var/www/html

clean:
	-rm -v *.hi *.o $(PROGS) Html.hs
