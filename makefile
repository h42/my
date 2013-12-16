MYDIR = $(HOME)/haskell
HSFLAGS = -O -fwarn-name-shadowing  -i$(MYDIR)
--CLG = $(HSFLAGS) -dynamic --make -O
CLG = $(HSFLAGS) --make -O

MODS=Ftp Glob Show Sql Time Xml
PROGS=ftp glob show time xml utf8 sql

%.o %.hi : %.hs
	ghc -c $(CLG) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)
sql  : sql.hs Sql.hs
	ghc $(CLG) -o $@ $< -lsqlite3
ftp  : ftp.hs  Ftp.hs
glob : glob.hs Glob.hs
show : show.hs Show.hs
time : time.hs Time.hs
xml  : xml.hs  Xml.hs
utf8 : utf8.hs Utf8.hs

clean:
		-rm *.hi *.o $(PROGS)
