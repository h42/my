
HSFLAGS = -O -fwarn-name-shadowing
CLG = $(HSFLAGS) --make -O

PROGS =ftp sql hdbm

#        ghc $(CLG) -o $@ $< -lsqlite3
%.o %.hi : %.hs
	ghc -c $(CLG) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

all:$(PROGS)

clean:
		-rm *.hi *.o $(PROGS)
