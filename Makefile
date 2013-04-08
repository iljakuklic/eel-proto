
WARNFLAGS=-Wall -fno-warn-missing-signatures -fno-warn-orphans
EXTFLAGS=-XNoMonomorphismRestriction
GHCFLAGS=-O -isrc $(EXTFLAGS) $(WARNFLAGS)

eel: src/*.hs src/*/*.hs
	ghc $(GHCFLAGS) -o eel --make src/Main.hs

clean:
	rm -f eel src/*/*.{o,hi} src/*.{o,hi}
