
eel: src/*.hs src/*/*.hs
	ghc -O -XNoMonomorphismRestriction -isrc -o eel --make src/Main.hs

clean:
	rm -f eel src/*/*.{o,hi} src/*.{o,hi}
