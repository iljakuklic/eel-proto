
top: eel

dist:
	cabal configure

dist/build/eel/eel: dist src/*.hs src/*/*.hs
	cabal build

eel: dist/build/eel/eel
	cp $< $@

clean:
	rm -f eel
	cabal clean

.PHONY: clean top
