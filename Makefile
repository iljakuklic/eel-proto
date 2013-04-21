
DOC=dist/doc/html/eel/eel/index.html

top: eel
doc: $(DOC)
docs: doc

readdoc: doc
	xdg-open $(DOC)

dist:
	cabal configure

$(DOC): dist
	cabal haddock --executables

dist/build/eel/eel: dist src/*.hs src/*/*.hs
	cabal build

eel: dist/build/eel/eel
	cp $< $@

repl: eel
	@rlwrap -O'^>>>' -w5 -pgreen ./eel -i samples/brainfuck.eel

clean:
	rm -f eel
	cabal clean

wc:
	wc samples/*.* lib/*.eel
	wc src/*.hs src/*/*.hs

.PHONY: clean top doc docs readdoc repl wc
