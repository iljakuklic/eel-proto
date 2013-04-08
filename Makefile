
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
	rlwrap -S'>>> ' -pgreen ./eel -i

clean:
	rm -f eel
	cabal clean

.PHONY: clean top doc docs readdoc repl
