
DOC=dist/doc/html/eel/eel/index.html
HSFILES=src/*.hs src/*/*.hs

top: eel
doc: $(DOC)
docs: doc

readdoc: doc
	xdg-open $(DOC)

dist:
	cabal configure

$(DOC): dist $(HSFILES)
	cabal haddock --executables --internal

dist/build/eel/eel: dist $(HSFILES)
	cabal build

eel: dist/build/eel/eel
	cp $< $@

repl: eel
	rlwrap -O'^[0-9]+>' -w5 -pgreen ./eel -i $(ARGS)

clean:
	rm -f eel
	rm -rf demo
	cabal clean

profrun: eel
	./eel test.eel +RTS -i0.005 -hy -p
	hp2ps -c eel.hp

prof:
	cabal configure --enable-executable-profiling --ghc-option=-prof --ghc-option=-auto-all --ghc-option=-rtsopts

wc:
	wc samples/*.* lib/*.eel
	wc src/*.hs src/*/*.hs

demo:
	mkdir -p demo



.PHONY: prof clean top doc docs readdoc repl wc prof profrun
