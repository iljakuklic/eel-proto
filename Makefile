
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

demodir:
	mkdir -p demo


# cellular autometa examples
demos-cell: demo/cell-rule110 demo/cell-rule30 demo/cell-rider
demo/cell-%: samples/%.ca
	./eel -o $@ lib/boot.eel samples/cellular.eel $<

# brainfuck examples
demos-bf: demo/bf-hello demo/bf-factorial demo/bf-cat
demo/bf-%: samples/%.bf
	./eel -o $@ lib/boot.eel samples/brainfuck.eel $<

# bootstrapped syntax example
demo/eeel-demo: samples/test.eeel
	./eel -o $@ lib/boot.eel $<

# structured syntax example
demo/seel-demo: samples/test.seel samples/seel.eel
	./eel -o $@ lib/boot.eel samples/seel.eel samples/brainfuck.eel samples/test.seel

# build all demos
demos: eel demodir demo/eeel-demo demo/seel-demo demos-bf demos-cell

run-rule110: demo/cell-rule110
	$< 15 "|__________________@___@"

run-factorial: demo/bf-factorial
	$<

run-hello: demo/bf-hello
	$<

.PHONY: prof clean top doc docs readdoc repl wc prof profrun demodir
