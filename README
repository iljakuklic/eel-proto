
EEL - an experimental programming language
==========================================

EEL was tested on a modern 64-bit Linux distribution.
It might require some effort to port it to Windows
(has not been attempted).

Runtime requirements
--------------------

* libgc
* LLVM command line compiler: llc
* GCC's compiler driver: gcc

Build instructions
------------------

Standard Haskell Platform distribution is required.
For the full list of dependencies see the eel.cabal file.

Building EEL should be as easy as typing:
$ make

If dependencies fail, following is worth the try:
$ cabal-dev install-deps

Launching the interactive interpreter
-------------------------------------

Requires the 'rlwrap' utility
$ make repl

Or without readline support
$ ./eel -i

Typing ':?' (without the quotes) prints the interpreter help.

Demos build instructions
------------------------

$ make demos

Demo executables will be created in the demo/ subdirectory.

Launching demos
---------------

some examples are:

$ make run-rule110
$ make run-factorial

Getting help
------------

Try:
$ ./eel --help

For source documentation, see the doc subdirectory,
or generate it yourself:
$ make docs
