TMP_DOC_DIR:=/tmp/tjr_path_resolution
#scratch:=/tmp/l/github/scratch

default: all
-include Makefile.ocaml

all::
	dune build test/test.exe

# FIXME how to get dune to provide this path?
test:=$(shell realpath _build/default/test/test.exe)

run:
	cd test && $(test)

# for auto-completion of Makefile target
clean::
