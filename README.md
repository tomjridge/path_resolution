Path resolution
===============

This is a simple specification/implementation of core path resolution,
taking account of real-world behaviour uncovered by SibylFS.

Build and install
=================

To build, type `make`

NOTE this installs a package `tjr_path_resolution` into `ocamlfind/opam`

Documentation
=============

For documentation, please read the source code of
`./src/path_resolution.ml` or the commented
`./src/tjr_path_resolution.mli` for the interface.

The ocamldoc is here: <https://tomjridge.github.io/path_resolution/docs>

Dependencies
============

opam deps:

other deps:

  ---------------
  tjr~fsshared~
  tjr~monad~
  ---------------

old?:

  ---------
  extunix
  ---------
