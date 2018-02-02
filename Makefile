SHELL:=/bin/bash
BASH_ENV:=bash_env.sh
export BASH_ENV


all: FORCE
	$$ocamlc -c tjr_path_resolution.mli
	$$ocamlc -c $$mls
	$$ocamlopt -c $$mls
	@echo "NOTE cma contains: $$cmos" # simple check
	$$mk_cma -g -a -o $$libname.cma $$cmos
	$$mk_cmxa -g -a -o $$libname.cmxa $$cmxs
	$(MAKE) install


install:
	-ocamlfind remove $$package_name
	mk_meta
	ocamlfind install $$package_name META *.cmi *.o *.a *.cma *.cmxa *.cmo *.cmx 


uninstall:
	ocamlfind remove $$package_name


clean: 
	clean


FORCE:
