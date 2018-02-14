set -a # export all vars
#set -x # debug


# NOTE in toplevel you need to #require "bos.top" and bos.setup;;

libname="tjr_path_resolution"
required_packages="extunix"
description="Path resolution, symlinks, etc"


function clean() {
	rm -f *.cmi *.cmo *.cmx *.o *.x *.a *.cma *.cmxa a.out
	# find . -maxdepth 1 -type l -exec rm -f \{\} \;
}



# generic from here ----------------------------------------------------

# was in bash_env.common

PKGS="-package $required_packages"
SYNTAX=""

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
# WARN="-w @f@p@u@s@40-8-11-26"
WARN=""

  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   -g $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt -g $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"

mls=`ocamldep -sort -one-line *.ml`
cmos="${mls//.ml/.cmo}"
cmxs="${mls//.ml/.cmx}"

natives="
"

branch=`git symbolic-ref --short HEAD` 
v=`date +'%F'`
if [ "$branch" = "master" ]; then
    package_name="${libname}"
else 
    package_name="${libname}_${branch}"
fi


function mk_meta() {
cat >META <<EOF
name="$package_name"
description="$description"
version="$v"
requires="$required_packages"
archive(byte)="$libname.cma"
archive(native)="$libname.cmxa"
EOF
}
