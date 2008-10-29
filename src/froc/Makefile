FILES=\
froc.cma froc.cmxa froc.a froc.cmjsa \
froc.mli froc.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild froc.cma froc.cmxa froc.cmjsa

install:
	ocamlfind install froc META $(BFILES)

uninstall:
	ocamlfind remove froc

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
