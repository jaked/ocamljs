FILES=\
froc.cma froc.cmxa froc.a froc.cmjsa \
froc.mli froc.cmi \
froc_behavior.mli froc_behavior.cmi \
froc_event.mli froc_event.cmi

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
