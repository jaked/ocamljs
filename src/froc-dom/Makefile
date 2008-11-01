FILES=\
froc-dom.cmjsa \
froc_dom.mli froc_dom.cmi

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild froc-dom.cmjsa
	ocamlfind remove -destdir ../../stage froc-dom
	ocamlfind install -destdir ../../stage froc-dom META $(BFILES)

install:
	ocamlfind install froc-dom META $(BFILES)

uninstall:
	ocamlfind remove froc-dom

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
