-include ../../Makefile.conf

FILES=\
dom.cmjsa \
dom.mli dom.cmi

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild dom.cmjsa
	ocamlfind remove -destdir ../../stage dom
	ocamlfind install -destdir ../../stage dom META $(BFILES)

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild doc.docdir/index.html

install:
	ocamlfind install dom META $(BFILES)

uninstall:
	ocamlfind remove dom

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
