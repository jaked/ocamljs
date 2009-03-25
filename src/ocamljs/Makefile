-include ../../Makefile.conf

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild ocamljs.cmjs
	ocamlfind remove -destdir ../../stage ocamljs
	ocamlfind install -destdir ../../stage ocamljs META _build/*.cmi _build/*.cmjs

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild doc.docdir/index.html

install:
	ocamlfind install ocamljs META _build/*.cmi _build/*.cmjs

uninstall:
	ocamlfind remove ocamljs

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
