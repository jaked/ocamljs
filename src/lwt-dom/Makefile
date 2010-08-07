-include ../../Makefile.conf

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild lwt_dom.cmjs
	ocamlfind remove -destdir ../../stage lwt-dom
	ocamlfind install -destdir ../../stage lwt-dom META _build/*.cmi _build/*.cmjs

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install lwt-dom META _build/*.cmi _build/*.cmjs

uninstall:
	ocamlfind remove lwt-dom

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
