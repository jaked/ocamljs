include ../../ocaml/config/Makefile

all: myocamlbuild.ml
	ocamlbuild ocamljs.cmjs
	ocamlfind remove -destdir ../../stage ocamljs
	ocamlfind install -destdir ../../stage ocamljs META _build/*.cmi _build/*.cmjs

install:
	cp support.js $(LIBDIR)
	cp primitives.js $(LIBDIR)
	ocamlfind install ocamljs META _build/*.cmi _build/*.cmjs

uninstall:
	rm -f $(LIBDIR)/*.js
	ocamlfind remove ocamljs

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
