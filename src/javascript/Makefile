all: myocamlbuild.ml
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild javascript.cmjs

doc:
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild doc.docdir/index.html

install:
	ocamlfind install javascript META _build/*.cmi _build/*.cmjs

uninstall:
	ocamlfind remove javascript

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
