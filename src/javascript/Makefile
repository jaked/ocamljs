all: myocamlbuild.ml
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild javascript.cmjs

install:
	ocamlfind install javascript META _build/*.cmi _build/*.cmjs

uninstall:
	ocamlfind remove javascript

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
