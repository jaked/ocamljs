all: myocamlbuild.ml
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild mozilla.cmjs

install:
	ocamlfind install mozilla META _build/*.cmi _build/*.cmjs

uninstall:
	ocamlfind remove mozilla

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
