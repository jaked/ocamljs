all: myocamlbuild.ml
	ocamlbuild main.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../../tools/myocamlbuild.ml
