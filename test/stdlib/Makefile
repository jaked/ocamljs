all: myocamlbuild.ml
	ocamlbuild main.js;
	js _build/main.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
