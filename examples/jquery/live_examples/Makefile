all: myocamlbuild.ml
	ocamlbuild live_examples.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../../tools/myocamlbuild.ml
