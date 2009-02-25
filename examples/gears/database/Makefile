all: myocamlbuild.ml
	ocamlbuild database.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml
