all: myocamlbuild.ml
	ocamlbuild follow.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml
