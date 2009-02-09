all: myocamlbuild.ml
	ocamlbuild orbit.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml
