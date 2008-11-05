all: myocamlbuild.ml
	ocamlbuild where.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml
