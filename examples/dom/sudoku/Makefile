all: myocamlbuild.ml
	ocamlbuild sudoku.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml
