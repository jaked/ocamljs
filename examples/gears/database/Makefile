all: myocamlbuild.ml sample.ml
	ocamlbuild database.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml sample.ml

myocamlbuild.ml:
	ln -s ../../../tools/myocamlbuild.ml

sample.ml:
	ln -s ../sample.ml
