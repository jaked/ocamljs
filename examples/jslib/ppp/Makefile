all: myocamlbuild.ml
	ocamlbuild ppp.byte

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../../tools/myocamlbuild.ml
