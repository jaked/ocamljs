include ../../ocaml/config/Makefile

all: myocamlbuild.ml ocaml stdlib.mllib
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	ocamlbuild stdlib.cmjsa std_exit.cmjs

install:
	cp _build/stdlib.cmjsa $(LIBDIR)
	cp _build/ocaml/stdlib/std_exit.cmjs $(LIBDIR)

uninstall:
	rm -f $(LIBDIR)/stdlib.cmjsa
	rm -f $(LIBDIR)/std_exit.cmjs

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml
	rm -f ocaml
	rm -f stdlib.mllib

myocamlbuild.ml: ../../tools/myocamlbuild.ml myocamlbuild.mlp
	cat ../../tools/myocamlbuild.ml myocamlbuild.mlp > myocamlbuild.ml

ocaml:
	ln -s ../../ocaml .

stdlib.mllib:
	cp ocaml/stdlib/stdlib.mllib .
