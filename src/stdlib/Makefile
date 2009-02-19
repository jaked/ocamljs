include ../../ocaml/config/Makefile

all: myocamlbuild.ml ocaml stdlib.mllib
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild stdlib.cmjsa std_exit.cmjs

doc:

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
	rm -f stdlib.mllib *.mli

myocamlbuild.ml: ../../tools/myocamlbuild.ml myocamlbuild.mlp
	cat ../../tools/myocamlbuild.ml myocamlbuild.mlp > myocamlbuild.ml

ocaml:
	ln -s ../../ocaml .

stdlib.mllib:
	ln -s ocaml/stdlib/stdlib.mllib .
	ln -s ocaml/stdlib/random.mli .
	ln -s ocaml/stdlib/buffer.mli .
	ln -s ocaml/stdlib/printf.mli .
	ln -s ocaml/stdlib/camlinternalMod.mli .
