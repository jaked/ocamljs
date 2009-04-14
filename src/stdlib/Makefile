-include ../../Makefile.conf

ML=buffer.ml printf.ml camlinternalMod.ml pervasives.ml
MLI=$(addsuffix .mli,$(basename $(ML)))

all: myocamlbuild.ml ocaml $(ML) $(MLI) random.mli stdlib.mllib
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild stdlib.cmjsa std_exit.cmjs

%.ml : %.ml.patch
	cp ocaml/stdlib/$@ .
	patch -p0 < $<

%.mli :
	ln -s ocaml/stdlib/$@ .

patches:
	for ml in $(ML); do diff -Naur ocaml/stdlib/$$ml $$ml > $$ml.patch; done || exit 0

doc:

install:
	cp _build/stdlib.cmjsa $(LIBDIR)
	cp _build/ocaml/stdlib/std_exit.cmjs $(LIBDIR)
	cp support.js primitives.js $(LIBDIR)

uninstall:
	rm -f $(LIBDIR)/stdlib.cmjsa
	rm -f $(LIBDIR)/std_exit.cmjs
	rm -f $(LIBDIR)/support.js $(LIBDIR)/primitives.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml
	rm -f ocaml
	rm -f stdlib.mllib $(ML) $(MLI) random.mli

myocamlbuild.ml: ../../tools/myocamlbuild.ml myocamlbuild.mlp
	cat ../../tools/myocamlbuild.ml myocamlbuild.mlp > myocamlbuild.ml

ocaml:
	ln -s ../../ocaml .

stdlib.mllib:
	ln -s ocaml/stdlib/stdlib.mllib .
