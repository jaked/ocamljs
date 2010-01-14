-include ../../Makefile.conf

ML=buffer.ml printf.ml camlinternalMod.ml pervasives.ml camlinternalOO.ml
MLI=$(addsuffix .mli,$(basename $(ML)))

all: myocamlbuild.ml ocaml $(ML) $(MLI) random.mli stdlib.mllib
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild stdlib.cmjsa std_exit.cmjs

%.ml : patches/$(OCAML_VERSION)/%.ml.patch
	cp ocaml/stdlib/$@ .
	patch $@ $<

%.mli :
	ln -s ocaml/stdlib/$@ .

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
