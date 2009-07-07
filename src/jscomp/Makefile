-include ../../Makefile.conf

ifeq ($(OCAML_3_10_2),1)
OCAMLBUILD_FLAGS=-tag OCAML_3_10_2
endif

ML=translobj.ml translclass.ml
MLI=$(addsuffix .mli,$(basename $(ML)))

all: myocamlbuild.ml ocaml $(ML) $(MLI) prereqs ocamljs # ocamljs.opt

%.ml : %.ml.patch
	cp ocaml/bytecomp/$@ .
	patch -p0 < $<

%.mli :
	ln -s ocaml/bytecomp/$@ .

patches:
	for ml in $(ML); do diff -Naur ocaml/bytecomp/$$ml $$ml > $$ml.patch; done || exit 0

doc:

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild $(OCAMLBUILD_FLAGS) jsmain.byte

# not sure how to get this to link
ocamljs.opt:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../jslib/_build \
	ocamlbuild $(OCAMLBUILD_FLAGS) jsmain.native

install:
	install _build/jsmain.byte $(BINDIR)/ocamljs
#	install _build/jsmain.native $(BINDIR)/ocamljs.opt

uninstall:
	rm -f $(BINDIR)/ocamljs
#	rm -f $(BINDIR)/ocamljs.opt

clean:
	ocamlbuild -clean
	rm -f ocaml
	rm -f myocamlbuild.ml

myocamlbuild.ml: ../../tools/myocamlbuild.ml myocamlbuild.mlp
	cat ../../tools/myocamlbuild.ml myocamlbuild.mlp > myocamlbuild.ml

ocaml:
	ln -s ../../ocaml .
