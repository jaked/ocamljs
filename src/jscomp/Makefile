-include ../../Makefile.conf

ifeq ($(OCAML_3_10_2),1)
OCAMLBUILD_FLAGS=-tag OCAML_3_10_2
endif

ML=translcore.ml translobj.ml translclass.ml typecore.ml
MLI=$(addsuffix .mli,$(basename $(ML)))

all: myocamlbuild.ml ocaml $(ML) $(MLI) prereqs ocamljs # ocamljs.opt

%.ml : %.ml.patch
	if [ -f ocaml/bytecomp/$@ ]; then cp ocaml/bytecomp/$@ .; \
	elif [ -f ocaml/typing/$@ ]; then cp ocaml/typing/$@ .; \
	fi
	patch -p0 < $<

%.mli :
	if [ -f ocaml/bytecomp/$@ ]; then ln -s ocaml/bytecomp/$@ .; \
	elif [ -f ocaml/typing/$@ ]; then ln -s ocaml/typing/$@ .; \
	fi

patches:
	for ml in $(ML); do \
	  if [ -f ocaml/bytecomp/$$ml ]; then diff -Naur ocaml/bytecomp/$$ml $$ml > $$ml.patch; \
	  elif [ -f ocaml/typing/$$ml ]; then diff -Naur ocaml/typing/$$ml $$ml > $$ml.patch; \
	  fi; \
	done || exit 0

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
	rm -f $(ML) $(MLI)

myocamlbuild.ml: ../../tools/myocamlbuild.ml myocamlbuild.mlp
	cat ../../tools/myocamlbuild.ml myocamlbuild.mlp > myocamlbuild.ml

ocaml:
	ln -s ../../ocaml .
