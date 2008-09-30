include ../../ocaml/config/Makefile

all: myocamlbuild.ml ocaml prereqs ocamljs # ocamljs.opt

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild jsmain.byte

# not sure how to get this to link
ocamljs.opt:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../jslib/_build \
	ocamlbuild jsmain.native

install:
	cp _build/jsmain.byte $(BINDIR)/ocamljs
#	cp _build/jsmain.native $(BINDIR)/ocamljs.opt

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
