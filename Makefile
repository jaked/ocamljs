include ocaml/config/Makefile

OCAMLBUILD=ocamlbuild -no-links

all: prereqs ocamljs stdlib.cmja std_exit.cmj

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	$(OCAMLBUILD) src/jsmain.byte
	cp _build/src/jsmain.byte bin/ocamljs

# not sure how to get the right link stuff for this
#ocamljs.opt:
#	ocamlbuild -no-links src/jsmain.native
#	cp _build/src/jsmain.native bin/ocamljs.opt

stdlib.cmja:
	$(OCAMLBUILD) ocaml/stdlib/stdlib.cmja
	cp _build/ocaml/stdlib/stdlib.cmja lib

std_exit.cmj:
	$(OCAMLBUILD) ocaml/stdlib/std_exit.cmj
	cp _build/ocaml/stdlib/std_exit.cmj lib

install:
	cp bin/ocamljs $(BINDIR)
	cp lib/support.js lib/primitives.js lib/stdlib.cmja lib/std_exit.cmj $(LIBDIR)

clean:
	$(OCAMLBUILD) -clean
	rm -rf bin/ocamljs lib/stdlib.cmja lib/std_exit.cmj
	make -C test clean

test:
	make -C test

.PHONY: test
