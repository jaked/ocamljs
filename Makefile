include ocaml/config/Makefile

OCAMLBUILD=ocamlbuild -no-links

all: prereqs ocamljs stdlib.cmjsa std_exit.cmjs

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

stdlib.cmjsa:
	$(OCAMLBUILD) ocaml/stdlib/stdlib.cmjsa
	cp _build/ocaml/stdlib/stdlib.cmjsa lib

std_exit.cmjs:
	$(OCAMLBUILD) ocaml/stdlib/std_exit.cmjs
	cp _build/ocaml/stdlib/std_exit.cmjs lib

install:
	cp bin/ocamljs $(BINDIR)
	cp lib/support.js lib/primitives.js lib/stdlib.cmjsa lib/std_exit.cmjs $(LIBDIR)

clean:
	$(OCAMLBUILD) -clean
	rm -rf bin/ocamljs lib/stdlib.cmjsa lib/std_exit.cmjs
	make -C test clean

test:
	make -C test

.PHONY: test
