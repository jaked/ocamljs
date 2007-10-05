include ocaml/config/Makefile

LIBS=support.js primitives.js stdlib.cmjsa std_exit.cmjs ocamljs.cmjs javascript.cmjs mozilla.cmjs

OCAMLBUILD=ocamlbuild -no-links

all: prereqs ocamljs $(LIBS)

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	$(OCAMLBUILD) src/ocamljs/jsmain.byte
	cp _build/src/ocamljs/jsmain.byte bin/ocamljs

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

ocamljs.cmjs:
	$(OCAMLBUILD) src/libs/ocamljs/ocamljs.cmjs
	cp _build/src/libs/ocamljs/ocamljs.cm* lib

javascript.cmjs:
	$(OCAMLBUILD) src/libs/javascript/javascript.cmjs
	cp _build/src/libs/javascript/javascript.cm* lib

mozilla.cmjs:
	$(OCAMLBUILD) src/libs/mozilla/mozilla.cmjs
	cp _build/src/libs/mozilla/mozilla.cm* lib

support.js:
	cp src/libs/ocamljs/support.js lib

primitives.js:
	cp src/libs/ocamljs/primitives.js lib

install:
	cp bin/ocamljs $(BINDIR)
	cd lib; cp $(LIBS) $(LIBDIR)

clean:
	$(OCAMLBUILD) -clean
	rm -rf bin/ocamljs
	cd lib; rm -rf $(LIBS)
	make -C test clean

test:
	make -C test

.PHONY: test
