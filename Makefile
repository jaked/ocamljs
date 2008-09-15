include ocaml/config/Makefile

LIBS=\
stdlib.cmjsa \
std_exit.cmjs

PKGS=\
ocamljs.cmjs \
javascript.cmjs \
mozilla.cmjs \
lwt-js.cmjsa

OCAMLBUILD=ocamlbuild -no-links

all: jslib prereqs ocamljs $(LIBS) $(PKGS)

jslib:
	make -C src/jslib

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/src/jslib/_build \
	$(OCAMLBUILD) src/ocamljs/jsmain.byte
	cp _build/src/ocamljs/jsmain.byte bin/ocamljs

# not sure how to get the right link stuff for this
#ocamljs.opt:
#	ocamlbuild -no-links src/jsmain.native
#	cp _build/src/jsmain.native bin/ocamljs.opt

# XXX figure out how to include an external lib in myocamlbuild.ml
# so we don't have to put everything in the top level

stdlib.cmjsa:
	export OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR); \
	$(OCAMLBUILD) ocaml/stdlib/stdlib.cmjsa

std_exit.cmjs:
	$(OCAMLBUILD) ocaml/stdlib/std_exit.cmjs

ocamljs.cmjs:
	$(OCAMLBUILD) src/libs/ocamljs/ocamljs.cmjs

javascript.cmjs:
	$(OCAMLBUILD) src/libs/javascript/javascript.cmjs

mozilla.cmjs:
	$(OCAMLBUILD) src/libs/mozilla/mozilla.cmjs

lwt-js.cmjsa:
	$(OCAMLBUILD) src/libs/lwt-js/lwt-js.cmjsa

install:
	make -C src/jslib install
	cp bin/ocamljs $(BINDIR)
	cp bin/ocamlfindjs $(BINDIR)
	cp $(addprefix _build/ocaml/stdlib/,$(LIBS)) $(LIBDIR)
	cp src/libs/ocamljs/support.js $(LIBDIR)
	cp src/libs/ocamljs/primitives.js $(LIBDIR)
	ocamlfind install ocamljs src/libs/ocamljs/META _build/src/libs/ocamljs/*.cmi _build/src/libs/ocamljs/*.cmjs
	ocamlfind install javascript src/libs/javascript/META _build/src/libs/javascript/*.cmi _build/src/libs/javascript/*.cmjs
	ocamlfind install mozilla src/libs/mozilla/META _build/src/libs/mozilla/*.cmi _build/src/libs/mozilla/*.cmjs
	ocamlfind install lwt-js src/libs/lwt-js/META _build/src/libs/lwt-js/*.cmi _build/src/libs/lwt-js/*.cmjsa

uninstall:
	make -C src/jslib uninstall
	rm $(BINDIR)/ocamljs
	rm $(addprefix $(LIBDIR)/,$(LIBS))
	rm $(LIBDIR)/*.js
	ocamlfind remove ocamljs
	ocamlfind remove javascript
	ocamlfind remove mozilla
	ocamlfind remove lwt-js

clean:
	make -C src/jslib clean
	$(OCAMLBUILD) -clean
	rm -rf bin/ocamljs
	make -C test clean

test:
	make -C test

.PHONY: test
