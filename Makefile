include ocaml/config/Makefile

PKGS=\
ocamljs.cmjs \
javascript.cmjs \
mozilla.cmjs \
lwt-js.cmjsa

OCAMLBUILD=ocamlbuild -no-links

all: jslib prereqs ocamljs stdlib.cmjsa std_exit.cmjs $(PKGS)

jslib:
	make -C src/jslib

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/src/jslib/_build \
	$(OCAMLBUILD) src/jscomp/jsmain.byte
	cp _build/src/jscomp/jsmain.byte bin/ocamljs

# not sure how to get the right link stuff for this
#ocamljs.opt:
#	ocamlbuild -no-links src/jsmain.native
#	cp _build/src/jsmain.native bin/ocamljs.opt

# XXX figure out how to include an external lib in myocamlbuild.ml
# so we don't have to put everything in the top level

stdlib.cmjsa:
	cp ocaml/stdlib/stdlib.mllib src/stdlib; \
	export OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR); \
	$(OCAMLBUILD) src/stdlib/stdlib.cmjsa

std_exit.cmjs:
	$(OCAMLBUILD) ocaml/stdlib/std_exit.cmjs

ocamljs.cmjs:
	$(OCAMLBUILD) src/ocamljs/ocamljs.cmjs

javascript.cmjs:
	$(OCAMLBUILD) src/javascript/javascript.cmjs

mozilla.cmjs:
	$(OCAMLBUILD) src/mozilla/mozilla.cmjs

lwt-js.cmjsa:
	$(OCAMLBUILD) src/lwt-js/lwt-js.cmjsa

install:
	make -C src/jslib install
	cp bin/ocamljs $(BINDIR)
	cp bin/ocamlfindjs $(BINDIR)
	cp _build/src/stdlib/stdlib.cmjsa $(LIBDIR)
	cp _build/ocaml/stdlib/std_exit.cmjs $(LIBDIR)
	cp src/ocamljs/support.js $(LIBDIR)
	cp src/ocamljs/primitives.js $(LIBDIR)
	ocamlfind install ocamljs src/ocamljs/META _build/src/ocamljs/*.cmi _build/src/ocamljs/*.cmjs
	ocamlfind install javascript src/javascript/META _build/src/javascript/*.cmi _build/src/javascript/*.cmjs
	ocamlfind install mozilla src/mozilla/META _build/src/mozilla/*.cmi _build/src/mozilla/*.cmjs
	ocamlfind install lwt-js src/lwt-js/META _build/src/lwt-js/*.cmi _build/src/lwt-js/*.cmjsa

uninstall:
	make -C src/jslib uninstall
	rm $(BINDIR)/ocamljs
	rm $(BINDIR)/ocamlfindjs
	rm $(LIBDIR)/stdlib.cmjsa
	rm $(LIBDIR)/std_exit.cmjs
	rm $(LIBDIR)/*.js
	ocamlfind remove ocamljs
	ocamlfind remove javascript
	ocamlfind remove mozilla
	ocamlfind remove lwt-js

clean:
	rm src/stdlib/stdlib.mllib
	make -C src/jslib clean
	$(OCAMLBUILD) -clean
	rm -f bin/ocamljs
	make -C test clean

test:
	make -C test

.PHONY: test
