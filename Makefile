include ocaml/config/Makefile

LIBS=\
stdlib.cmjsa \
std_exit.cmjs \
ocamljs.cmjs \
javascript.cmjs \
mozilla.cmjs \
lwt-js.cmjsa

LIB_CMIS=\
ocamljs.cmi \
javascript.cmi \
mozilla.cmi \
lwt.cmi lwt_mutex.cmi \
lwt_pool.cmi lwt_util.cmi

LIB_FILES=$(LIBS) $(LIB_CMIS)

OCAMLBUILD=ocamlbuild -no-links

all: jslib prereqs ocamljs $(LIBS)

jslib:
	make -C src/jslib

prereqs:
	./ocaml/build/mkmyocamlbuild_config.sh
	make -C ocaml/byterun primitives # how does this get built for regular OCaml?

ocamljs:
	export OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR); \
	export OCAMLPATH=`pwd`/src/jslib/_build; \
	$(OCAMLBUILD) src/ocamljs/jsmain.byte
	cp _build/src/ocamljs/jsmain.byte bin/ocamljs

# not sure how to get the right link stuff for this
#ocamljs.opt:
#	ocamlbuild -no-links src/jsmain.native
#	cp _build/src/jsmain.native bin/ocamljs.opt

stdlib.cmjsa:
	export OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR); \
	$(OCAMLBUILD) ocaml/stdlib/stdlib.cmjsa
	cp _build/ocaml/stdlib/stdlib.cmjsa lib

std_exit.cmjs:
	$(OCAMLBUILD) ocaml/stdlib/std_exit.cmjs
	cp _build/ocaml/stdlib/std_exit.cmjs lib

ocamljs.cmjs:
	$(OCAMLBUILD) src/libs/ocamljs/ocamljs.cmjs
	cp _build/src/libs/ocamljs/*.cm* lib

javascript.cmjs:
	$(OCAMLBUILD) src/libs/javascript/javascript.cmjs
	cp _build/src/libs/javascript/*.cm* lib

mozilla.cmjs:
	$(OCAMLBUILD) src/libs/mozilla/mozilla.cmjs
	cp _build/src/libs/mozilla/mozilla.cm* lib

lwt-js.cmjsa:
	$(OCAMLBUILD) src/libs/lwt-js/lwt-js.cmjsa
	cp _build/src/libs/lwt-js/*.cmi lib
	cp _build/src/libs/lwt-js/*.cmjsa lib

install:
	make -C src/jslib install
	cp bin/ocamljs $(BINDIR)
	cd lib; cp $(LIB_FILES) $(LIBDIR)
	cp src/libs/ocamljs/support.js $(LIBDIR)
	cp src/libs/ocamljs/primitives.js $(LIBDIR)

uninstall:
	make -C src/jslib uninstall
	rm $(BINDIR)/ocamljs
	rm $(addprefix $(LIBDIR)/,$(LIB_FILES))
	rm $(LIBDIR)/*.js

clean:
	make -C src/jslib clean
	$(OCAMLBUILD) -clean
	rm -rf bin/ocamljs
	cd lib; rm -rf $(LIB_FILES)
	make -C test clean

test:
	make -C test

.PHONY: test
