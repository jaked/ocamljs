-include ../../Makefile.conf

all: myocamlbuild.ml
	cp $(SRCDIR_OUNIT)/oUnit.ml .
	cp $(SRCDIR_OUNIT)/oUnit.mli .
	cp $(SRCDIR_OUNIT)/META .
	patch oUnit.ml < oUnit.ml.patch
	patch -b META < META.patch
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild oUnit.cmjsa

doc:

install:
	dir=`ocamlfind -query -format "%d" oUnit`; \
	cp META _build/oUnit.cmjsa $$dir

uninstall:
	dir=`ocamlfind -query -format "%d" oUnit`; \
	rm -f $$dir/oUnit.cmjsa; \
	cp META.orig $$dir/META

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml *.ml *.mli META META.orig

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
