-include ../../Makefile.conf

all: myocamlbuild.ml
	cp $(SRCDIR_LWT)/src/core/* .
	cp $(SRCDIR_LWT)/_build/META .
	patch -b META < META.patch
	ocamlbuild lwt.cmjsa

doc:

install:
	dir=`ocamlfind -query -format "%d" lwt`; \
	cp META _build/lwt.cmjsa $$dir

uninstall:
	dir=`ocamlfind -query -format "%d" lwt`; \
	rm -f $$dir/lwt.cmjsa; \
	cp META.orig $$dir/META

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml *.mllib *.ml *.mli META META.orig

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
