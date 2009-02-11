FILES=\
gears.cmjsa \
gears.mli gears.cmi

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR) \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild gears.cmjsa
	ocamlfind remove -destdir ../../stage gears
	ocamlfind install -destdir ../../stage gears META $(BFILES)

doc:
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild doc.docdir/index.html

install:
	ocamlfind install gears META $(BFILES)

uninstall:
	ocamlfind remove gears

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
