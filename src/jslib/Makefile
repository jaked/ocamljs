-include ../../Makefile.conf

FILES=\
jslib.cma jslib.cmxa jslib.a \
syntax_quotations.cmo \
syntax_inline.cmo \
syntax_lambda.cmo \
jslib_ast.mli jslib_ast.cmi \
jslib_parse.mli jslib_parse.cmi \
jslib_pp.mli jslib_pp.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -verbose 10 jslib.cma jslib.cmxa syntax_quotations.cmo syntax_inline.cmo syntax_lambda.cmo
	ocamlfind remove -destdir ../../stage jslib
	ocamlfind install -destdir ../../stage jslib META $(BFILES)

doc:
	OCAMLFIND_IGNORE_DUPS_IN=$(LIBDIR)/site-lib \
	OCAMLPATH=`pwd`/../../stage \
	ocamlbuild -no-links doc.docdir/index.html

install:
	ocamlfind install jslib META $(BFILES)

uninstall:
	ocamlfind remove jslib

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
