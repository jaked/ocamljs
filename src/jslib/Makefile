FILES=\
jslib.cma jslib.cmxa jslib.a \
pa_js.cma \
jslib_ast.mli jslib_ast.cmi \
jslib_parse.mli jslib_parse.cmi \
jslib_pp.mli jslib_pp.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild jslib.cma jslib.cmxa pa_js.cma
	ocamlfind remove -destdir ../../stage jslib
	ocamlfind install -destdir ../../stage jslib META $(BFILES)

install:
	ocamlfind install jslib META $(BFILES)

uninstall:
	ocamlfind remove jslib

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
