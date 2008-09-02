FILES=\
jslib.cma jslib.cmxa jslib.a \
pa_js.cma \
jslib_ast.mli jslib_ast.cmi \
jslib_parse.mli jslib_parse.cmi \
jslib_pp.mli jslib_pp.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: build tmp-install

build:
	ocamlbuild jslib.cma jslib.cmxa pa_js.cma

tmp-install: $(BFILES)
	ocamlfind remove -destdir _build jslib
	ocamlfind install -destdir _build jslib META $(BFILES)

install:
	ocamlfind install jslib META $(BFILES)

uninstall:
	ocamlfind remove jslib

clean:
	ocamlbuild -clean
