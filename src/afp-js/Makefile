FILES=\
afp.cma afp.cmxa afp.a afp.cmjsa \
afp.mli afp.cmi \
afp_list.mli afp_list.cmi \

BFILES=$(addprefix _build/,$(FILES))

all: myocamlbuild.ml
	ocamlbuild afp.cma afp.cmxa afp.cmjsa

install:
	ocamlfind install afp META $(BFILES)

uninstall:
	ocamlfind remove afp

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml .
