all: myocamlbuild.ml
	ocamlbuild lwt-js.cmjsa

install:
	ocamlfind install lwt-js META _build/*.cmi _build/*.cmjsa

uninstall:
	ocamlfind remove lwt-js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml

myocamlbuild.ml:
	ln -s ../../tools/myocamlbuild.ml
