all: myocamlbuild.ml sample.ml sample.css
	ocamlbuild geolocation.js

clean:
	ocamlbuild -clean
	rm -f myocamlbuild.ml sample.ml sample.css

myocamlbuild.ml:
	ln -s ../../myocamlbuild.ml

sample.ml:
	ln -s ../sample.ml

sample.css:
	ln -s ../sample.css
