-include Makefile.conf

all:
	mkdir -p stage
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg all || exit; \
	done

doc:
	mkdir -p doc
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg doc || exit; \
	done
	find . -name '*.odoc' | awk '{print "-load"; print $$1}' | xargs ocamldoc -html -sort -d doc

install:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg install || exit; \
	done
	install bin/ocamlfindjs $(BINDIR)

uninstall:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg uninstall || exit; \
	done
	rm -f $(BINDIR)/ocamlfindjs

clean:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg clean || exit; \
	done
	$(MAKE) -C test clean
	$(MAKE) -C examples clean
	rm -rf doc
	rm -rf stage

distclean: clean
	rm -rf Makefile.conf
	rm -f ocaml

test:
	$(MAKE) -C test

examples:
	$(MAKE) -C examples

.PHONY: test examples doc

github:
	rsync -a --delete --exclude './' doc/ ../ocamljs.gh-pages/doc/
	rsync -a -L --delete --delete-excluded --exclude _build/ --include '*/' --exclude myocamlbuild.ml --include '*.ml' --include '*.js' --include '*.html' --include '*.css' --include '*.png' --include '*.jpg' --exclude '*' examples/ ../ocamljs.gh-pages/examples/

dist:
	$(MAKE) distclean
	cd ..; \
        mv ocamljs ocamljs-0.3; \
        tar cvfz ocamljs-0.3.tar.gz --exclude-from ocamljs-0.3/.exclude ocamljs-0.3; \
	mv ocamljs-0.3 ocamljs

