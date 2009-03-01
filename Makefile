include ocaml/config/Makefile

PKGLIST=jslib jscomp ocamljs stdlib javascript mozilla lwt-js dom froc froc-dom gears

all:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg all || exit; \
	done

doc:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg doc || exit; \
	done
	find . -name '*.odoc' | awk '{print "-load"; print $$1}' | xargs ocamldoc -html -sort -d doc

install:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg install || exit; \
	done
	cp bin/ocamlfindjs $(BINDIR)

uninstall:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg uninstall || exit; \
	done
	rm -f $(BINDIR)/ocamlfindjs

clean:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg clean || exit; \
	done
	make -C test clean
	make -C examples clean
	rm -rf stage/*

test:
	make -C test

examples:
	make -C examples

.PHONY: test examples doc

gcode:
	rsync -a --delete --exclude '.svn/' doc/ ../doc/
	rsync -a --delete --exclude '.svn/' --include '*/' --include '*.html' --include '*.js' --include '*.css' --exclude '*' examples/ ../examples/
