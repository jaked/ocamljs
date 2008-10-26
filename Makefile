include ocaml/config/Makefile

PKGLIST=jslib jscomp ocamljs stdlib javascript mozilla lwt-js afp

all:
	for pkg in $(PKGLIST); do \
		$(MAKE) -C src/$$pkg all || exit; \
	done

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
	rm -rf stage/*

test:
	make -C test

.PHONY: test
