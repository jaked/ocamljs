# e.g. SpiderMonkey at http://www.mozilla.org/js/spidermonkey/
JS=js

COMPFLAGS=-warn-error A

TESTS=$(basename $(wildcard *.ml))

all: $(addsuffix .test,$(TESTS))

%.js : %.ml
	../bin/ocamljs $(COMPFLAGS) -I ../lib -o $@ $<

%.exp : %.ml
	ocamlc $(COMPFLAGS) -o $(basename $@) $<; \
	./$(basename $@) > $@

%.test: %.exp %.js
	@t=`basename $< .exp`; \
	echo -n $$t ": "; \
	$(JS) $$t.js > $$t.act; \
	if cmp -s $$t.exp $$t.act; then \
	  echo "OK"; \
	else \
	  echo "Failed"; diff $$t.exp $$t.act; \
	fi

clean:
	@for t in $(TESTS); do \
	  rm -f $$t $$t.js $$t.exp $$t.act $$t.cmi $$t.cmj; \
	done

.PRECIOUS: %.js %.exp
