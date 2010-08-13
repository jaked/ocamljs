ML=$(wildcard *.ml)
PATCHES=$(addsuffix .patch,$(ML))

%.ml.patch: %.ml
	-diff -u $<.orig $< > $@

patches: $(PATCHES)
