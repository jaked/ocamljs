patches:
	for ml in $(wildcard *.ml); do diff -Naur $$ml.orig $$ml > $$ml.patch; done || exit 0
