---
layout: page
title: Ounit library for unit testing
---
#Ounit library for unit testing

To build Ounit under `ocamljs`, unpack the source (matching to the
installed version) in a directory adjacent to the `ocamljs` source,
then configure, build, and install as usual. You can then use the
Findlib `oUnit` package as usual with `ocamljs`. Note that the shipped
Ounit 1.0.3 has version 1.0.2 in the `META` file, so you either need
to fix the `META` file, rename the source directory to match, or use
the `-srcdir-ounit` argument to `configure`.

You can use Ounit in the usual way (see e.g. `test/ocamljs` for some
examples), then run the result through SpiderMonkey (or other
Javascript interpreter). However, the Javascript interpreter must be
able to print a string without a trailing newline. In SpiderMonkey the
`print` builtin function adds a newline; `Ocamljs` expects there to be
a function `print_verbatim` which doesn't add the newline. You can
build SpiderMonkey with the patch in `src/ounit/js.patch`, or
implement this function in some other Javascript interpreter.
