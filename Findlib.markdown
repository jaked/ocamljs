---
layout: page
title: Findlib support
---
#Findlib support#

There is some basic support for Findlib and `ocamlfind`. When you
install `ocamljs` you'll get a script `ocamlfindjs` which replaces
`ocamlfind` (it's just a wrapper). In a `META` file you can use the
`js` tag for archives etc. However, `ocamlfind` doesn't know anything
about `ocamljs`; we just override the regular `ocamlc`, and both the
`byte` and `js` tags are set, so in a `META` file the `js` entries
must go before the `byte` entries if both are supported by the
library.

I'd like to do something better here, but this works pretty well and
requires no changes to Findlib.
