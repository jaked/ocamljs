---
layout: page
title: Introduction to ocamljs
---
#Introduction to ocamljs#

`Ocamljs` is a system for compiling OCaml to Javascript. It includes a
Javascript back-end for the OCaml compiler, as well as several support
libraries, such as bindings to the browser DOM. `Ocamljs` also works
with [orpc](http://jaked.github.com/orpc) for RPC over HTTP, and
[froc](http://jaked.github.com/froc) for functional reactive browser
programming. The full OCaml language and much of the OCaml standard
library is supported. There is special support for the object system
so Javascript objects may be called from OCaml and vice-versa.

 * [how to install and use `ocamljs`](Installation.html)
 * [the Javascript back-end](Jscomp.html)
 * [interfacing with native code](Interfacing.html)
 * [findlib support](Findlib.html)

Libraries:

 * [Jslib](Jslib.html) library for working with Javascript using Camlp4
 * [Ocamljs](Ocamljs.html) ocamljs support library
 * [Stdlib](Stdlib.html) support for the OCaml standard library
 * [Javascript](Javascript.html) binding to built-in Javascript libraries
 * [Dom](Dom.html) binding to browser DOM
 * [Mozilla](Mozilla.html) binding to Mozilla API
 * [Jquery](Jquery.html) binding to jQuery

Third-party libraries

 * [Lwt](Lwt.html) library for cooperative threading
 * [OUnit](Ounit.html) unit test library

See the [Ocamldoc](doc/index.html) and some [examples](examples/index.html).

###Contributing###

If you find a bug, it would be very helpful to get a test case that
produces different behavior from regular OCaml (see `test/jscomp` for
examples). Other contributions are very welcome as well. The best way
to submit a bug is to add a
[Github issue](http://github.com/jaked/ocamljs/issues). The best way
to submit a patch is to send a Github pull request.

###See also###

 * [O'Browser](http://ocsigen.org/obrowser/)
 * [Js\_of\_ocaml](http://ocsigen.org/js_of_ocaml/)
 * [Smltojs](http://www.itu.dk/people/mael/smltojs/)
 * [Ycr2js](http://www.haskell.org/haskellwiki/Yhc/Javascript)
