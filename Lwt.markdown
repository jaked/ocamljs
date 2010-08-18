---
layout: page
title: Lwt library for cooperative threading
---
#Lwt library for cooperative threading

Because Javascript in a browser is not normally multi-threaded, and
`ocamljs` makes no attempt to implement threads under the hood, it is
very convenient to use the monadic cooperative threads of
[Lwt](http://ocsigen.org/lwt). Lwt under `ocamljs` is also useful with
[orpc](http://github.com/jaked/orpc).

The core of Lwt can be compiled under `ocamljs` (but of course the
parts that depend on the `Unix` library may not be used). To build it,
unpack the Lwt source (matching to the installed version) in a
directory adjacent to the `ocamljs` source, then configure, build, and
install as usual. You can then use the Findlib `lwt` package as usual
with `ocamljs`.

In addition there are a few functions in `Lwt_dom` for timers etc. See
the [Ocamldoc](doc/Lwt_dom.html).
