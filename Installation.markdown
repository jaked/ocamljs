---
layout: page
title: Installing and running ocamljs
---
#Installing and running ocamljs#

###Installing ocamljs###

 1. Prerequisites: OCaml 3.11.x or 3.12.0, [Findlib](http://projects.camlcity.org/projects/findlib.html), [ulex](http://www.cduce.org/download.html#side)
 2. Unpack the OCaml source distribution (matching your OCaml installation) into a directory adjacent to the `ocamljs` source (you can also use the `-srcdir` argument to `configure` to point to a different directory).
 3. If you want Lwt and/or Ounit support, unpack their sources (matching the installed versions) into adjacent directories (or use the `-srcdir-lwt` and `-srcdir-ounit` arguments). NB: `ounit-1.0.3` has `1.0.2` in its `META` file, so you need to either fix the `META` file, rename the source directory, or use `-srcdir-ounit`.
 4. In the `ocamljs` directory, run `./configure`
 5. `make` builds `ocamljs` and the associated libraries
 6. `make install` installs `ocamljs` and the associated libraries
 7. `make test` runs the test suite; see [Ounit](Ounit.html) for prerequisites
 8. `make examples` builds all the examples

###Using ocamljs###

The command `ocamljs` may be used just like `ocamlc` or `ocamlopt`. It
generates `.cmjs`/`.cmjsa` files instead of `.cmo`/`.cma`. A linked
file (`.js`) consists of Javascript source, which may be run through a
standalone Javascript interpreter, or embedded in a web page or
Firefox extension. See the examples for details.

See `tools/myocambuild.ml` for some useful ocamlbuild rules.

There is also [findlib support](Findlib.html) which is recommended.
