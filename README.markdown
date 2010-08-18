#Ocamljs#

`Ocamljs` is a system for compiling OCaml to Javascript. It includes a
Javascript back-end for the OCaml compiler, as well as several support
libraries, such as bindings to the browser DOM. `Ocamljs` also works
with [orpc](http://jaked.github.com/orpc) for RPC over HTTP, and
[froc](http://jaked.github.com/froc) for functional reactive browser
programming. The full OCaml language and much of the OCaml standard
library is supported. There is special support for the object system
so Javascript objects may be called from OCaml and vice-versa.

You can download `ocamljs` at [http://github.com/jaked/ocamljs/downloads](http://github.com/jaked/ocamljs/downloads).

See [Introduction](Introduction.html) and [Installation](Installation.html).

For a quick start:

 0. Unpack OCaml source adjacent to the `ocamljs` tree
 1. `./configure`
 2. `make`
 3. `make install`
 4. `make examples`

`Ocamljs` is written by Jake Donham, with contributions from

 * Dave Benjamin (jQuery binding and examples)
 * Haoyang Wang
 * Mike Wells
 * Stepan Zastupov
