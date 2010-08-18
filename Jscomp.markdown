---
layout: page
title: How ocamljs compiles OCaml to Javascript
---
#How ocamljs compiles OCaml to Javascript

###How ocamljs compiles OCaml###

`Ocamljs` uses the `ocamlc` front-end up to the point where it
generates 'lambda' code--an untyped representation that looks a lot
like Lisp. (You can see the lambda representation of OCaml code with
the -dlambda option to `ocamlc` or `ocamljs`.)

The translation is very direct: OCaml functions turn into
Javascript functions, OCaml exception handling to Javascript exception
handling, etc. Javascript distinguishes between expressions and
statements, while OCaml has only expressions, so there are two
compilations for most OCaml constructs, depending on which context the
construct appears in.

One difference between OCaml and Javascript is that functions in
OCaml may be applied to a different number of arguments than they are
defined to take. So OCaml function application is not compiled to
Javascript function application, but rather to a call to an apply
method which checks the number of arguments and either returns a
closure (for under-application) or tail calls the result of the
application with the extra arguments (for over-application). See
Xavier Leroy's talk
[From Krivine's machine to the Caml implementations](http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf).

This apply method is also where tail-recursion is implemented,
using trampolines. See `src/stdlib/support.js` for details.

Much of standard OCaml is implemented by a library of C primitives;
Javascript implementations of these (insofar as they have been
implemented) are in `src/ocamljs/primitives.js`. Some OCaml
primitives are implemented directly by the code generator; see
`comp_ccall` in `src/jscomp/jsgen.ml`.

###How `ocamljs` represents OCaml values in Javascript###

OCaml `int`s, `nativeint`s, `int32`s, `float`s, and `char`s are
represented by Javascript numbers. There is no support for
`int64`s. OCaml booleans are represented by Javascript booleans.

OCaml heap blocks (used in OCaml to represent records, variants,
polymorphic variants, exceptions, and arrays) are represented by
Javascript arrays. Blocks may have a tag (used to distinguish branches
of variants), which is represented as an extra slot on the array
object. See `src/stdlib/support.js` for functions on blocks.

Javascript strings are immutable; while OCaml strings are mutable,
mutable strings are not used much except in low-level code. `Ocamljs`
has two string representations: Mutable strings are arrays of chars
(represented as numbers) with a `toString` method to convert to an
actual string. Immutable strings are Javascript strings. If you write
a literal string in OCaml, you get a literal (immutable) Javascript
string. `Pervasives.(^)` and `Buffer.contents` (and therefore
`Printf.sprintf`) yield immutable strings. But `String.create` returns
a mutable string. You can read the elements of either string as usual,
but writing to an immutable string fails (at present with an
inscrutable error). If you call native code with a mutable string it
may or may not do the right thing. (XXX maybe it would be better to
drop support for immutable strings entirely.)
