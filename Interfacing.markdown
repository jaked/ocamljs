---
layout: page
title: How to interface with Javascript
---
#How to interface with Javascript#

There are a number of ways to talk to Javascript code and objects from OCaml:

###External functions###

You can call Javascript functions in the same way you call C
primitives in ordinary OCaml, by declaring them `external`. Put the
external functions in a file `foo.js`, and add `foo.js` to the link
line. The names of the primitives are read out of the Javascript file;
any name after `var` or `function` at the beginning of the line is
included.

You can also use special `external` declarations to avoid having to
write trivial wrapper functions for Javascript field accessors and
method calls. The following special prefixes to the function name are
recognized:

| prefix | compiles to                                                        |
|--------|--------------------------------------------------------------------|
| `#`    | method call                                                        |
| `.`    | read property                                                      |
| `=`    | assign property                                                    |
| `@`    | function call for builtin function, not checked against primitives |

The object-related prefixes expect the object as the first
argument. For example, if you declare
`external foo : bar -> unit = "#foo"` and call `foo b`, that compiles
to `b.foo()`.

Finally, there are several special `external` symbols which produce
Javascript syntax when compiled. They can also help you to avoid
trivial wrappers in some cases. They expect particular OCaml syntax as
arguments (they are recognized at compile time). But see below for
inline Javascript, which is a better way to do it for larger fragments.

| symbol      | expects args          | compiles to           |
|-------------|-----------------------|-----------------------|
| `$assign`   | \[lvalue; expression] | `lvalue = expression` |
| `$call`     | function::args        | `function(args)`      |
| `$false`    | unused                | `false`               |
| `$fieldref` | \[expression; id]     | `expression.id`       |
| `$function` | \[function]           | Javascript function   |
| `$hashref`  | \[exp1; exp2]         | `exp1[exp2]`          |
| `$new`      | id::args              | `new id(args)`        |
| `$new id`   | args                  | `new id(args)`        |
| `$null`     | unused                | `null`                |
| `$obj`      | alist of ids and exps | object literal        |
| `$this`     | unused                | `this`                |
| `$throw`    | \[expression\]        | `throw expression`    |
| `$true`     | unused                | `true`                |
| `$var`      | id                    | `id`                  |

Identifiers are given as strings. The `$new id` symbol uses the native
name argument to external, e.g. `external "$new" "Foo" : unit ->
foo`. See `comp_ccall` in `src/jscomp/jsgen.ml` to see exactly what
these do.

Most of these are available in the [Ocamljs](Ocamljs.html) library, or
you can declare them yourself with `external` (sometimes that's more
convenient if you want more accurate types).

###Object wrappers###

It's possible to call methods on Javascript objects using the OCaml
method call syntax. For example, `document#getElementById "foo"`
becomes `document.getElementById("foo")`. You can also access the
properties of an object with specially-named methods,
e.g. `style#_get_background` and `style#_set_background "#ffffff"` become
`style.background` and `style.background = "#ffffff"`.

There are some other special cases: a leading underscore is dropped so
you can use capitalized method names (e.g. `obj#_QueryInterface`); a
trailing underscore or `_.*_` is dropped so you can give multiple
types to the same method when the Javascript interface is more
dynamically-typed than can be accomodated in OCaml
(e.g. `canvas#createPattern` and `canvas#createPattern_canvas_` both
become `canvas.createPattern`); you can use either of these to deal
with reserved words (e.g. `_open` or `end_`).

To get these uses to typecheck, of course, you need to operate on an
object of the appropriate type. It's easiest to write down an
interface as a `class type`, then magic your Javascript object to that
type, or get the object using a function or method with a polymorphic
return value (such as `Ocamljs.var` or `Dom.document#getElementById`)
and ascribe the type. See the [Dom](Dom.html) and
[Mozilla](Mozilla.html) libraries for examples.

Types given this way are only advisory in two senses: first, you have
to ascribe the correct types; second, if an object is given as input
to a Javascript method or function, it may not be the type that's
expected even if it has the right shape (e.g. a DOM object must be an
actual DOM object, not just an object with the same methods). In both
cases there can be a dynamic failure at the Javascript level that's
not caught at compile time.

A final caveat is that partial application does not work for native
methods; a partial application causes an immediate call to the method
with the arguments you provide. (Partial application does work for
functions called via `external`, because the OCaml compiler wraps an
OCaml function around them.)

###Using OCaml objects from Javascript###

Objects defined in OCaml can be used from Javascript; they are
compiled to Javascript objects with the same method names (along with
some other stuff to support OCaml's object system---see
[here](http://ambassadortothecomputers.blogspot.com/2010/03/inside-ocaml-objects.html)
for details). There is no way to define properties, however, only
methods. (XXX should implement `_get_foo` to generate a property
`foo`.)

###Inline Javascript###

A final way to talk to Javascript is to use inline code. This is
implemented with Camlp4 and [Jslib](Jslib.html); you need to compile
with Camlp4 and the `jslib.inline` package, then open `Ocamljs.Inline`
to get the AST types. To get an inline expression use an `<:exp< >>`
or `<< >>` quotation; for an inline statment use `<:stmt< >>`. You can
use antiquotations to refer to OCaml expressions. For example:

{% highlight ocaml %}
  let concat s1 s2 = << $s1$ + $s2$ >>
{% endhighlight %}

There is no typechecking of quotations and antiquotations. You can see
some other examples in `src/stdlib`. Caveat: the Jslib parser is incomplete.

###Callbacks###

OCaml functions are compiled to Javascript functions, so you can use
them directly as callbacks. For example, `Dom.window#setTimeout` takes
a `(unit -> unit)`.

You can use the usual `Callback.register` function in the standard
library to register a value for use from Javascript. A linked
`ocamljs` program exports a function `ocamljs$caml_named_value` which
you can use to look up registered values. You can also get access to
OCaml values by stashing them in a global from inline code.

###Strings, null###

See [Jscomp](Jscomp.html) for details about how OCaml is compiled. In
particular, when interfacing with Javascript you may need to be aware
how values are represented, particularly strings, which are not always
Javascript strings.

Most Javascript code can return null, and this can cause a runtime
failure at the Javascript level that isn't checked at compile
time. You should use `Ocamljs.is_null` before using a value that might
be null.
