---
layout: page
title: Binding to jQuery
---
#Binding to jQuery

The Findlib package is `jquery`. See the [Ocamldoc](doc/JQuery.html)
and [example](examples/jquery/live_examples/). Thanks to Dave Benjamin
for contributing this library.

There is a syntax extension (`jquery.syntax` package), which just
changes the precedence of the method call operator `#`, in order to
allow method chaining, e.g.

{% highlight ocaml %}
  let size = ~$"div.contentToChange" #find "p" #size in
{% endhighlight %}

(where `~$` is the jQuery `$` operator).
