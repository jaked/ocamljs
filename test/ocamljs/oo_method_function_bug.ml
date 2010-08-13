open OUnit
open Ocamljs.Inline

let foo x = x + 1

let o =
object
  method foo = foo
  method bar x = x + 1
end

let tests = "Oo_method_function_bug" >:: begin fun () ->
  assert_equal (o#bar 1) 2;
  assert_equal << $o$.bar(1) >> 2;
  assert_equal (o#foo 1) 2;
  assert_equal << $o$.foo(1) >> 2;
end
