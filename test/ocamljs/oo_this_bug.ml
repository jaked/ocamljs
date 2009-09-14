(*
  in Javascript this is not lexically scoped; it's rebound when you
  apply a function. unfortunately we do that all over the place (for
  OCaml level function applications and also to implement local
  variable binding) so we need to keep track of this in a regular
  variable.
*)

open OUnit

class foo =
object (self)
  method test1 =
    "test"
  method test2 =
    (fun () -> self#test1)()
end

let tests = "Oo_this_bug" >:: begin fun () ->
  assert_equal (new foo)#test2 "test"
end
