(*
  in Javascript this is not lexically scoped; it's rebound when you
  apply a function. unfortunately we do that all over the place (for
  OCaml level function applications and also to implement local
  variable binding) so we need to keep track of this in a regular
  variable.
*)

class foo =
object (self)
  method test1 =
    print_endline "test"
  method test2 =
    (fun () -> self#test1)()
end

;;

(new foo)#test2
