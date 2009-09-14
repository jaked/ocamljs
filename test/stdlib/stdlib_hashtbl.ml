open OUnit

let tests = "Stdlib_hashtbl" >:: begin fun () ->
  let t = Hashtbl.create 7 in
  Hashtbl.add t "foo" "bar";
  Hashtbl.add t "foo" "baz";
  assert_equal (Hashtbl.find t "foo") "baz";

  let t = Hashtbl.create 7 in
  Hashtbl.add t `Foo "bar";
  Hashtbl.add t `Foo "baz";
  assert_equal (Hashtbl.find t `Foo) "baz"
end
