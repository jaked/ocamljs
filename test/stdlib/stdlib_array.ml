open OUnit

let tests = "Stdlib_array" >::: [
  "sort" >:: begin fun () ->
    let a =  [| "foo"; "bar"; "baz"; "quux" |] in
    Array.sort compare a;
    assert_equal a [| "bar"; "baz"; "foo"; "quux" |];
  end;
]
