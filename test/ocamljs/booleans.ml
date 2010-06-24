open OUnit
open Ocamljs.Inline

type t = {
  mutable x : bool;
}

let tests = "Booleans" >::: [
(*
  "literals" >:: begin fun () ->
    assert_bool "true" << true  === $true$ >>;
    assert_bool "false" << false === $false$ >>;
  end;

  "record" >:: begin fun () ->
    let x = { x = false } in
    x.x <- true;
    assert_bool "" << true === $x.x$ >>;
  end;
*)
]
