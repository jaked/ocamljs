open OUnit
open Ocamljs.Inline

type t = {
  mutable x : bool;
}

let tests = "Booleans" >::: [
  "literals" >:: begin fun () ->
    assert_bool "true" << true  === $true$ >>;
    assert_bool "false" << false === $false$ >>;
  end;

  "record" >:: begin fun () ->
    let x = { x = false } in
    x.x <- true;
    assert_bool "" << true === $x.x$ >>;
  end;

  "match" >:: begin fun () ->
    (*
      test for true compiles to if (!= match 0);
      translated to if (!!match) in js since OCaml bools become js bools
    *)
    match true, true with
      | true, true -> ()
      | _ -> assert_failure "fall through"
  end;

  "eq" >:: begin fun () ->
    let x = Some () in
    assert_bool "=" << false === $x = None$ >>;
    assert_bool "<>" << true === $x <> None$ >>;
  end;
]
