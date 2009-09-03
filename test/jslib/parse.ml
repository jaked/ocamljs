open OUnit
open Jslib_ast

let parse_exp_string s =
  Jslib_parse.Gram.parse_string
    Jslib_parse.expression
    (Jslib_parse.Gram.Loc.mk "<string>")
    s

let assert_exp s f =
  assert_bool s (f (parse_exp_string s))

let tests = "Parse" >::: [
  "div" >:: begin fun () ->
    assert_exp
      "1 / 2"
      (function
        | Jbinop (_, Jdiv, Jnum (_, "1"), Jnum (_, "2")) -> true
        | _ -> false)
  end;

  "regexp" >:: begin fun () ->
    assert_exp
      "1 / /2/"
      (function
        | Jbinop (_, Jdiv, Jnum (_, "1"), Jregexp (_, "2", "")) -> true
        | _ -> false)
  end;
]
