open OUnit
open Stdlib_parsing_stuff.Parsetree

let tests = "Stdlib_parsing" >::: [
  "parse" >:: begin fun() ->
    let lexbuf = Lexing.from_string "type t = unit" in
    (* ignore (Parsing.set_trace true); *)
    match Stdlib_parsing.implementation Stdlib_lexing.token lexbuf with
      | [ { pstr_desc = Pstr_type [ "t", _ ] } ] -> ()
      | _ -> assert_failure ""
  end
]
