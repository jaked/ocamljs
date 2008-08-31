(* parse and pretty print Javascript *)

try
  let ss = Jslib_parse.parse_stdin () in
  Jslib_pp.stmts Format.std_formatter ss;
  Format.fprintf Format.std_formatter "\n";
  Format.pp_print_flush Format.std_formatter ()
with e ->
  Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print e
