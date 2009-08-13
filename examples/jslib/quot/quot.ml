(* pretty print Javascript from a quotation *)

let _loc = Camlp4.PreCast.Loc.ghost

let ss = [
  <:stmt< return foo(); >>;
  <:stmt< return new Foo; >>;
  <:stmt< return new Foo(); >>;
] in

List.iter (fun s ->
  Jslib_pp.stmt Format.std_formatter s;
  Format.fprintf Format.std_formatter "\n") ss;
Format.pp_print_flush Format.std_formatter ()
