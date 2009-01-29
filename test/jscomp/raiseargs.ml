type 'a triple = Smaller of 'a * 'a * 'a | Bigger of 'a * 'a * 'a

let print_triple tr = match tr with
  | Smaller(l,m,s)
  | Bigger (s,m,l) ->
      print_endline (s ^ " " ^ m ^ " " ^ l)

let _ =
  print_triple (Smaller("Large", "Medium", "Small"));
  print_triple (Bigger ("Small", "Medium", "Large"))
