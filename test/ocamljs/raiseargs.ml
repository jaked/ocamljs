open OUnit

type 'a triple = Smaller of 'a * 'a * 'a | Bigger of 'a * 'a * 'a

let print_triple tr = match tr with
  | Smaller(l,m,s)
  | Bigger (s,m,l) ->
      s ^ " " ^ m ^ " " ^ l

let tests = "Raisargs" >:: begin fun () ->
  assert_equal (print_triple (Smaller("Large", "Medium", "Small"))) "Small Medium Large";
  assert_equal (print_triple (Bigger ("Small", "Medium", "Large"))) "Small Medium Large";
end
