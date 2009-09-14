open OUnit

type foo = Foo of int | Bar of int | Baz

let tests = "Stdlib_pervasives" >::: [
  "infinity" >:: begin fun () -> assert_equal (string_of_float infinity) "inf" end;
  "neg_infinity" >:: begin fun () -> assert_equal (string_of_float neg_infinity) "-inf" end;
  "nan" >:: begin fun () -> assert_equal (string_of_float nan) "nan" end;
  (* tests "epsilon_float" (string_of_float epsilon_float); *)(* XXX *)
  (* tests "min_float" (string_of_float min_float); *)(* XXX *)
  (* tests "max_float" (string_of_float max_float); *)(* XXX *)

  (* XXX JS doesn't give access to pointer equality so some of the =='s don't work *)
  "bool <" >:: begin fun () -> assert_bool "" (false < true) end;
  "bool >" >:: begin fun () -> assert_bool "" (not false > not true) end;

  "int <" >:: begin fun () -> assert_bool "" (1 < 2) end;
  "int ==" >:: begin fun () -> assert_bool "" (1 == 1) end;
  "int !=" >:: begin fun () -> assert_bool "" (not (1 != 1)) end;
  "int =" >:: begin fun () -> assert_bool "" (1 = 1) end;
  "int <>" >:: begin fun () -> assert_bool "" (not (1 <> 1)) end;

  "float <" >:: begin fun () -> assert_bool "" (1.7 < 2.3) end;
  (* testb "float ==" (1.7 == 1.7); *)(* XXX *)
  "float =" >:: begin fun () -> assert_bool "" (1.7 = 1.7) end;

  "string <" >:: begin fun () -> assert_bool "" ("123" < "124") end;
  (* testb "string ==" ("foo" == "foo"); *)(* XXX *)
  "string =" >:: begin fun () -> assert_bool "" ("foo" = "foo") end;

  "string mut mut =" >:: begin fun () -> assert_equal (String.copy "123") (String.copy "123") end;
  "string tuple mut mut =" >:: begin fun () -> assert_equal (String.copy "123", 1) (String.copy "123", 1) end;
  "string tuple imm mut =" >:: begin fun () -> assert_equal ("123", 1) (String.copy "123", 1) end;

  "tuple <" >:: begin fun () -> assert_bool "" ((1, 2) < (1, 3)) end;
  "tuple ==" >:: begin fun () -> assert_bool "" (not ((1, 2) == (1, 2))) end;
  "tuple =" >:: begin fun () -> assert_bool "" ((1, 2) = (1, 2)) end;

  "array <" >:: begin fun () -> assert_bool "" ([| 2 |] < [| 1; 2 |]) end;
  "array =" >:: begin fun () -> assert_bool "" ([| 1; 2 |] = [| 1; 2 |]) end;
  "array ==" >:: begin fun () -> assert_bool "" (not ([| 1; 2 |] == [| 1; 2 |])) end;

  "list <" >:: begin fun () -> assert_bool "" (not ([ 2 ] < [ 1; 2 ])) end;
  "list =" >:: begin fun () -> assert_bool "" ([ 1; 2 ] = [ 1; 2 ]) end;
  "list ==" >:: begin fun () -> assert_bool "" (not ([ 1; 2 ] == [ 1; 2 ])) end;

  "datatype = 1" >:: begin fun () -> assert_bool "" (Foo 3 = Foo 3) end;
  "datatype = 2" >:: begin fun () -> assert_bool "" (not (Foo 3 = Bar 3)) end;
  "datatype < 1" >:: begin fun () -> assert_bool "" (Foo 3 < Bar 3) end;
  "datatype < 2" >:: begin fun () -> assert_bool "" (not (Foo 3 < Baz)) end;


  "&&" >:: begin fun () -> assert_bool "" (not (try false && raise (Failure "") with _ -> true)) end;


  (* XXX most of these work OK but sprintf doesn't show the right precision *)
  (* tests "**" (Printf.sprintf "%g" (1.7 ** 2.9)); *)
  (* tests "exp" (Printf.sprintf "%g" (exp 1.7)) *)
  (* tests "acos" (Printf.sprintf "%g" (acos 0.7)); *)
  (* tests "asin" (Printf.sprintf "%g" (asin 0.7)); *)
  (* tests "atan" (Printf.sprintf "%g" (atan 1.7)); *)
  (* tests "atan2" (Printf.sprintf "%g" (atan2 1.7 2.9)); *)
  (* tests "cos" (Printf.sprintf "%g" (cos 1.7)); *)
  (* tests "cosh" (Printf.sprintf "%g" (cosh 1.7)); *)(* XXX *)
  (* tests "log" (Printf.sprintf "%g" (log 1.7)); *)
  (* tests "log10" (Printf.sprintf "%g" (log10 1.7)); *)(* XXX *)
  (* tests "sin" (Printf.sprintf "%g" (sin 1.7)); *)
  (* tests "sinh" (Printf.sprintf "%g" (sinh 1.7)); *)(* XXX *)
  (* tests "sqrt" (Printf.sprintf "%g" (sqrt 1.7)); *)
  (* tests "tan" (Printf.sprintf "%g" (tan 1.7)); *)
  (* tests "tanh" (Printf.sprintf "%g" (tanh 1.7)); *)(* XXX *)
  "ceil" >:: begin fun () -> assert_equal (Printf.sprintf "%g" (ceil 1.7)) "2" end;
  "floor" >:: begin fun () -> assert_equal (Printf.sprintf "%g" (floor 1.7)) "1" end;
  "-" >:: begin fun () -> assert_equal (Printf.sprintf "%g" (- 1.7)) "-1.7" end;

  "^" >:: begin fun () -> assert_equal ("foo" ^ "bar") "foobar" end;

  "string_of_int_of_string" >:: begin fun () -> assert_equal (string_of_int (int_of_string "08")) "8" end;
]
