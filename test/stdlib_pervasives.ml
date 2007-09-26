let testb t b = print_endline (t ^ " : " ^ if b then "true" else "false")
let tests t s = print_endline (t ^ " : " ^ s)

type foo = Foo of int | Bar of int | Baz

;;

tests "infinity" (string_of_float infinity);
tests "neg_infinity" (string_of_float neg_infinity);
tests "nan" (string_of_float nan);
(* tests "epsilon_float" (string_of_float epsilon_float); *)(* XXX *)
(* tests "min_float" (string_of_float min_float); *)(* XXX *)
(* tests "max_float" (string_of_float max_float); *)(* XXX *)

(* XXX JS doesn't give access to pointer equality so some of the =='s don't work *)
testb "bool <" (false < true);
testb "bool >" (not false > not true);

testb "int <" (1 < 2);
testb "int ==" (1 == 1);
testb "int !=" (1 != 1);
testb "int =" (1 = 1);
testb "int <>" (1 <> 1);

testb "float <" (1.7 < 2.3);
(* testb "float ==" (1.7 == 1.7); *)(* XXX *)
testb "float =" (1.7 = 1.7);

testb "string <" ("123" < "124");
(* testb "string ==" ("foo" == "foo"); *)(* XXX *)
testb "string =" ("foo" = "foo");

testb "string mut mut =" ((String.copy "123") = (String.copy "123"));
testb "string tuple mut mut =" ((String.copy "123", 1) = (String.copy "123", 1));
testb "string tuple imm mut =" (("123", 1) = (String.copy "123", 1));

testb "tuple <" ((1, 2) < (1, 3));
testb "tuple ==" ((1, 2) == (1, 2));
testb "tuple =" ((1, 2) = (1, 2));

testb "array <" ([| 2 |] < [| 1; 2 |]);
testb "array =" ([| 1; 2 |] = [| 1; 2 |]);
testb "array ==" ([| 1; 2 |] == [| 1; 2 |]);

testb "list <" ([ 2 ] < [ 1; 2 ]);
testb "list =" ([ 1; 2 ] = [ 1; 2 ]);
testb "list ==" ([ 1; 2 ] == [ 1; 2 ]);

testb "datatype = 1" (Foo 3 = Foo 3);
testb "datatype = 2" (Foo 3 = Bar 3);
testb "datatype < 1" (Foo 3 < Bar 3);
testb "datatype < 2" (Foo 3 < Baz);


testb "&&" (try false && raise (Failure "") with _ -> true);


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
tests "ceil" (Printf.sprintf "%g" (ceil 1.7));
tests "floor" (Printf.sprintf "%g" (floor 1.7));
tests "-" (Printf.sprintf "%g" (- 1.7));


tests "^" ("foo" ^ "bar");


tests "string_of_int_of_string" (string_of_int (int_of_string "08"))
