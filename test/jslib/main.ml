open OUnit

let tests = "Jslib" >::: [
  "passes" >:: begin fun _ -> assert_bool "passes" true end;
  (* "fails" >:: begin fun _ -> assert_bool "fails" false end; *)
]

;;

OUnit.run_test_tt_main tests
