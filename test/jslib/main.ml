open OUnit

let tests = "Jslib" >::: [
  Parse.tests
]

;;

OUnit.run_test_tt_main tests
