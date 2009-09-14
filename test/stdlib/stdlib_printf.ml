open OUnit

let tests = "Stdlib_printf" >::: [
  "1" >:: begin fun () -> assert_equal (Printf.sprintf "%d %i %n %l %L %N" 1 2 3 4 5 6) "1 2 3 4 5 6" end;
  "2" >:: begin fun () -> assert_equal (Printf.sprintf "%u %x %X %o" 10 12 13 14) "10 c D 16" end;
  "3" >:: begin fun () -> assert_equal (Printf.sprintf "%s %S" "foo" "bar") "foo \"bar\"" end;
  "4" >:: begin fun () -> assert_equal (Printf.sprintf "%c %C" 'a' 'b') "a 'b'" end;
  (* print_endline (Printf.sprintf "%f %F %e %E %g %G" 1.0 2.0 3.0 4.0 5.0 6.0); *)(* XXX *)
  "5" >:: begin fun () -> assert_equal (Printf.sprintf "%B %b" true false) "true false" end;
  (* print_endline (Printf.sprintf "%nd %ni %nu %nx %nX %no" 1n 2n 3n 4n 5n 6n); *)(* XXX *)
]
