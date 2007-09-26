print_endline (Printf.sprintf "%d %i %n %l %L %N" 1 2 3 4 5 6);
print_endline (Printf.sprintf "%u %x %X %o" 10 12 13 14);
print_endline (Printf.sprintf "%s %S" "foo" "bar");
print_endline (Printf.sprintf "%c %C" 'a' 'b');
(* print_endline (Printf.sprintf "%f %F %e %E %g %G" 1.0 2.0 3.0 4.0 5.0 6.0); *)(* XXX *)
print_endline (Printf.sprintf "%B %b" true false);
(* print_endline (Printf.sprintf "%nd %ni %nu %nx %nX %no" 1n 2n 3n 4n 5n 6n); *)(* XXX *)
