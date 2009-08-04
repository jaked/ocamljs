let t = Hashtbl.create 7 in
Hashtbl.add t "foo" "bar";
Hashtbl.add t "foo" "baz";
print_endline (Hashtbl.find t "foo");

let t = Hashtbl.create 7 in
Hashtbl.add t `Foo "bar";
Hashtbl.add t `Foo "baz";
print_endline (Hashtbl.find t `Foo)
