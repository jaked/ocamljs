let testb t b = print_endline (t ^ " : " ^ if b then "true" else "false")

;;

let a =  [| "foo"; "bar"; "baz"; "quux" |] in
Array.sort compare a;
testb "sort" (a = [| "bar"; "baz"; "foo"; "quux" |])
