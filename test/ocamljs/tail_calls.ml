open OUnit
open Ocamljs.Inline

let tests = "Tail_calls" >::: [
  "enough recursion" >:: begin fun () ->
    let rec loop = function
      | 0 -> true
      | n -> loop (n - 1) in
    (* SpiderMonkey has 999 frame limit *)
    assert_bool "" (loop 10000);
  end;

  "tail call from js callback" >:: begin fun () ->
    (* simulate a callback by setting $in_tail false *)
    let foo () = 3 in
    let bar () = foo () in
    let baz = 0 in
    <:stmt<
      $$in_tail = false;
      $exp:baz$ = $bar$ ();
    >>;
    assert_equal baz 3
  end;

  (*
    don't have a good idea how to support this case. maybe we could
    explicitly check whether the previous function on the stack is _m?

  "tail call from js" >:: begin fun () ->
    let foo () = 3 in
    let bar () = foo () in
    let baz = 0 in
    <:stmt<
      $exp:baz$ = $bar$ ();
    >>;
    assert_equal baz 3
  end;
  *)
]
