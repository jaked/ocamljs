let (|>) x f = f x

open Froc

let ticks_b msb =
  let e = make_event () in
  let id = ref None in
  let clear () =
    match !id with Some i -> Dom.window#clearInterval i; id := None | _ -> () in
  let set_interval r =
    clear ();
    match r with
      | Value p -> id := Some (Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) p)
      | Fail _ -> () (* ? *) in
  set_interval (read_result msb);
  cleanup clear;
  notify_b msb set_interval;
  e

let ticks ms =
  let e = make_event () in
  let id = Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) ms in
  cleanup (fun () -> Dom.window#clearInterval id);
  e

type 'a link = {
  l_val : 'a;
  mutable l_next : 'a link;
}

(* ensure we send delayed events in the same order they arrived *)
let send_delayed_event e de =
  let rec send de =
    if not (de.l_next == de)
    then begin
      send_result e de.l_val;
      let de_next = de.l_next in de.l_next <- de;
      send de_next;
    end in
  send de

let delay_eb t msb =
  let e = make_event () in
  let rec de = { l_val = Fail Exit; l_next = de } in
  let de_next = ref de in
  notify_e t (fun r ->
    match read_result msb with
      | (Fail _) as r ->
          de_next := { l_val = r; l_next = !de_next};
          send_delayed_event e !de_next
      | Value ms ->
          let de = { l_val = r; l_next = !de_next } in
          de_next := de;
          ignore (Dom.window#setTimeout (Ocamljs.jsfun (fun () -> send_delayed_event e de)) ms));
  e

let delay_e t ms = delay_eb t (return ms)

let delay_bb t msb =
  t |> changes |> (fun e -> delay_eb e msb) |> hold_result (read_result t)

let delay_b t ms = delay_bb t (return ms)

let mouse_e () =
  let e = make_event () in
  let f = Ocamljs.jsfun (fun me -> send e (me#_get_clientX, me#_get_clientY)) in
  Dom.document#addEventListener_mouseEvent_ "mousemove" f false;
  cleanup (fun () -> Dom.document#addEventListener_mouseEvent_ "mousemove" f false);
  e

let mouse_b () = hold (0, 0) (mouse_e ())

let attach_innerHTML elem b =
  let e = changes b in
  notify_e e (function Value s -> elem#_set_innerHTML s | _ -> ())

let input_value_e input =
  let e = make_event () in
  let f = Ocamljs.jsfun (fun _ -> send e input#_get_value) in
  input#addEventListener "change" f false;
  cleanup (fun () -> input#addEventListener "change" f false);
  e

let input_value_b input = hold input#_get_value (input_value_e input)

let node_of_result = function
  | Value v -> (v :> Dom.node)
  | Fail e -> (* ? *)
      let s = (Dom.document#createElement "span" :> Dom.node) in
      let t = (Dom.document#createTextNode ("exception") :> Dom.node) in (* XXX get Printexc working *)
      ignore (s#appendChild t);
      s

let appendChild n nb =
  let n = (n :> Dom.node) in
  let old = ref None in
  let update r =
    let c = node_of_result r in
    ignore
      (match !old with
        | None -> n#appendChild c
        | Some oc -> n#replaceChild c oc);
    old := Some c in
  update (read_result nb);
  notify_b nb update

let replaceNode n nb =
  let n = (n :> Dom.node) in
  let p = n#_get_parentNode in
  let old = ref n in
  let update r =
    let c = node_of_result r in
    ignore (p#replaceChild c !old);
    old := c in
  update (read_result nb);
  notify_b nb update

let clicks (elem : #Dom.element) =
  let e = make_event () in
  let f = Ocamljs.jsfun (fun ev -> ev#preventDefault; send e ()) in
  elem#addEventListener "click" f false;
  cleanup (fun () -> elem#removeEventListener "click" f false);
  e
