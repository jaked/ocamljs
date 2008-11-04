let debug = ref (fun _ -> ())
let set_debug f = debug := f

module F = Froc
module B = Froc.Behavior
module E = Froc.Event

let (|>) x f = f x

let init = F.init

type 'a result = 'a F.result = Value of 'a | Fail of exn

type 'a link = {
  l_val : 'a;
  mutable l_next : 'a link;
}

(* ensure we send delayed events in the same order they arrived *)
let send_delayed_event e de =
  let rec send de =
    if not (de.l_next == de)
    then begin
      E.send_result e de.l_val;
      let de_next = de.l_next in de.l_next <- de;
      send de_next;
    end in
  send de

let delay_eventb t msb =
  let e = E.make () in
  let rec de = { l_val = Fail Exit; l_next = de } in
  let de_next = ref de in
  ignore
    (E.add_notify t (fun r ->
      match B.read_result msb with
        | (Fail _) as r ->
            de_next := { l_val = r; l_next = !de_next};
            send_delayed_event e !de_next
        | Value ms ->
            let de = { l_val = r; l_next = !de_next } in
            de_next := de;
            ignore (Dom.window#setTimeout (Ocamljs.jsfun (fun () -> send_delayed_event e de)) ms)));
  e

let mouse_event =
  let e = E.make () in
  Dom.document#addEventListener_mouseEvent_
    "mousemove"
    (Ocamljs.jsfun (fun me -> E.send e (me#_get_clientX, me#_get_clientY)))
    false;
  e

module Behavior =
struct
  include B

  let mouse = F.hold (0, 0) mouse_event

  let attach_innerHTML elem b =
    let e = F.changes b in
    ignore (E.add_notify e (function Value s -> elem#_set_innerHTML s | _ -> ()))

  let delayb t msb =
    t |> F.changes |> (fun e -> delay_eventb e msb) |> F.hold_result (read_result t)

  let delay t ms = delayb t (B.return ms)

end

module Event =
struct
  include E

  let mouse = mouse_event

  let clicks elem =
    let e = make () in
    elem#_set_onclick (Ocamljs.jsfun (fun () -> send e ()));
    e

  let ticksb msb =
    let e = make () in
    let id = ref None in
    let set_interval r =
      (match !id with Some i -> Dom.window#clearInterval i; id := None | _ -> ());
      match r with
        | Value p -> id := Some (Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) p)
        | Fail _ -> () (* ? *) in
    set_interval (B.read_result msb);
    ignore (add_notify (F.changes msb) set_interval);
    e

  let ticks ms =
    let e = make () in
    ignore (Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) ms);
    e

  let delayb = delay_eventb

  let delay t ms = delayb t (B.return ms)
end

(* modules can't be multiply defined *)
let hold = F.hold
let changes = F.changes
let switch = F.switch
let when_true = F.when_true
let count = F.count
