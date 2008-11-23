(* mask out Event and Behavior so we can override them *)
(* XXX not sure if this is any better than assigning individual fields *)
module F :
sig
  val init : unit -> unit
  val set_debug : (string -> unit) -> unit
  val set_exn_handler : (exn -> unit) -> unit

  type 'a result = 'a Froc.result = Value of 'a | Fail of exn

  val hold : 'a -> 'a Froc.Event.t -> 'a Froc.Behavior.t
  val hold_result : 'a result -> 'a Froc.Event.t -> 'a Froc.Behavior.t
  val changes : 'a Froc.Behavior.t -> 'a Froc.Event.t
  val when_true : bool Froc.Behavior.t -> unit Froc.Event.t
  val count : 'a Froc.Event.t -> int Froc.Behavior.t
end =
struct
  include Froc
end
include F

module B = Froc.Behavior
module E = Froc.Event

let (|>) x f = f x

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
    update (B.read_result nb);
    ignore (E.add_notify (F.changes nb) update)

  let replaceNode n nb =
    let n = (n :> Dom.node) in
    let p = n#_get_parentNode in
    let old = ref n in
    let update r =
      let c = node_of_result r in
      ignore (p#replaceChild c !old);
      old := c in
    update (B.read_result nb);
    ignore (E.add_notify (F.changes nb) update)

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
