module F = Froc
module B = Froc.Behavior
module E = Froc.Event

let (|>) x f = f x

let init = F.init

type 'a result = 'a F.result = Value of 'a | Fail of exn

let delay_event t ms =
  let e = E.make () in
  ignore
    (E.add_notify t (fun r ->
      ignore
        (Dom.window#setTimeout (Ocamljs.jsfun (fun () -> E.send_result e r)) ms)));
  e

let delay_eventb t msb =
  let e = E.make () in
  ignore
    (E.add_notify t (fun r ->
      try
        ignore
          (Dom.window#setTimeout (Ocamljs.jsfun (fun () -> E.send_result e r)) (B.read msb))
      with _ -> ())); (* ? *)
  e

module Behavior =
struct
  include B

  let attach_innerHTML elem b =
    let e = F.changes b in
    ignore (E.add_notify e (function Value s -> elem#_set_innerHTML s | _ -> ()))

  let delay t ms =
    let r = read_result t in
    t |> F.changes |> (fun e -> delay_event e ms) |> F.hold_result r

  let delayb t msb =
    let r = read_result t in
    t |> F.changes |> (fun e -> delay_eventb e msb) |> F.hold_result r
end

module Event =
struct
  include E

  let clicks elem =
    let e = make () in
    elem#_set_onclick (Ocamljs.jsfun (fun () -> send e ()));
    e

  let ticks period =
    let e = make () in
    ignore (Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) period);
    e

  let ticksb pb =
    let e = make () in
    let id = ref None in
    let set_interval r =
      (match !id with Some i -> Dom.window#clearInterval i; id := None | _ -> ());
      match r with
        | Value p -> id := Some (Dom.window#setInterval (Ocamljs.jsfun (fun () -> send e ())) p)
        | Fail _ -> () (* ? *) in
    set_interval (B.read_result pb);
    ignore (add_notify (F.changes pb) set_interval);
    e

  let delay = delay_event
  let delayb = delay_eventb
end

(* modules can't be multiply defined *)
let hold = F.hold
let changes = F.changes
let switch = F.switch
let when_true = F.when_true
let count = F.count
