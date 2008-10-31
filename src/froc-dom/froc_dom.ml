module F = Froc
module B = Froc.Behavior
module E = Froc.Event

module Behavior =
struct
  include B

  let attach_innerHTML elem b =
    let e = F.changes b in
    ignore
      (E.add_notify e
          (fun r -> match r with E.Value s -> elem#_set_innerHTML s | _ -> ()))
end

module Event =
struct
  include E

  let clicks elem =
    let e = E.make () in
    elem#_set_onclick (Ocamljs.jsfun (fun () -> E.send e ()));
    e

  let ticks period =
    let e = E.make () in
    Dom.window#setInterval (Ocamljs.jsfun (fun () -> E.send e ())) period;
    e
end

(* modules can't be multiply defined *)
let hold = F.hold
let changes = F.changes
let switch = F.switch
let when_true = F.when_true
let count = F.count
