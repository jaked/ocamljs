(* framework *)

class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit
  method setInterval : (unit -> unit) Ocamljs.jsfun -> float -> unit
end

class type document =
object
  method getElementById : string -> < ..>
end

class type span =
object
  method _set_innerHTML : string -> unit
end

class type button =
object
  method _set_onclick : (unit -> unit) Ocamljs.jsfun -> unit
end

(* FireBug console *)
class type console =
object
  method log : string -> unit
end

let window = (Ocamljs.var "window" : window)
let document = (Ocamljs.var "document" : document)
let console = (Ocamljs.var "console" : console)

module F = Froc
module E = Froc.Event
module B = Froc.Behavior

let (>>=) = B.(>>=)

let attach_innerHTML elem b =
  let e = F.changes b in
  ignore
    (E.add_notify e
        (fun r -> match r with E.Value s -> elem#_set_innerHTML s | _ -> ()))

let clicks elem =
  let e = E.make () in
  elem#_set_onclick (Ocamljs.jsfun (fun () -> E.send e ()));
  F.hold 0 (E.collect (fun n () -> n + 1) 0 e)

let ticks period =
  let e = E.make () in
  window#setInterval (Ocamljs.jsfun (fun () -> E.send e ())) period;
  F.hold 0 (E.collect (fun n () -> n + 1) 0 e)

(* application *)

let onload () =
  let clicks = clicks (document#getElementById "click") in
  let ticks = ticks 1000. in

  attach_innerHTML
    (document#getElementById "clicks")
    (clicks >>= fun c -> B.return (string_of_int c));
  attach_innerHTML
    (document#getElementById "seconds")
    (ticks >>= fun s -> B.return (string_of_int s));
  attach_innerHTML
    (document#getElementById "difference")
    (clicks >>= fun clicks ->
      ticks >>= fun ticks ->
        let s =
          if clicks = ticks
          then "same number of clicks as ticks"
          else if clicks > ticks
          then string_of_int (clicks - ticks) ^ " more clicks than ticks"
          else string_of_int (ticks - clicks) ^ " more ticks than clicks" in
        B.return s)

;;

window#_set_onload (Ocamljs.jsfun onload)
