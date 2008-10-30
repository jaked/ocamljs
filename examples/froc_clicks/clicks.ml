class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit
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

let onload () =
  let clicks = (document#getElementById "clicks" : span) in
  let click = (document#getElementById "click" : button) in

  let state =
    let e = E.make () in
    click#_set_onclick (Ocamljs.jsfun (fun () -> E.send e ()));
    E.collect (fun n () -> n + 1) 0 e in

  ignore(E.add_notify state (fun r ->
    match r with
      | E.Value cs -> clicks#_set_innerHTML (string_of_int cs)
      | _ -> ()))

;;

window#_set_onload (Ocamljs.jsfun onload)
