module JM = Javascript.Math
module D = Dom
module F = Froc
module Fd = Froc_dom
module Fda = Froc_dom_anim

class type console =
object
  method log : string -> unit
end
let console = (Ocamljs.var "console" : console)

let (>>=) = F.(>>=)

let get id = D.document#getElementById id

let int_value id = F.blift (Fd.input_value_b (get id : D.input)) int_of_string
let float_value id = F.blift (Fd.input_value_b (get id : D.input)) float_of_string

let build_list n f =
  if n < 0 then invalid_arg "n";
  let rec bl k =
    if k = n then []
    else f k :: bl (k + 1) in
  bl 0

let onload () =
  let canvas = (get "canvas" : D.canvas) in

  let balls = int_value "balls" in
  let red = int_value "red" in
  let green = int_value "green" in
  let blue = int_value "blue" in
  let radius = float_value "radius" in
  let speed = float_value "speed" in
  let mouse = Fd.mouse_b () in
  let ticks = F.count (Fd.ticks 20.) in
  let phase =
    F.blift2 ticks speed
      (fun ticks speed -> float_of_int ticks *. speed *. 0.01) in

  (* we have to recompute everything every frame, so might as well
     bind all the behaviors up front. *)
  let shapes =
    F.blift7 balls radius red green blue mouse phase
      (fun balls radius red green blue (mx, my) phase ->
        build_list balls (fun i ->
          let t = 2. *. JM.pi *. float_of_int i /. float_of_int balls +. phase in
          console#log (string_of_float t);
          Fda.disk
            (float_of_int mx +. cos t *. radius,
            float_of_int my +. sin t *. radius)
            5.
            (Fda.color red green blue))) in

  (* alternatively, lift all the functions *)
(*
  let ( *.) = F.lift2 ( *.) in
  let (/.) = F.lift2 ( /.) in
  let (+.) = F.lift2 (+.) in
  let cos = F.lift cos in
  let sin = F.lift sin in
  let float_of_int = F.lift float_of_int in
  let build_list = F.lift2 build_list in
  let disk = F.lift3 Fda.disk in
  let color = F.lift3 Fda.color in
  let fst = F.lift fst in
  let snd = F.lift snd in
  let pair = F.lift2 (fun x y -> x, y) in

  let shapes =
    build_list balls (F.return (fun i ->
      let t =
        (F.return 2.) *. (F.return JM.pi) *.
          float_of_int (F.return i) /. float_of_int balls +. phase in
      disk
        (pair
            (float_of_int (fst mouse) +. cos t *. radius)
            (float_of_int (snd mouse) +. sin t *. radius))
        (F.return 5.)
        (color red green blue))) in

  (* shape behavior list behavior -> shape list behavior *)
  let shapes = F.bind shapes (F.liftN (fun l -> l)) in
*)

  Froc_dom_anim.attach canvas shapes

;;

F.init ();
F.set_debug (fun s -> console#log s);
F.set_exn_handler (fun e -> console#log (Obj.magic e));
D.window#_set_onload (Ocamljs.jsfun onload)
