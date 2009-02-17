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

let onload () =
  let canvas = (get "canvas" : D.canvas) in

  let int_value id = F.blift (Fd.input_value_b (get id : D.input)) int_of_string in

  let radius = int_value "radius" in
  let speed = int_value "speed" in
  let balls = int_value "balls" in
  let red = int_value "red" in
  let green = int_value "green" in
  let blue = int_value "blue" in
  let mouse = Fd.mouse_b () in

  let shapes =
    F.bind3 red green blue (fun r g b ->
      let rgb = Printf.sprintf "rgb(%d,%d,%d)" r g b in
      console#log rgb;
      F.blift mouse (fun (x, y) -> [
        (fun ctx -> ctx#_set_fillStyle rgb);
        (fun ctx -> ctx#fillRect (float_of_int x) (float_of_int y) 55. 50.);
      ])) in

  Froc_dom_anim.attach canvas shapes

;;

F.init ();
F.set_debug (fun s -> console#log s);
F.set_exn_handler (fun e -> console#log (Obj.magic e));
D.window#_set_onload (Ocamljs.jsfun onload)
