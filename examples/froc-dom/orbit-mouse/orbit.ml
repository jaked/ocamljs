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

let onload () =
  let canvas = (D.document#getElementById "canvas" : D.canvas) in

  let shape =
    F.blift Fd.mouse_b (fun (x, y) -> [
      (fun ctx -> ctx#_set_fillStyle "rgb(200,0,0)");
      (fun ctx -> ctx#fillRect (float_of_int x) (float_of_int y) 55. 50.);
    ]) in

  Froc_dom_anim.attach canvas shape

;;

F.init ();
F.set_debug (fun s -> console#log s);
F.set_exn_handler (fun e -> console#log (Obj.magic e));
D.window#_set_onload (Ocamljs.jsfun onload)
