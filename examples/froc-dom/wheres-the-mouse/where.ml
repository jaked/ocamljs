module D = Dom
module F = Froc
module Fd = Froc_dom

let onload () =

  let span_of_string s =
    let sp = D.document#createElement "span" in
    let t = (D.document#createTextNode s :> D.node) in
    ignore (sp#appendChild t);
    sp in

  let mouse = Fd.mouse_b () in

  Fd.replaceNode
    (D.document#getElementById "Mleft")
    (F.blift mouse (fun (x, y) -> span_of_string (string_of_int x)));
  Fd.replaceNode
    (D.document#getElementById "Mtop")
    (F.blift mouse (fun (x, y) -> span_of_string (string_of_int y)))

;;

D.window#_set_onload (Ocamljs.jsfun onload)
