module D = Dom
module F = Froc_dom
module E = Froc_dom.Event
module B = Froc_dom.Behavior

let (>>) = B.(>>)

let onload () =

  let span_of_string s =
    let sp = Dom.document#createElement "span" in
    let t = (Dom.document#createTextNode s :> Dom.node) in
    ignore (sp#appendChild t);
    sp in

  B.replaceNode
    (D.document#getElementById "Mleft")
    (B.mouse >> fun (x, y) -> span_of_string (string_of_int x));
  B.replaceNode
    (D.document#getElementById "Mtop")
    (B.mouse >> fun (x, y) -> span_of_string (string_of_int y))

;;

D.window#_set_onload (Ocamljs.jsfun onload)
