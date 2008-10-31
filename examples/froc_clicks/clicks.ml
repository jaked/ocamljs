module D = Dom
module F = Froc_dom
module E = Froc_dom.Event
module B = Froc_dom.Behavior

let (>>=) = B.(>>=)

let onload () =
  let clicks = F.count (E.clicks (D.document#getElementById "click")) in
  let ticks = F.count (E.ticks 1000.) in

  B.attach_innerHTML
    (D.document#getElementById "clicks")
    (clicks >>= fun c -> B.return (string_of_int c));

  B.attach_innerHTML
    (D.document#getElementById "seconds")
    (ticks >>= fun s -> B.return (string_of_int s));

  B.attach_innerHTML
    (D.document#getElementById "difference")
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

D.window#_set_onload (Ocamljs.jsfun onload)
