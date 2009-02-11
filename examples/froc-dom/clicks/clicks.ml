module D = Dom
module F = Froc
module Fd = Froc_dom

let (>>=) = F.(>>=)

let onload () =
  let clicks = F.count (Fd.clicks (D.document#getElementById "click")) in
  let ticks = F.count (Fd.ticks 1000.) in

  Fd.attach_innerHTML
    (D.document#getElementById "clicks")
    (clicks >>= fun c -> F.return (string_of_int c));

  Fd.attach_innerHTML
    (D.document#getElementById "seconds")
    (ticks >>= fun s -> F.return (string_of_int s));

  Fd.attach_innerHTML
    (D.document#getElementById "difference")
    (F.bind2
        clicks ticks
        (fun clicks ticks ->
          let s =
            if clicks = ticks
            then "same number of clicks as ticks"
            else if clicks > ticks
            then string_of_int (clicks - ticks) ^ " more clicks than ticks"
            else string_of_int (ticks - clicks) ^ " more ticks than clicks" in
          F.return s))

;;

D.window#_set_onload (Ocamljs.jsfun onload)
