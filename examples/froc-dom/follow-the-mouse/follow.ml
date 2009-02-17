module D = Dom
module F = Froc
module Fd = Froc_dom

let onload () =

  let delay = 300. in

  let body = D.document#getElementById "body" in

  (*
    node-valued behaviors are less interesting without auto-lifting;
    we could just make the divs once and set the position in an event
    handler, but this approximates the Flapjax code.
  *)
  let div ~id ~color ~backgroundColor ~position ~padding ~left ~top cs =
    let div = D.document#createElement "div" in
    div#setAttribute "id" id;
    div#_get_style#_set_color color;
    div#_get_style#_set_backgroundColor backgroundColor;
    div#_get_style#_set_position position;
    div#_get_style#_set_padding padding;
    div#_get_style#_set_left left;
    div#_get_style#_set_top top;
    List.iter (fun c -> ignore(div#appendChild c)) (cs :> D.node list);
    div in

  let mouse = Fd.mouse_b () in

  Fd.appendChild body
    (F.blift mouse (fun (x, y) ->
      div
        ~id:"themouse"
        ~color:"#FFFFFF"
        ~backgroundColor:"#000000"
        ~position:"absolute"
        ~left:(string_of_int x)
        ~top:(string_of_int y)
        ~padding:"10px"
        [ D.document#createTextNode "the mouse!" ]));

  let mouse_offset = (D.document#getElementById "themouse")#_get_offsetWidth in
  let tail_pos = F.blift (Fd.delay_b mouse delay) (fun (x, y) -> (x + mouse_offset, y)) in

  Fd.appendChild body
    (F.blift tail_pos (fun (x, y) ->
      div
        ~id:"tail"
        ~color:"#FF0000"
        ~backgroundColor:"#000000"
        ~position:"absolute"
        ~left:(string_of_int x)
        ~top:(string_of_int y)
        ~padding:"10px"
        [ D.document#createTextNode "its tail!" ]));

  let wag_delay = delay *. 1.5 in
  let mouseandtail_offset = mouse_offset + (D.document#getElementById "tail")#_get_offsetWidth in
  let wag_offset = F.hold 0 (F.collect (fun _ _ -> (Random.int 10) - 5) 0 (Fd.ticks 100.)) in
  let wag_pos =
    F.blift2
      (Fd.delay_b mouse wag_delay) wag_offset
      (fun (x, y) wag_offset ->
        (x + mouseandtail_offset, y + wag_offset)) in

  Fd.appendChild body
    (F.blift wag_pos (fun (x, y) ->
      div
        ~id:"wagging"
        ~color:"#FFFF00"
        ~backgroundColor:"#000000"
        ~position:"absolute"
        ~left:(string_of_int x)
        ~top:(string_of_int y)
        ~padding:"10px"
        [ D.document#createTextNode "is happy!" ]));

;;

D.window#_set_onload (Ocamljs.jsfun onload)
