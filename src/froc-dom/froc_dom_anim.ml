let draw canvas instrs =
  let ctx = canvas#getContext "2d" in
  ctx#clearRect 0. 0. (float_of_int canvas#_get_width) (float_of_int canvas#_get_height);
  List.iter (fun f -> f ctx) instrs

let attach
    (canvas : Dom.canvas)
    (instrsb : (Dom.canvasRenderingContext2D -> unit) list Froc.behavior) =
  let notify = function
    | Froc.Value instrs -> draw canvas instrs
    | Froc.Fail _ -> () (* XXX ? *) in
  (* maybe notify_b should notify when you first call it? *)
  notify (Froc.read_result instrsb);
  Froc.notify_b instrsb notify
