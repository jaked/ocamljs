type color = string
type point = float * float
type shape = Dom.canvasRenderingContext2D -> unit

let color ?a r g b =
    match a with
      | None -> Printf.sprintf "rgb(%d,%d,%d)" r g b
      | Some a -> Printf.sprintf "rgba(%d,%d,%d,%d)" r g b a

let disk (cx, cy) radius color : shape =
  (fun ctx ->
    ctx#_set_fillStyle color;
    ctx#beginPath;
    ctx#arc cx cy radius 0. (2. *. Javascript.Math.pi) true;
    ctx#fill)

let draw canvas instrs =
  let ctx = canvas#getContext "2d" in
  ctx#clearRect 0. 0. (float_of_int canvas#_get_width) (float_of_int canvas#_get_height);
  ListLabels.iter instrs ~f:(fun f ->
    ctx#save;
    f ctx;
    ctx#closePath;
    ctx#restore)

let attach
    (canvas : Dom.canvas)
    (instrsb : (Dom.canvasRenderingContext2D -> unit) list Froc.behavior) =
  let notify = function
    | Froc.Value instrs -> draw canvas instrs
    | Froc.Fail _ -> () (* XXX ? *) in
  (* maybe notify_b should notify when you first call it? *)
  notify (Froc.read_result instrsb);
  Froc.notify_b instrsb notify
