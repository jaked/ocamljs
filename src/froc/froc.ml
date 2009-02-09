include Froc_ddg

type 'a event = 'a t
type 'a behavior = 'a t
let notify_e = notify
let notify_b = notify

let switch bb = bb >>= fun b -> b

let make_event () = make ~event:true ()

let merge ts =
  let t = make_event () in
  List.iter (fun t' -> notify t' (write_result t)) ts;
  t

let map f t =
  let t' = make_event () in
  notify t
    (fun r ->
      let r =
        match r with
          | Fail e -> Fail e
          | Value v ->
              try Value (f v)
              with e -> Fail e in
      write_result t' r);
  t'

let filter p t =
  let t' = make_event () in
  notify t
    (fun r ->
      let r =
        match r with
          | Fail _ -> Some r
          | Value v ->
              try if p v then Some (Value v) else None
              with e -> Some (Fail e) in (* ? *)
      match r with Some r -> write_result t' r | _ -> ());
  t'

let collect f init t =
  let t' = make_event () in
  let s = ref (Value init) in
  notify t
    (fun r ->
      let r =
        match !s, r with
          | Fail _, _ -> None (* ? *)
          | _, Fail e -> Some (Fail e)
          | Value sv, Value v ->
              try Some (Value (f sv v))
              with e -> Some (Fail e) in
      match r with Some r -> s := r; write_result t' r | _ -> ());
  t'

let q = Queue.create ()
let running = ref false

let run_queue () =
  running := true;
  while not (Queue.is_empty q) do
    let f = Queue.take q in
    f ();
    propagate ()
  done;
  running := false

let enqueue f =
  Queue.add f q;
  if not !running
  then run_queue ()

let send_result t r = enqueue (fun () -> write_result t r)
let send t v = send_result t (Value v)
let send_exn t e = send_result t (Fail e)

let hold_result ?eq init e =
  let b = make ?eq ~result:init () in
  notify e (write_result b);
  b

let hold ?eq init e = hold_result ?eq (Value init) e

let changes b =
  let e = make_event () in
  notify b (send_result e);
  e

let when_true b =
  map (fun b -> ()) (filter (fun b -> b) (changes b))

let count e = hold 0 (collect (fun n _ -> n + 1) 0 e)

let init () =
  init ();
  Queue.clear q
