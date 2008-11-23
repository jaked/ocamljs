include Froc_ddg

module Behavior =
struct
  include Froc_ddg

  let switch bb = bb >>= fun b -> b
end

module Event =
struct
  include Froc_ddg

  let make () = make_unset ~event:true ()

  let merge ts =
    let t = make () in
    List.iter (fun t' -> add_notify t' (write_result t)) ts;
    t

  let map f t =
    let t' = make () in
    add_notify t
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
    let t' = make () in
    add_notify t
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
    let t' = make () in
    let s = ref (Value init) in
    add_notify t
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

  let init () =
    Queue.clear q

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
end

let hold_result init e =
  let b = Behavior.make_result init in
  Event.add_notify e (Behavior.write_result b);
  b

let hold init e = hold_result (Value init) e

let changes b =
  let e = Event.make () in
  Behavior.add_notify b (Event.send_result e);
  e

let when_true b =
  Event.map (fun b -> ()) (Event.filter (fun b -> b) (changes b))

let count e = hold 0 (Event.collect (fun n _ -> n + 1) 0 e)

let init () =
  Event.init ();
  Behavior.init ();
