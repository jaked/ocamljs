module Behavior =
struct
  include Afp
end

module Event =
struct

  type 'a result = 'a Behavior.result = Value of 'a | Fail of exn

  let handle_exn = ref (fun e -> raise e)

  let set_exn_handler h =
    Behavior.set_exn_handler h;
    handle_exn := h

  type notify = int

  type 'a t = {
    id : int;
    mutable notifys : (int * ('a result -> unit)) list
  }

  let next_id =
    let next_id = ref 1 in
    fun () -> let id = !next_id in incr next_id; id

  let add_notify t f =
    let id = next_id () in
    t.notifys <- (id, f)::t.notifys;
    id

  let remove_notify t id =
    t.notifys <- List.filter (fun (id', _) -> id' <> id) t.notifys

  let make () = {
    id = next_id ();
    notifys = [];
  }

  let send_result t r =
    List.iter (fun (_, f) -> try f r with e -> !handle_exn e) t.notifys

  let send t v = send_result t (Value v)
  let send_exn t e = send_result t (Fail e)

  let merge ts =
    let t = make () in
    List.iter (fun t' -> ignore (add_notify t' (send_result t))) ts;
    t

  let map f t =
    let t' = make () in
    ignore
      (add_notify t
          (fun r ->
            let r =
              match r with
                | Fail e -> Fail e
                | Value v ->
                    try Value (f v)
                    with e -> Fail e in
            send_result t' r));
    t'

  let filter p t =
    let t' = make () in
    ignore
      (add_notify t
          (fun r ->
            let r =
              match r with
                | Fail _ -> Some r
                | Value v ->
                    try if p v then Some (Value v) else None
                    with e -> Some (Fail e) in (* ? *)
            match r with Some r -> send_result t' r | _ -> ()));
    t'

  let collect f init t =
    let t' = make () in
    let s = ref (Value init) in
    ignore
      (add_notify t
          (fun r ->
            let r =
              match !s, r with
                | Fail _, _ -> None (* ? *)
                | _, Fail e -> Some (Fail e)
                | Value sv, Value v ->
                    try Some (Value (f sv v))
                    with e -> Some (Fail e) in
            match r with Some r -> s := r; send_result t' r | _ -> ()));
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
      Behavior.propagate ()
    done;
    running := false

  let enqueue f =
    Queue.add f q;
    if not !running
    then run_queue ()

  let send t v = enqueue (fun () -> send t v)
  let send_exn t e = enqueue (fun () -> send_exn t e)
end

let hold init e =
  let b = Behavior.return init in
  ignore
    (Event.add_notify e (Behavior.write_result b));
  b

let changes b =
  let e = Event.make () in
  ignore
    (Behavior.add_notify b (Event.send_result e));
  e

let switch init e =
  let res = Behavior.make () in
  Behavior.connect res init;
  ignore
    (Event.add_notify e (function
      | Event.Fail e -> Behavior.disconnect_result res (Behavior.Fail e)
      | Event.Value v -> Behavior.connect res v));
  res

let when_true b =
  Event.map (fun b -> ()) (Event.filter (fun b -> b) (changes b))

let init () =
  Event.init ();
  Behavior.init ();
