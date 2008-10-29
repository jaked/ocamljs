type 'a result = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  mutable notifys : (int * ('a result -> unit)) list
}

let next_id =
  let next_id = ref 1 in
  fun () -> let id = !next_id in incr next_id; id

let handle_exn = ref (fun e -> raise e)

type notify = int

let add_notify t f =
  let id = next_id () in
  t.notifys <- (id, f)::t.notifys;
  id

let remove_notify t id =
  t.notifys <- List.filter (fun (id', _) -> id' <> id) t.notifys

let set_exn_handler h = handle_exn := h

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
  ignore (add_notify t
             (function
               | Value v -> send t' (f v)
               | Fail e -> send_exn t' e));
  t'

let filter f t =
  let t' = make () in
  ignore (add_notify t
             (function
               | Value v -> if f v then send t' v
               | Fail e -> send_exn t' e)); (* ? *)
  t'

let collect f b t = failwith "unimplemented"
let hold b t = failwith "unimplemented"
let changes b = failwith "unimplemented"    
