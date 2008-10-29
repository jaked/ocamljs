type edge = {
  read : unit -> unit;
  start : Timestamp.t;
  finish : Timestamp.t;
}

type 'a result = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  time : Timestamp.t;
  mutable state : 'a result;
  mutable edges : edge list;
  mutable repr : 'a t option;
  mutable reprs : 'a t list;
  mutable notifys : (int * ('a result -> unit)) list;
}

module PQ = Pqueue.Make(struct
  type t = edge
  let compare t1 t2 = Timestamp.compare t1.start t2.start
end)

let now = ref (Timestamp.init ())
let pq = ref (PQ.empty)

let init () =
  now := Timestamp.init ();
  pq := PQ.empty

let next_id =
  let next_id = ref 1 in
  fun () -> let id = !next_id in incr next_id; id

let tick () =
  let now' = !now in
  now := Timestamp.add_after now';
  now'

let make s = {
  id = next_id ();
  time = !now;
  state = s;
  edges = [];
  repr = None;
  reprs = [];
  notifys = [];
}

let return v = make (Value v)
let fail e = make (Fail e)

let handle_exn = ref (fun e -> raise e)

let write_result t s =
  if compare t.state s <> 0 (* total *)
  then
    let rec ws t =
      t.state <- s;
      List.iter (fun (_, f) -> try f t.state with e -> !handle_exn e) t.notifys;
      t.reprs <-
        List.fold_left
          (fun reprs t -> if Timestamp.is_spliced_out t.time then reprs else (ws t; t::reprs))
          []
          t.reprs;
      List.iter (fun e -> pq := PQ.add e !pq) t.edges;
      t.edges <- [] in
    ws t

let write t v = write_result t (Value v)
let write_exn t e = write_result t (Fail e)

let read t =
  match t.state with
    | Value v -> v
    | Fail e -> raise e

let add_edge t f =
  let rec read () =
    let start = tick () in
    f ();
    let e = { read = read; start = start; finish = !now } in
    t.edges <- e::t.edges in
  read ()

let connect t t' =
  (*
    XXX
    space leak: if a repr node is abandoned it is still referenced by
    its repr. not sure how often this comes up in real code but here
    is an example:

      let x = return 0
      let y = return 1

      let z = x >>= fun _ ->
        ignore(x >>= fun _ -> y);
        return 2

    this is partially fixed by timestamping repr nodes then cleaning
    up spliced out ones in write_state. maybe too lazy? also doesn't
    handle abandoned nodes that are never spliced out.
  *)
  match t.repr with
    | Some r when r.id = t'.id -> ()
    | tr ->
        write_result t t'.state;
        t.repr <- Some t';
        t'.reprs <- t::t'.reprs;
        match tr with
          | Some r ->  r.reprs <- List.filter (fun t' -> t'.id <> t.id) r.reprs
          | None -> ()

let disconnect_result t result =
  write_result t result;
  match t.repr with
    | None -> ()
    | Some r ->
        t.repr <- None;
        r.reprs <- List.filter (fun t' -> t'.id <> t.id) r.reprs

let disconnect_exn t e = disconnect_result t (Fail e)
let disconnect t v = disconnect_result t (Value v)

exception Unset

let make () = make (Fail Unset)

let bind t f =
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Fail e -> disconnect_exn res e
      | Value v ->
          try connect res (f v)
          with e -> disconnect_exn res e);
  res

let (>>=) = bind

let bind_lift t f =
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Fail e -> write_exn res e
      | Value v -> try write res (f v) with e -> write_exn res e);
  res

let (>>) = bind_lift

let try_bind f succ err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    try connect res (match t.state with Value v -> succ v | Fail e -> err e)
    with e -> disconnect_exn res e);
  res

let try_bind_lift f succ err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    try write res (match t.state with Value v -> succ v | Fail e -> err e)
    with e -> write_exn res e);
  res

let catch f err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Value v -> disconnect res v
      | Fail e ->
          try connect res (err e)
          with e -> disconnect_exn res e);
  res

let catch_lift f err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Value v -> write res v
      | Fail e ->
          try write res (err e)
          with e -> write_exn res e);
  res

let propagate () =
  let rec prop () =
    if not (PQ.is_empty !pq)
    then
      begin
        let e = PQ.find_min !pq in
        pq := PQ.remove_min !pq;
        if not (Timestamp.is_spliced_out e.start)
        then
          begin
            Timestamp.splice_out e.start e.finish;
            now := e.start;
            e.read ();
          end;
        prop ()
      end in
  let now' = !now in
  prop ();
  now := now'

type notify = int

let add_notify t f =
  let id = next_id () in
  t.notifys <- (id, f)::t.notifys;
  id

let remove_notify t id =
  t.notifys <- List.filter (fun (id', _) -> id' <> id) t.notifys

let set_exn_handler h = handle_exn := h
