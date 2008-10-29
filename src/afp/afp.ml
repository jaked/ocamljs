type edge = {
  read : unit -> unit;
  start : Timestamp.t;
  finish : Timestamp.t;
}

type 'a result = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  mutable state : 'a result;
  mutable edges : edge list;
  mutable repr : 'a t option;
  mutable reprs : ('a t * Timestamp.t) list;
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
  state = s;
  edges = [];
  repr = None;
  reprs = [];
  notifys = [];
}

let return v = make (Value v)
let fail e = make (Fail e)

let handle_exn = ref (fun e -> raise e)

let write_state t s =
  if compare t.state s <> 0 (* total *)
  then
    let rec ws t =
      t.state <- s;
      List.iter (fun (_, f) -> try f t.state with e -> !handle_exn e) t.notifys;
      t.reprs <-
        List.fold_left
          (fun reprs ((t, ts) as r) -> if Timestamp.is_spliced_out ts then reprs else (ws t; r::reprs))
          []
          t.reprs;
      List.iter (fun e -> pq := PQ.add e !pq) t.edges;
      t.edges <- [] in
    ws t

let write t v = write_state t (Value v)
let write_exn t e = write_state t (Fail e)

let read t =
  match t.state with
    | Value v -> v
    | Fail e -> raise e

let add_edge t e = t.edges <- e::t.edges

let apply f x = try f x with e -> make (Fail e)
let apply2 f x1 x2 = try f x1 x2 with e -> make (Fail e)
let apply3 f x1 x2 x3 = try f x1 x2 x3 with e -> make (Fail e)

let connect t ts t' =
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

    this is partially fixed by timestamping repr nodes then cleaning up
    spliced out ones in write_state. maybe too lazy?
  *)
  match t.repr with
    | Some r when r.id = t'.id -> ()
    | tr ->
        write_state t t'.state;
        t.repr <- Some t';
        t'.reprs <- (t, ts)::t'.reprs;
        match tr with
          | Some r ->  r.reprs <- List.filter (fun (t', _) -> t'.id <> t.id) r.reprs
          | None -> ()

exception Unset

let bind t f =
  let res = make (Fail Unset) in
  let time = !now in
  let rec read () =
    match t.state with
      | Value v ->
          let start = tick () in
          connect res time (apply f v);
          let e = { read = read; start = start; finish = !now } in
          t.edges <- e::t.edges
      | Fail e -> write_exn res e in
  read ();
  res

let (>>=) = bind

let bind2 t1 t2 f =
  let res = make (Fail Unset) in
  let time = !now in
  let rec read () =
    match t1.state, t2.state with
      | Value v1, Value v2 ->
          let start = tick () in
          connect res time (apply2 f v1 v2);
          let e = { read = read; start = start; finish = !now } in
          t1.edges <- e::t1.edges;
          t2.edges <- e::t2.edges;
      | Fail e, _
      | _, Fail e -> write_exn res e in
  read ();
  res

let bind3 t1 t2 t3 f =
  let res = make (Fail Unset) in
  let time = !now in
  let rec read () =
    match t1.state, t2.state, t3.state with
      | Value v1, Value v2, Value v3 ->
          let start = tick () in
          connect res time (apply3 f v1 v2 v3);
          let e = { read = read; start = start; finish = !now } in
          t1.edges <- e::t1.edges;
          t2.edges <- e::t2.edges;
          t3.edges <- e::t3.edges;
      | Fail e, _, _
      | _, Fail e, _
      | _, _, Fail e -> write_exn res e in
  read ();
  res

let bind_lift t f =
  let res = make (Fail Unset) in
  let rec read () =
    match t.state with
      | Value v ->
          let start = tick () in
          (try write res (f v) with e -> write_exn res e);
          let e = { read = read; start = start; finish = !now } in
          t.edges <- e::t.edges
      | Fail e -> write_exn res e in
  read ();
  res

let (>>) = bind_lift

let bind_lift2 t1 t2 f =
  let res = make (Fail Unset) in
  let rec read () =
    match t1.state, t2.state with
      | Value v1, Value v2 ->
          let start = tick () in
          (try write res (f v1 v2) with e -> write_exn res e);
          let e = { read = read; start = start; finish = !now } in
          t1.edges <- e::t1.edges;
          t2.edges <- e::t2.edges;
      | Fail e, _
      | _, Fail e -> write_exn res e in
  read ();
  res

let bind_lift3 t1 t2 t3 f =
  let res = make (Fail Unset) in
  let rec read () =
    match t1.state, t2.state, t3.state with
      | Value v1, Value v2, Value v3 ->
          let start = tick () in
          (try write res (f v1 v2 v3) with e -> write_exn res e);
          let e = { read = read; start = start; finish = !now } in
          t1.edges <- e::t1.edges;
          t2.edges <- e::t2.edges;
          t3.edges <- e::t3.edges;
      | Fail e, _, _
      | _, Fail e, _
      | _, _, Fail e -> write_exn res e in
  read ();
  res

let try_bind f succ fail =
  let t = apply f () in
  match t.state with
    | Value v -> apply succ v
    | Fail e -> apply fail e

let catch f fail = try_bind f return fail

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
