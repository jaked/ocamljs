type edge = {
  read : unit -> unit;
  start : Timestamp.t;
  finish : Timestamp.t;
}

type notify = {
  nid : int;
  notify : unit -> unit;
}

type 'a state = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  mutable state : 'a state;
  mutable edges : edge list;
  mutable repr : 'a t option;
  mutable reprs : 'a t list;
  mutable notifys : notify list;
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

let write_state t s =
  (* prerr_endline "write_state"; *)
  if compare t.state s <> 0 (* total *)
  then
    let rec ws t =
      t.state <- s;
      (* prerr_endline "write_state: notify"; *)
      List.iter (fun n -> n.notify ()) t.notifys;
      (* prerr_endline "write_state: reprs"; *)
      List.iter ws t.reprs;
      (* prerr_endline "write_state: edges"; *)
      List.iter (fun e -> pq := PQ.add e !pq) t.edges;
      (* prerr_endline "write_state: done"; *)
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

let connect t t' =
  (* prerr_endline "connect"; *)
  (*
    XXX
    we have a space leak because when a t is forgotten, it remains
    linked from its repr. I can't think how to fix it though.
  *)
  begin
  if not (t.id = t'.id) (* can this happen? *)
  then
    match t.repr with
      | Some r when r = t' -> ()
      | tr ->
          (* prerr_endline "connect: write_state"; *)
          write_state t t'.state;
          (* prerr_endline "connect: repr"; *)
          t.repr <- Some t';
          (* prerr_endline "connect: reprs"; *)
          t'.reprs <- t::t'.reprs;
          (* prerr_endline "connect: r.reprs"; *)
          match tr with
            | Some r ->
                (* prerr_endline "connect: filter"; *)
                r.reprs <- List.filter (fun t' -> t'.id <> t.id) r.reprs;
                (* prerr_endline "connect: after filter"; *)
            | None -> ()
  end;
  (* prerr_endline "connect done"; *)

exception Unset

let bind t f =
  let res = make (Fail Unset) in
  let rec read () =
    match t.state with
      | Value v ->
          let start = tick () in
          connect res (apply f v);
          let e = { read = read; start = start; finish = !now } in
          t.edges <- e::t.edges
      | Fail e -> write_exn res e in
  read ();
  res

let (>>=) = bind

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
        (* prerr_endline "propagate: find_min"; *)
        let e = PQ.find_min !pq in
        pq := PQ.remove_min !pq;
        if not (Timestamp.is_spliced_out e.start)
        then
          begin
            Timestamp.splice_out e.start e.finish;
            now := e.start;
            (* prerr_endline "propagate: edge read"; *)
            e.read ();
          end;
        prop ()
      end in
  let now' = !now in
  prop ();
  now := now'

let add_notify_state t f =
  let notify () = try f t.state with _ -> () in
  let n = { nid = next_id (); notify = notify } in
  t.notifys <- n::t.notifys;
  n

let add_notify t f =
  add_notify_state t (function Value v -> f v | _ -> ())

let add_notify_exn t f =
  add_notify_state t (function Fail e -> f e | _ -> ())

let remove_notify t n =
  t.notifys <- List.filter (fun n' -> n'.nid <> n.nid) t.notifys
