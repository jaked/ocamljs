module Dlist =
struct

  type 'a t = {
    data : 'a;
    mutable prev : 'a t;
    mutable next : 'a t;
  }

  let empty () =
    let rec t = { data = Obj.magic None; prev = t; next = t } in
    t

  let add_after t d =
    t.next <- { data = d; prev = t; next = t.next };
    t.next

  let remove t =
    t.next.prev <- t.prev; t.prev.next <- t.next;
    t.next <- t; t.prev <- t

  let iter f d =
    let rec loop t =
      if not (t == d)
      then (f t.data; loop t.next) in
    loop d.next

end

let debug = ref (fun _ -> ())
let set_debug f = debug := f; Timestamp.set_debug f

type 'a result = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  mutable state : 'a result;
  mutable deps : ('a result -> unit) Dlist.t;
}

type edge = {
  read : unit -> unit;
  start : Timestamp.t;
  finish : Timestamp.t;
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
  now := Timestamp.add_after !now;
  !now

let make_result s = {
  id = next_id ();
  state = s;
  deps = Dlist.empty ();
}

let return v = make_result (Value v)
let fail e = make_result (Fail e)

let handle_exn = ref (fun e -> raise e)

let write_result t s =
  if compare t.state s <> 0 (* total *)
  then
    begin
      t.state <- s;
      Dlist.iter (fun f -> try f t.state with e -> !handle_exn e) t.deps
    end

let write t v = write_result t (Value v)
let write_exn t e = write_result t (Fail e)

let read_result t = t.state

let read t =
  match t.state with
    | Value v -> v
    | Fail e -> raise e

let add_dep ts t dep =
  let dl = Dlist.add_after t.deps dep in
  Timestamp.set_cleanup ts (fun () -> Dlist.remove dl)

let add_edge t read =
  let start = tick () in
  read ();
  let e = { read = read; start = start; finish = tick () } in
  add_dep start t (fun _ -> pq := PQ.add e !pq)

let add_notify t notify =
  add_dep (tick ()) t notify

let connect t t' =
  let notify _ = write_result t t'.state in
  notify ();
  add_notify t' notify

exception Unset

let make () = make_result (Fail Unset)

let bind_gen assign t f =
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Fail e -> write_exn res e
      | Value v -> try assign res (f v) with e -> write_exn res e);
  res

let bind t f = bind_gen connect t f
let (>>=) = bind
let bind_lift t f = bind_gen write t f
let (>>) = bind_lift

let try_bind_gen assign f succ err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    try assign res (match t.state with Value v -> succ v | Fail e -> err e)
    with e -> write_exn res e);
  res

let try_bind f succ err = try_bind_gen connect f succ err
let try_bind_lift f succ err = try_bind_gen write f succ err

let catch_gen assign f err =
  let t = try f () with e -> fail e in
  let res = make () in
  add_edge t (fun () ->
    match t.state with
      | Value v -> write_result res t.state
      | Fail e -> try assign res (err e) with e -> write_exn res e);
  res

let catch f err = catch_gen connect f err
let catch_lift f err = catch_gen write f err

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

let set_exn_handler h = handle_exn := h
