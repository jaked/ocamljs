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
    let n = { data = d; prev = t; next = t.next } in
    t.next.prev <- n;
    t.next <- n;
    n

  let add_before t d =
    let n = { data = d; prev = t.prev; next = t } in
    t.prev.next <- n;
    t.prev <- n;
    n

  let remove t =
    t.next.prev <- t.prev; t.prev.next <- t.next;
    t.next <- t; t.prev <- t

  let iter f d =
    let rec loop t =
      if not (t == d)
      then (f t.data; loop t.next) in
    loop d.next

end

module TS = Froc_timestamp

let debug = ref (fun _ -> ())
let set_debug f = debug := f; TS.set_debug f

type 'a result = Value of 'a | Fail of exn

type 'a t = {
  id : int;
  event : bool;
  mutable state : 'a result;
  mutable deps : ('a result -> unit) Dlist.t;
}

type edge = {
  read : unit -> unit;
  start : TS.t;
  finish : TS.t;
}

(*
module PQ = Pqueue.Make(struct
  type t = edge
  let compare t1 t2 = TS.compare t1.start t2.start
end)
*)

module PQ : sig
  type elt = edge
  type t
  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val find_min : t -> elt
  val remove_min : t -> t
end =
struct
  type elt = edge
  type t = elt Dlist.t
  let empty = Dlist.empty ()
  let is_empty t = t.Dlist.prev == t && t.Dlist.next == t
  let add elt d =
    let rec loop t =
      if t == d || TS.compare elt.start t.Dlist.data.start = -1
      then ignore (Dlist.add_before t elt)
      else loop t.Dlist.next in
    loop d.Dlist.next;
    d
  let find_min t =
    if is_empty t
    then raise Not_found
    else t.Dlist.next.Dlist.data
  let remove_min t =
    if is_empty t
    then ()
    else Dlist.remove t.Dlist.next;
    t
end

let now = ref (TS.init ())
let pq = ref (PQ.empty)

let init () =
  now := TS.init ();
  pq := PQ.empty

let next_id =
  let next_id = ref 1 in
  fun () -> let id = !next_id in incr next_id; id

let tick () =
  now := TS.add_after !now;
  !now

let make_result s = {
  id = next_id ();
  event = false;
  state = s;
  deps = Dlist.empty ();
}

exception Unset

let make_unset ?(event=false) () = {
  id = next_id ();
  event = event;
  state = Fail Unset;
  deps = Dlist.empty ();
}

let return v = make_result (Value v)
let fail e = make_result (Fail e)

let handle_exn = ref (fun e -> raise e)

let write_result t r =
  if t.event
  then Dlist.iter (fun f -> try f r with e -> !handle_exn e) t.deps
  else if compare t.state r <> 0 (* total *)
  then begin
    t.state <- r;
    Dlist.iter (fun f -> try f r with e -> !handle_exn e) t.deps
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
  let cancel () = Dlist.remove dl in
  TS.set_cleanup ts cancel

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

let bind_gen assign t f =
  let res = make_unset () in
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
  let res = make_unset () in
  add_edge t (fun () ->
    try assign res (match t.state with Value v -> succ v | Fail e -> err e)
    with e -> write_exn res e);
  res

let try_bind f succ err = try_bind_gen connect f succ err
let try_bind_lift f succ err = try_bind_gen write f succ err

let catch_gen assign f err =
  let t = try f () with e -> fail e in
  let res = make_unset () in
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
        if not (TS.is_spliced_out e.start)
        then
          begin
            TS.splice_out e.start e.finish;
            now := e.start;
            e.read ();
          end;
        prop ()
      end in
  let now' = !now in
  prop ();
  now := now'

let set_exn_handler h = handle_exn := h
