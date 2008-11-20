let debug = ref (fun _ -> ())
let set_debug f = debug := f

type t = {
  mutable id : int;
  mutable next : t;
  mutable cleanup : unit -> unit;
}

let is_spliced_out t = t.id = -1

let check t =
  if is_spliced_out t
  then raise (Invalid_argument "spliced out timestamp")

let next_id =
  let next_id = ref 1 in
  fun () -> let id = !next_id in incr next_id; id

let init () =
  let rec s = { id = 0; next = s; cleanup = ignore } in
  { id = next_id (); next = s; cleanup = ignore }

let add_after t =
  check t;
  let t' = { id = next_id (); next = t.next; cleanup = ignore } in
  t.next <- t';
  t'

let set_cleanup t cleanup =
  check t;
  t.cleanup <- cleanup

let splice_out t1 t2 =
  check t1;
  check t2;
  let rec loop t =
    match t.id with
      | -1 -> assert false
      | 0 -> raise (Invalid_argument "t1 >= t2")
      | id when id = t2.id -> ()
      | _ -> t.id <- -1; t.cleanup (); t.cleanup <- ignore; loop t.next in
  loop t1.next;
  t1.next <- t2

let compare t1 t2 =
  (*
    spliced-out timestamps are less than everything else

    by splicing out timestamps we disturb the heap invariant. I think
    this is benign; a spliced-out timestamp might not be the min of
    the heap when it should, but there's no harm in leaving it around
    since we just ignore it when it is the min.
  *)
  match t1.id, t2.id with
    | id1, id2 when id1 = id2 -> 0
    | -1, _ -> -1
    | _, -1 -> 1
    | _, _ ->
        let rec loop t =
          match t.id with
            | -1 -> assert false
            | 0 -> 1
            | id when id = t2.id -> -1
            | _ -> loop t.next in
        loop t1.next
