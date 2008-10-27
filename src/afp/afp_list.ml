type 'a t = Nil | Cons of 'a * 'a at
and 'a at = 'a t Afp.t

open Afp

let rec filter p l =
  l >>= function
    | Nil -> return Nil
    | Cons (h, t) ->
        let ft = filter p t in
        if p h
        then return (Cons (h, ft))
        else ft

(*
  bad adaptive behavior: we bind the recursive call, so a change to
  the last element of the input causes changes all the way up.
*)
(*
let rec partition p l =
  l >>= function
    | Nil -> return (return Nil, return Nil)
    | Cons (h, t) ->
        partition p t >> fun (yes, no) ->
          if p h
          then (return (Cons (h, yes)), no)
          else (yes, return (Cons (h, no)))
*)

(*
  ok adaptive behavior internally, but bad for callers: a change to
  the last element of the input causes the first element of the output
  to change.
*)
(*
let partition p l =
  let rec part yes no l =
    l >>= function
      | Nil -> return (yes, no)
      | Cons (h, t) ->
          if p h
          then part (return (Cons (h, yes))) no t
          else part yes (return (Cons (h, no))) t in
  part (return Nil) (return Nil) l
*)

(*
  worse constant factor since we traverse the list twice, but good
  adaptive behavior: a change to the last element of the input causes
  at worst a change in the last elements of the partitions.
*)
let partition p l =
  return (filter p l, filter (fun x -> not (p x)) l)

let sort cmp l =
  let rec qs l sorted =
    l >>= function
      | Nil -> return sorted
      | Cons (h, t) ->
          partition (fun x -> cmp x h < 0) t >>= fun (lt, ge) ->
            qs lt (Cons (h, qs ge sorted)) in
  qs l Nil

let rec to_list l =
  l >>= function
    | Nil -> return []
    | Cons (h, t) ->
        to_list t >>= fun t ->
          return (h::t)
