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

let rec partition p l =
  l >>= function
    | Nil -> return (return Nil, return Nil)
    | Cons (h, t) ->
        partition p t >> fun (yes, no) ->
          if p h
          then (return (Cons (h, yes)), no)
          else (yes, return (Cons (h, no)))

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
