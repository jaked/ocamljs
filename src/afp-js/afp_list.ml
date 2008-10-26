type 'a t = Nil | Cons of 'a * 'a t Afp.t

let (>>=) = Afp.(>>=)

let rec filter p l =
  l >>= function
    | Nil -> Afp.return Nil
    | Cons (h, t) ->
        let ft = filter p t in
        if p h
        then Afp.return (Cons (h, ft))
        else ft

let rec to_list l =
  l >>= function
    | Nil -> Afp.return []
    | Cons (h, t) ->
        to_list t >>= fun t ->
          Afp.return (h::t)
