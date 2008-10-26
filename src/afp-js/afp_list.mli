type 'a t = Nil | Cons of 'a * 'a t Afp.t

val filter : ('a -> bool) -> 'a t Afp.t -> 'a t Afp.t

val to_list : 'a t Afp.t -> 'a list Afp.t
