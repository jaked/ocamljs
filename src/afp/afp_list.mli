type 'a t = Nil | Cons of 'a * 'a at
and 'a at = 'a t Afp.t

val filter : ('a -> bool) -> 'a at -> 'a at

val partition : ('a -> bool) -> 'a at -> ('a at * 'a at) Afp.t

val sort : ('a -> 'a -> int) -> 'a at -> 'a at

val to_list : 'a at -> 'a list Afp.t
