type 'a t = Nil | Cons of 'a * 'a at
and 'a at = 'a t Afp.t

val exists : ('a -> bool) -> 'a at -> bool Afp.t

val filter : ('a -> bool) -> 'a at -> 'a at

val map : ('a -> 'b) -> 'a at -> 'b at

val partition : ('a -> bool) -> 'a at -> ('a at * 'a at) Afp.t

val sort : ('a -> 'a -> int) -> 'a at -> 'a at

val to_list : 'a at -> 'a list Afp.t
