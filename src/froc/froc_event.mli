type 'a t

val make : unit -> 'a t

val send : 'a -> 'a t
val merge : 'a t list -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val collect : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
val hold : 'a -> 'a t -> 'a Froc_behavior.t
val changes : 'a Froc_behavior.t

type notify
val add_notify : 'a t -> ('a -> unit) -> notify
val remove_notify : 'a t -> notify -> unit
