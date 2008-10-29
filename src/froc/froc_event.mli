type 'a t

val make : unit -> 'a t

val send : 'a t -> 'a -> unit
val send_exn : 'a t -> exn -> unit
val merge : 'a t list -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val collect : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
val hold : 'a -> 'a t -> 'a Froc_behavior.t
val changes : 'a Froc_behavior.t -> 'a t

type notify
type 'a result = Value of 'a | Fail of exn
val add_notify : 'a t -> ('a result -> unit) -> notify
val remove_notify : 'a t -> notify -> unit
val send_result : 'a t -> 'a result -> unit
val set_exn_handler : (exn -> unit) -> unit
