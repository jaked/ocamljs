type 'a t

val return : 'a -> 'a t
val fail : exn -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val bind_lift : 'a t -> ('a -> 'b) -> 'b t
val (>>) : 'a t -> ('a -> 'b) -> 'b t
val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t

val init : unit -> unit
val read : 'a t -> 'a
val write : 'a t -> 'a -> unit
val write_exn : 'a t -> exn -> unit
val propagate : unit -> unit

type notify
val add_notify : 'a t -> ('a -> unit) -> notify
val remove_notify : 'a t -> notify -> unit
