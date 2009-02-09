type 'a t

val return : ?eq:('a -> 'a -> bool) -> 'a -> 'a t
val fail : exn -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val lift : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
val blift : 'a t -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'b t

val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
val catch_lift : (unit -> 'a t) -> ?eq:('a -> 'a -> bool) -> (exn -> 'a) -> 'a t
val try_bind_lift : (unit -> 'a t) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> (exn -> 'b) -> 'b t

val read : 'a t -> 'a
val write : 'a t -> 'a -> unit
val write_exn : 'a t -> exn -> unit
val init : unit -> unit
val propagate : unit -> unit

val set_debug : (string -> unit) -> unit
