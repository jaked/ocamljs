type 'a t

val return : 'a -> 'a t
val fail : exn -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val bind_lift : 'a t -> ('a -> 'b) -> 'b t
val (>>) : 'a t -> ('a -> 'b) -> 'b t
val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
val catch_lift : (unit -> 'a t) -> (exn -> 'a) -> 'a t
val try_bind_lift : (unit -> 'a t) -> ('a -> 'b) -> (exn -> 'b) -> 'b t

val read : 'a t -> 'a
val write : 'a t -> 'a -> unit
val write_exn : 'a t -> exn -> unit
val init : unit -> unit
val propagate : unit -> unit

type 'a result = Value of 'a | Fail of exn
val read_result : 'a t -> 'a result
val write_result : 'a t -> 'a result -> unit
val make_result : 'a result -> 'a t
val add_notify : 'a t -> ('a result -> unit) -> unit
val set_exn_handler : (exn -> unit) -> unit
val make : unit -> 'a t
val connect : 'a t -> 'a t -> unit

val set_debug : (string -> unit) -> unit
