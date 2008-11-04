type t

val init : unit -> t
val add_after : t -> t
val splice_out : t -> t -> unit
val is_spliced_out : t -> bool
val compare : t -> t -> int

val set_debug : (string -> unit) -> unit
