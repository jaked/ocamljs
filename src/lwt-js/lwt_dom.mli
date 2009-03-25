val sleep : float -> unit Lwt.t
      (** [sleep d] is a threads which remain suspended for [d] seconds
          (letting other threads run) and then terminates. *)
val yield : unit -> unit Lwt.t
      (** [yield ()] is a threads which suspends itself (letting other
          thread run) and then resumes as soon as possible and
          terminates. *)

val http_request : ?headers:(string * string) list -> [ `Get | `Post of string ] -> string -> Dom.xMLHttpRequest Lwt.t
