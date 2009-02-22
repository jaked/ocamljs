(** Adaptive functional programming *)
(**
   {2 Overview}

   [Froc_afp] implements adaptive functional programming following
   Acar et al. Changeable values are presented as a monad, using ideas
   from [Lwt].

   A {e changeable} is a monadic value that can change over
   time. Binding a changeable causes the binder to be made a
   dependency of the changeable, and to be re-executed when the
   changeable changes.

   Adaptive functional programming proceeds in phases: after an
   initial computation, call [write] to change some inputs, then
   [propagate] to make the result consistent again.
*)

val init : unit -> unit
  (** Initialize the library. Must be called before any other function. *)

(** {2 Changeables} *)

type 'a t
    (** Type of changeables of type ['a]. *)

val return : ?eq:('a -> 'a -> bool) -> 'a -> 'a t
  (**
     [return v] is a changeable with initial value [v].

     The optional [eq] argument gives an equality function; a
     changeable is considered changed (and its dependencies notified)
     only if its new value is not [eq] to its old one. The default
     equality holds iff the values [compare] to [0] (incomparable
     values are always not equal).
  *)

val fail : exn -> 'a t
  (**
     [fail e] is a changeable that fails with the exception [e].
  *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
  (**
     [bind c f] behaves as [f] applied to the value of [c]. If [c]
     fails, [bind c f] also fails, with the same exception.

     When the value of a changeable changes, all functions [f] bound to
     it are re-executed.
  *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (**
     [c >>= f] is an alternative notation for [bind c f].
  *)

val blift : 'a t -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'b t
  (**
     [blift c ?eq f] is equivalent to [bind c (fun v -> return ?eq (f
     v))], but is slightly more efficient.
  *)

val lift : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t
  (**
     [lift ?eq f c] is equivalent to [blift c ?eq f]; it can be
     partially applied to lift a function to the monad without yet
     binding it to a changeable.
  *)

val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  (**
     [catch c f] behaves the same as [c()] if [c()] succeeds. If [c()]
     fails with some exception [e], [catch c f] behaves as [f e].
  *)

val catch_lift : (unit -> 'a t) -> ?eq:('a -> 'a -> bool) -> (exn -> 'a) -> 'a t
  (**
     [catch_lift c ?eq f] is equivalent to [catch c (fun e -> return
     ?eq (f e))], but is slightly more efficient.
  *)

val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
  (**
     [try_bind c f g] behaves as [bind (c()) f] if [c()] succeeds. If
     [c()] fails with exception [e], [try_bind c f g] behaves as [g
     e].
  *)

val try_bind_lift : (unit -> 'a t) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> (exn -> 'b) -> 'b t
  (**
     [try_bind_lift c ?eq f g] is equivalent to [try_bind c (fun v ->
     return ?eq (f v)) (fun e -> return ?eq (g e))], but is slightly
     more efficient.
  *)

val read : 'a t -> 'a
  (**
     [read c] returns the value of [c], or raises an exception if [c] fails.

     You shouldn't call [read] in the context of a binder, since you
     might get a stale result.
  *)

val write : 'a t -> 'a -> unit
  (**
     [write c v] updates the value of [c]. Changeables that depend on
     [c] will not be consistent until you call [propagate].
  *)

val write_exn : 'a t -> exn -> unit
  (**
     [write_exn c e] updates [c] to fail with exception
     [e]. Changeables that depend on [c] will not be consistent until
     you call [propagate].
  *)

val propagate : unit -> unit
  (** Process any outstanding changes so all changeables are consistent. *)

(** {2 Variations} *)

val bindN : 'a t list -> ('a list -> 'b t) -> 'b t
val bliftN : 'a t list -> ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'b t
val liftN : ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'a t list -> 'b t

val bind2 :
  'a1 t -> 'a2 t ->
  ('a1 -> 'a2 -> 'b t) ->
  'b t
val blift2 :
  'a1 t -> 'a2 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'b t
val lift2 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'a1 t -> 'a2 t ->
  'b t

val bind3 :
  'a1 t -> 'a2 t -> 'a3 t ->
  ('a1 -> 'a2 -> 'a3 -> 'b t) ->
  'b t
val blift3 :
  'a1 t -> 'a2 t -> 'a3 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'b t
val lift3 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'a1 t -> 'a2 t -> 'a3 t ->
  'b t

val bind4 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b t) ->
  'b t
val blift4 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'b t
val lift4 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t ->
  'b t

val bind5 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b t) ->
  'b t
val blift5 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'b t
val lift5 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
  'b t

val bind6 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b t) ->
  'b t
val blift6 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'b t
val lift6 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
  'b t

val bind7 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b t) ->
  'b t
val blift7 :
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'b t
val lift7 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
  'b t

(** {2 Debugging} *)

val set_debug : (string -> unit) -> unit
  (** Set a function for showing library debugging. *)
