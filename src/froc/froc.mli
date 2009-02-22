(** Functional reactive programming *)
(**
   {2 Overview}

   [Froc] implements functional reactive programming in the style of
   FrTime / Flapjax (but typed). It uses the dynamic dependency graph
   of Acar et al. (adaptive functional programming). Behaviors are
   presented as monadic values, using ideas from [Lwt].

   A {e behavior} is a monadic value that can change over
   time. Binding a behavior causes the binder to be made a dependency
   of the behavior, and to be re-executed when the behavior changes.

   An {e event} is a channel over which values may be sent. Listeners
   on the channel are notified when an event occurs (i.e. a value is
   sent on the channel).

   Sent events are queued; after each event in the queue is sent and
   its listeners notified, the dependency graph is processed to update
   any affected behaviors in a consistent way.

   When a dependency of a behavior must be re-executed, all resources
   (i.e. binds and notifies) used in the previous execution are
   released, and all cleanup functions set in the previous execution
   are run (see [cleanup]).
*)

val init : unit -> unit
  (** Initialize the library. Must be called before any other function. *)

(** {2 Behaviors} *)

type 'a behavior
  (** Type of behaviors of type ['a]. *)

(** Type of values of type ['a] or exception. *)
type 'a result = Value of 'a | Fail of exn

val return : ?eq:('a -> 'a -> bool) -> 'a -> 'a behavior
  (**
     [return e] is a behavior whose result is [e].

     The optional [eq] argument gives an equality function; a
     behavior's value is considered changed (and its dependencies
     notified) only if its new value is not [eq] to its old one. The
     default equality holds iff the values [compare] to [0]
     (incomparable values are always not equal).
  *)

val fail : exn -> 'a behavior
  (**
     [fail e] is a behavior that fails with the exception [e].
  *)

val bind : 'a behavior -> ('a -> 'b behavior) -> 'b behavior
  (**
     [bind b f] behaves as [f] applied to the value of [b]. If [b]
     fails, [bind b f] also fails, with the same exception.

     When the value of a behavior changes, all functions [f] bound to
     it are re-executed.
  *)

val (>>=) : 'a behavior -> ('a -> 'b behavior) -> 'b behavior
  (**
     [b >>= f] is an alternative notation for [bind b f].
  *)

val blift : 'a behavior -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'b behavior
  (**
     [blift b ?eq f] is equivalent to [bind b (fun v -> return ?eq (f
     v))], but is slightly more efficient.
  *)

val lift : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a behavior -> 'b behavior
  (**
     [lift ?eq f b] is equivalent to [blift b ?eq f]; it can be
     partially applied to lift a function to the monad without yet
     binding it to a behavior.
  *)

val catch : (unit -> 'a behavior) -> (exn -> 'a behavior) -> 'a behavior
  (**
     [catch b f] behaves the same as [b()] if [b()] succeeds. If [b()]
     fails with some exception [e], [catch b f] behaves as [f e].
  *)

val catch_lift : (unit -> 'a behavior) -> ?eq:('a -> 'a -> bool) -> (exn -> 'a) -> 'a behavior
  (**
     [catch_lift b ?eq f] is equivalent to [catch b (fun e -> return
     ?eq (f e))], but is slightly more efficient.
  *)

val try_bind : (unit -> 'a behavior) -> ('a -> 'b behavior) -> (exn -> 'b behavior) -> 'b behavior
  (**
     [try_bind b f g] behaves as [bind (b()) f] if [b()] succeeds. If
     [b()] fails with exception [e], [try_bind b f g] behaves as [g
     e].
  *)

val try_bind_lift : (unit -> 'a behavior) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> (exn -> 'b) -> 'b behavior
  (**
     [try_bind_lift b ?eq f g] is equivalent to [try_bind b (fun v ->
     return ?eq (f v)) (fun e -> return ?eq (g e))], but is slightly
     more efficient.
  *)

val read : 'a behavior -> 'a
  (**

     [read b] returns the current value of [b]; if [b] fails with
     exception [e] it raises [e].

     Since [read] doesn't go through the dependency tracking
     machinery, it can get a stale value if called at the wrong
     time. You probably want [bind] instead.
  *)

val read_result : 'a behavior -> 'a result
  (**
     Same as [read] but returns a result instead of possibly raising
     an exception.
  *)

val notify_b : 'a behavior -> ('a result -> unit) -> unit
  (**
     Adds a listener for the result of a behavior, which is called
     whenever the result changes.
  *)

val cleanup : (unit -> unit) -> unit
  (**
     When called in the context of a binder, adds a function to be
     called when the binder must be re-executed. You can use this to
     clean up external resources.

     Binds and notifies in the context of a binder are cleaned up
     automatically.
  *)

(** {2 Events} *)

type 'a event
  (** Type of events of type ['a]. *)

val make_event : unit -> 'a event
  (** Makes a new channel for events of type ['a]. *)

val notify_e : 'a event -> ('a result -> unit) -> unit
  (**
     Adds a listener on the channel, which is called whenever an event
     is sent on it.
  *)

val send : 'a event -> 'a -> unit
  (** [send e v] calls the listeners of [e] with [Value v]. *) 

val send_exn : 'a event -> exn -> unit
  (** [send_exn e x] calls the listeners of [e] with [Fail x]. *) 

val send_result : 'a event -> 'a result -> unit
  (** [send_result e r] calls the listeners of [e] with [r]. *)

(** {2 Derived operations} *)

val switch : 'a behavior behavior -> 'a behavior
  (** [switch b] behaves as whichever behavior is currently the value of [b]. *)

val merge : 'a event list -> 'a event
  (** [merge es] is an event that fires whenever any of the events in [e] fire. *)

val map : ('a -> 'b) -> 'a event -> 'b event
  (** [map f e] is an event that fires [f v] whenever [e] fires [v]. *)

val filter : ('a -> bool) -> 'a event -> 'a event
  (** [filter p e] is an event that fires [v] whenever [e] fires [v] and [p v] is true. *)

val collect : ('b -> 'a -> 'b) -> 'b -> 'a event -> 'b event
  (**
     [collect f b e] is an event that maintains an internal state [s]
     (initialized to [b]); whenever [e] fires [v], [s'] becomes [f s
     v], the event fires [s'], and [s'] becomes the new internal
     state.
  *)

val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a behavior
  (**
     [hold v e] behaves as the last value fired by [e], or [v] if [e]
     has not yet fired a value (since [hold] was called). [eq]
     gives the equality on the resulting behavior.
  *)

val hold_result : ?eq:('a -> 'a -> bool) -> 'a result -> 'a event -> 'a behavior
  (**
     [hold_result] is the same as [hold] but initialized with a result
     instead of a value.
  *)

val changes : 'a behavior -> 'a event
  (** [changes b] fires the value of [b] whenever it changes. *)


val when_true : bool behavior -> unit event
  (** [when_true b] fires whenever [b] becomes true. *)

val count : 'a event -> int behavior
  (**
     [count e] behaves as the number of times [e] has fired (since
     [count] was called).
  *)

(** {2 Variations} *)

val bindN : 'a behavior list -> ('a list -> 'b behavior) -> 'b behavior
val bliftN : 'a behavior list -> ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'b behavior
val liftN : ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'a behavior list -> 'b behavior

val bind2 :
  'a1 behavior -> 'a2 behavior ->
  ('a1 -> 'a2 -> 'b behavior) ->
  'b behavior
val blift2 :
  'a1 behavior -> 'a2 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'b behavior
val lift2 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'a1 behavior -> 'a2 behavior ->
  'b behavior

val bind3 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'b behavior) ->
  'b behavior
val blift3 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'b behavior
val lift3 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  'b behavior

val bind4 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b behavior) ->
  'b behavior
val blift4 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'b behavior
val lift4 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  'b behavior

val bind5 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b behavior) ->
  'b behavior
val blift5 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'b behavior
val lift5 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  'b behavior

val bind6 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b behavior) ->
  'b behavior
val blift6 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'b behavior
val lift6 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  'b behavior

val bind7 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b behavior) ->
  'b behavior
val blift7 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'b behavior
val lift7 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  'b behavior

(** {2 Debugging} *)

val set_exn_handler : (exn -> unit) -> unit
  (**
     Set an exception handler which is called on exceptions from
     notification functions.
  *)

val set_debug : (string -> unit) -> unit
  (** Set a function for showing library debugging. *)
