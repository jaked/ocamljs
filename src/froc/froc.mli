type 'a result = Value of 'a | Fail of exn
type 'a event
type 'a behavior

val init : unit -> unit
val set_debug : (string -> unit) -> unit
val set_exn_handler : (exn -> unit) -> unit

val return : ?eq:('a -> 'a -> bool) -> 'a -> 'a behavior
val fail : exn -> 'a behavior

val bind : 'a behavior -> ('a -> 'b behavior) -> 'b behavior
val (>>=) : 'a behavior -> ('a -> 'b behavior) -> 'b behavior
val lift : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a behavior -> 'b behavior
val blift : 'a behavior -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'b behavior

val catch : (unit -> 'a behavior) -> (exn -> 'a behavior) -> 'a behavior
val try_bind : (unit -> 'a behavior) -> ('a -> 'b behavior) -> (exn -> 'b behavior) -> 'b behavior
val catch_lift : (unit -> 'a behavior) -> ?eq:('a -> 'a -> bool) -> (exn -> 'a) -> 'a behavior
val try_bind_lift : (unit -> 'a behavior) -> ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> (exn -> 'b) -> 'b behavior

val read : 'a behavior -> 'a
val read_result : 'a behavior -> 'a result
val notify_b : 'a behavior -> ('a result -> unit) -> unit
val switch : 'a behavior behavior -> 'a behavior

val make_event : unit -> 'a event
val send : 'a event -> 'a -> unit
val send_exn : 'a event -> exn -> unit
val send_result : 'a event -> 'a result -> unit

val notify_e : 'a event -> ('a result -> unit) -> unit
val cleanup : (unit -> unit) -> unit

val merge : 'a event list -> 'a event
val map : ('a -> 'b) -> 'a event -> 'b event
val filter : ('a -> bool) -> 'a event -> 'a event
val collect : ('b -> 'a -> 'b) -> 'b -> 'a event -> 'b event

val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a event -> 'a behavior
val hold_result : ?eq:('a -> 'a -> bool) -> 'a result -> 'a event -> 'a behavior
val changes : 'a behavior -> 'a event
val when_true : bool behavior -> unit event
val count : 'a event -> int behavior

val bind2 :
  'a1 behavior -> 'a2 behavior ->
  ('a1 -> 'a2 -> 'b behavior) ->
  'b behavior
val lift2 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'a1 behavior -> 'a2 behavior ->
  'b behavior
val blift2 :
  'a1 behavior -> 'a2 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'b) ->
  'b behavior

val bind3 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'b behavior) ->
  'b behavior
val lift3 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  'b behavior
val blift3 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'b behavior

val bind4 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b behavior) ->
  'b behavior
val lift4 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  'b behavior
val blift4 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) ->
  'b behavior

val bind5 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b behavior) ->
  'b behavior
val lift5 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  'b behavior
val blift5 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) ->
  'b behavior

val bind6 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b behavior) ->
  'b behavior
val lift6 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  'b behavior
val blift6 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'b) ->
  'b behavior

val bind7 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b behavior) ->
  'b behavior
val lift7 :
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  'b behavior
val blift7 :
  'a1 behavior -> 'a2 behavior -> 'a3 behavior -> 'a4 behavior -> 'a5 behavior -> 'a6 behavior -> 'a7 behavior ->
  ?eq:('b -> 'b -> bool) ->
  ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'b) ->
  'b behavior

val bindN : 'a behavior list -> ('a list -> 'b behavior) -> 'b behavior
val liftN : ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'a behavior list -> 'b behavior
val bliftN : 'a behavior list -> ?eq:('b -> 'b -> bool) -> ('a list -> 'b) -> 'b behavior
