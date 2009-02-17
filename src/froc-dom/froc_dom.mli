val ticks : float -> unit Froc.event
val ticks_b : float Froc.behavior -> unit Froc.event

val delay_e : 'a Froc.event -> float -> 'a Froc.event
val delay_eb : 'a Froc.event -> float Froc.behavior -> 'a Froc.event
val delay_b : 'a Froc.behavior -> float -> 'a Froc.behavior
val delay_bb : 'a Froc.behavior -> float Froc.behavior -> 'a Froc.behavior

val mouse_e : unit -> (int * int) Froc.event
val mouse_b : unit -> (int * int) Froc.behavior

val attach_innerHTML : #Dom.element -> string Froc.behavior -> unit

val input_value_e : #Dom.input -> string Froc.event
val input_value_b : #Dom.input -> string Froc.behavior

val appendChild : #Dom.node -> #Dom.node Froc.behavior -> unit
val replaceNode : #Dom.node -> #Dom.node Froc.behavior -> unit

val clicks : #Dom.element -> unit Froc.event
