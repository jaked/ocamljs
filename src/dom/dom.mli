type interval_id
type timeout_id

class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit

  method setInterval : (unit -> unit) Ocamljs.jsfun -> float -> interval_id
  method clearInterval : interval_id -> unit

  method setTimeout : (unit -> unit) Ocamljs.jsfun -> float -> timeout_id
  method clearTimeout : timeout_id -> unit
end

class type eventTarget =
object
end

class type abstractView =
object
end

class type event =
object
  method _get_bubbles : bool
  method _get_cancelable : bool
  method _get_currentTarget : eventTarget
  method _get_eventPhase : int
  method _get_target : eventTarget
  method _get_timeStamp : float
  method _get_type : string
  method initEvent : string -> bool -> bool -> unit
  method preventDefault : unit
  method stopPropagation : unit
end

class type uIEvent =
object
  inherit event

  method _get_detail : int
  method _get_view : abstractView
  method initUIEvent : string -> bool -> bool -> abstractView -> int -> unit
end

class type mouseEvent =
object
  inherit uIEvent

  method _get_altKey : bool
  method _get_button : int
  method _get_clientX : int
  method _get_clientY : int
  method _get_ctrlKey : bool
  method _get_metaKey : bool
  method _get_relatedTarget : eventTarget
  method _get_screenX : int
  method _get_screenY : int
  method _get_shiftKey : bool
  method initMouseEvent : string -> bool -> bool -> abstractView -> int -> int -> int -> int -> int -> bool -> bool -> bool -> bool -> int -> eventTarget -> unit
end

class type element =
object
  method _get_innerHTML : string
  method _set_innerHTML : string -> unit

  method addEventListener : string -> (#event -> unit) Ocamljs.jsfun -> bool -> unit
  method addEventListener_mouseEvent_ : string -> (#mouseEvent -> unit) Ocamljs.jsfun -> bool -> unit
end

class type body =
object
  inherit element

  method _get_scrollLeft : int
  method _get_scrollTop : int
end

class type document =
object
  inherit element

  method getElementById : string -> < ..>
  method _get_body : body
end

class type span =
object
  inherit element
end

class type button =
object
  inherit element

  method _set_onclick : (unit -> unit) Ocamljs.jsfun -> unit
end

val window : window
val document : document
