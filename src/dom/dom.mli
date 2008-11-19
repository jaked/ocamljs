type interval_id
type timeout_id

class type node =
object
  method appendChild : node -> node
  method removeChild : node -> node
  method replaceChild : node -> node -> node
  method _get_parentNode : node
end

class type abstractView =
object
end

class type eventTarget =
object
  method addEventListener : string -> (event -> unit) Ocamljs.jsfun -> bool -> unit
  method addEventListener_mouseEvent_ : string -> (mouseEvent -> unit) Ocamljs.jsfun -> bool -> unit
end

and event =
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

and uIEvent =
object
  inherit event

  method _get_detail : int
  method _get_view : abstractView
  method initUIEvent : string -> bool -> bool -> abstractView -> int -> unit
end

and mouseEvent =
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

class type style =
object
  method _set_color : string -> unit
  method _set_backgroundColor : string -> unit
  method _set_position : string -> unit
  method _set_left : string -> unit
  method _set_top : string -> unit
  method _set_padding : string -> unit
end

class type element =
object
  inherit node
  inherit eventTarget

  method getAttribute : string -> string
  method setAttribute : string -> string -> unit

  method _get_style : style

  method _get_offsetWidth : int

  method _get_innerHTML : string
  method _set_innerHTML : string -> unit
end

class type characterData =
object
  inherit node

  method _get_data : string
end

class type text =
object
  inherit characterData
end

class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit

  method setInterval : (unit -> unit) Ocamljs.jsfun -> float -> interval_id
  method clearInterval : interval_id -> unit

  method setTimeout : (unit -> unit) Ocamljs.jsfun -> float -> timeout_id
  method clearTimeout : timeout_id -> unit
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

  method createElement : string -> #element
  method createTextNode : string -> text
  method getElementById : string -> #element
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

class type image =
object
end

class type canvas =
object
  inherit element

  method _get_width : int
  method _set_width : int -> unit
  method _get_height : int
  method _set_height : int -> unit

  method toDataUrl : string
  method toDataUrl_args_ : string -> 'a -> string

  method getContext : string -> canvasRenderingContext2D
end

and canvasRenderingContext2D =
object
  method _get_canvas : canvas

  method save : unit -> unit
  method restore : unit -> unit

  method scale : float -> float -> unit
  method rotate : float -> unit
  method translate : float -> float -> unit
  method transform : float -> float -> float -> float -> float -> float -> unit
  method setTransform : float -> float -> float -> float -> float -> float -> unit

  method _get_globalAlpha : float
  method _set_globalAlpha : float -> unit
  method _get_globalCompositeOperation : string
  method _set_globalCompositeOperation : string -> unit

  method _get_strokeStyle : string
  method _set_strokeStyle : string -> unit
  method _get_strokeStyle_canvasGradient_ : canvasGradient
  method _set_strokeStyle_canvasGradient_ : canvasGradient -> unit
  method _get_strokeStyle_canvasPattern_ : canvasPattern
  method _set_strokeStyle_canvasPattern_ : canvasPattern -> unit
  method _get_fillStyle : string
  method _set_fillStyle : string -> unit
  method _get_fillStyle_canvasGradient_ : canvasGradient
  method _set_fillStyle_canvasGradient_ : canvasGradient -> unit
  method _get_fillStyle_canvasPattern_ : canvasPattern
  method _set_fillStyle_canvasPattern_ : canvasPattern -> unit

  method createLinearGradient : float -> float -> float -> float -> canvasGradient
  method createRadialGradient : float -> float -> float -> float -> float -> float -> canvasGradient

  method createPattern : image -> string -> canvasPattern
  method createPattern_canvas_ : canvas -> string -> canvasPattern

  method _get_lineWidth : float
  method _set_lineWidth : float -> unit
  method _get_lineCap : string
  method _set_lineCap : string -> unit
  method _get_miterLimit : float
  method _set_miterLimit : float -> unit

  method _get_shadowOffsetX : float
  method _set_shadowOffsetX : float -> unit
  method _get_shadowOffsetY : float
  method _set_shadowOffsetY : float -> unit
  method _get_shadowBlur : float
  method _set_shadowBlur : float -> unit
  method _get_shadowColor : string
  method _set_shadowColor : string -> unit

  method clearRect : float -> float -> float -> float -> unit
  method fillRect : float -> float -> float -> float -> unit
  method strokeRect : float -> float -> float -> float -> unit

  method beginPath : unit
  method closePath : unit
  method moveTo : float -> float -> unit
  method lineTo : float -> float -> unit
  method quadraticCurveTo : float -> float -> float -> float -> unit
  method bezierCurveTo : float -> float -> float -> float -> float -> float -> unit
  method arcTo : float -> float -> float -> float -> float -> unit
  method rect : float -> float -> float -> float -> unit
  method arc : float -> float -> float -> float -> float -> bool -> unit
  method fill : unit
  method stroke : unit
  method clip : unit
  method isPointInPath : float -> float -> bool

  method _get_font : string
  method _set_font : string -> unit
  method _get_textAlign : string
  method _set_textAlign : string -> unit
  method _get_textBaseline : string
  method _set_textBaseline : string -> unit
  method fillText : string -> float -> float -> unit
  method fillText_maxWidth_ : string -> float -> float -> float -> unit
  method strokeText : string -> float -> float -> unit
  method strokeText_maxWidth_ : string -> float -> float -> float -> unit
  method measureText : string -> textMetrics

  method drawImage : image -> float -> float -> unit
  method drawImage_5_ : image -> float -> float -> float -> float -> unit
  method drawImage_9_ : image -> float -> float -> float -> float -> float -> float -> float -> float -> unit
  method drawImage_canvas_ : canvas -> float -> float -> unit
  method drawImage_canvas_5_ : canvas -> float -> float -> float -> float -> unit
  method drawImage_canvas_9_ : canvas -> float -> float -> float -> float -> float -> float -> float -> float -> unit

  method createImageData : float -> float -> imageData
  method getImageData : float -> float -> float -> float -> imageData
  method putImageData : imageData -> float -> float -> unit
  method putImageData_7_ : imageData -> float -> float -> float -> float -> float -> float -> unit
end

and canvasGradient =
object
  method addColorStop : float -> string -> unit
end

and canvasPattern =
object
end

and textMetrics =
object
  method _get_width : float
end

and imageData =
object
  method _get_width : int
  method _get_height : int
  method _get_data : canvasPixelArray
end

and canvasPixelArray =
object
  method _get_length : int
  (* XXX I think these are just draft names *)
  (* method XXX5 : int -> int *)
  (* method XXX6 : int -> int -> void *)
end

class type xMLHttpRequest =
object
  method _set_onreadystatechange : (unit -> unit) Ocamljs.jsfun -> unit
  method _get_readyState : int
  (* method _get_responseXML : Dom.document ? *)
  method _get_responseText : string
  method _get_status : int
  method _get_statusText : string
  method abort : unit
  method getAllResponseHeaders : string
  method getResponseHeader : string -> string
  method open__ : string -> string -> bool -> unit
  method send : string -> unit
  method setRequestHeader : string -> string -> unit
end

external new_XMLHttpRequest : unit -> xMLHttpRequest = "$new" "XMLHttpRequest"

val window : window
val document : document
