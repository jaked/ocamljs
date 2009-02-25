type interval_id
type timeout_id

class type node =
object
  method appendChild : node -> node
  method removeChild : node -> node
  method replaceChild : node -> node -> node
  method _get_parentNode : node
end

class type documentFragment =
object
  inherit node
end

class type abstractView =
object
end

class type eventTarget =
object
  method addEventListener : string -> (event -> unit) Ocamljs.jsfun -> bool -> unit
  method addEventListener_mouseEvent_ : string -> (mouseEvent -> unit) Ocamljs.jsfun -> bool -> unit
  method removeEventListener : string -> (event -> unit) Ocamljs.jsfun -> bool -> unit
  method removeEventListener_mouseEvent_ : string -> (mouseEvent -> unit) Ocamljs.jsfun -> bool -> unit
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
  method _get_background : string
  method _set_background : string -> unit
  method _get_background : string
  method _set_background : string -> unit
  method _get_backgroundColor : string
  method _set_backgroundColor : string -> unit
  method _get_backgroundImage : string
  method _set_backgroundImage : string -> unit
  method _get_backgroundPosition : string
  method _set_backgroundPosition : string -> unit
  method _get_backgroundRepeat : string
  method _set_backgroundRepeat : string -> unit
  method _get_border : string
  method _set_border : string -> unit
  method _get_borderBottom : string
  method _set_borderBottom : string -> unit
  method _get_borderLeft : string
  method _set_borderLeft : string -> unit
  method _get_borderRight : string
  method _set_borderRight : string -> unit
  method _get_borderTop : string
  method _set_borderTop : string -> unit
  method _get_borderBottomColor : string
  method _set_borderBottomColor : string -> unit
  method _get_borderLeftColor : string
  method _set_borderLeftColor : string -> unit
  method _get_borderRightColor : string
  method _set_borderRightColor : string -> unit
  method _get_borderTopColor : string
  method _set_borderTopColor : string -> unit
  method _get_borderBottomStyle : string
  method _set_borderBottomStyle : string -> unit
  method _get_borderLeftStyle : string
  method _set_borderLeftStyle : string -> unit
  method _get_borderRightStyle : string
  method _set_borderRightStyle : string -> unit
  method _get_borderTopStyle : string
  method _set_borderTopStyle : string -> unit
  method _get_borderBottomWidth : string
  method _set_borderBottomWidth : string -> unit
  method _get_borderLeftWidth : string
  method _set_borderLeftWidth : string -> unit
  method _get_borderRightWidth : string
  method _set_borderRightWidth : string -> unit
  method _get_borderTopWidth : string
  method _set_borderTopWidth : string -> unit
  method _get_borderCollapse : string
  method _set_borderCollapse : string -> unit
  method _get_borderColor : string
  method _set_borderColor : string -> unit
  method _get_borderSpacing : string
  method _set_borderSpacing : string -> unit
  method _get_borderStyle : string
  method _set_borderStyle : string -> unit
  method _get_borderWidth : string
  method _set_borderWidth : string -> unit
  method _get_bottom : string
  method _set_bottom : string -> unit
  method _get_captionSide : string
  method _set_captionSide : string -> unit
  method _get_clear : string
  method _set_clear : string -> unit
  method _get_clip : string
  method _set_clip : string -> unit
  method _get_color : string
  method _set_color : string -> unit
  method _get_content : string
  method _set_content : string -> unit
  method _get_cssFloat : string
  method _set_cssFloat : string -> unit
  method _get_cssText : string
  method _get_cursor : string
  method _set_cursor : string -> unit
  method _get_direction : string
  method _set_direction : string -> unit
  method _get_display : string
  method _set_display : string -> unit
  method _get_emptyCells : string
  method _set_emptyCells : string -> unit
  method _get_font : string
  method _set_font : string -> unit
  method _get_fontFamily : string
  method _set_fontFamily : string -> unit
  method _get_fontSize : string
  method _set_fontSize : string -> unit
  method _get_fontSizeAdjust : string
  method _set_fontSizeAdjust : string -> unit
  method _get_fontStretch : string
  method _set_fontStretch : string -> unit
  method _get_fontStyle : string
  method _set_fontStyle : string -> unit
  method _get_fontVariant : string
  method _set_fontVariant : string -> unit
  method _get_fontWeight : string
  method _set_fontWeight : string -> unit
  method _get_height : string
  method _set_height : string -> unit
  method _get_width : string
  method _set_width : string -> unit
  method _get_left : string
  method _set_left : string -> unit
  method _get_letterSpacing : string
  method _set_letterSpacing : string -> unit
  method _get_lineHeight : string
  method _set_lineHeight : string -> unit
  method _get_listStyle : string
  method _set_listStyle : string -> unit
  method _get_listStyleImage : string
  method _set_listStyleImage : string -> unit
  method _get_listStylePosition : string
  method _set_listStylePosition : string -> unit
  method _get_listStyleType : string
  method _set_listStyleType : string -> unit
  method _get_marginBottom : string
  method _set_marginBottom : string -> unit
  method _get_marginLeft : string
  method _set_marginLeft : string -> unit
  method _get_marginRight : string
  method _set_marginRight : string -> unit
  method _get_marginTop : string
  method _set_marginTop : string -> unit
  method _get_maxHeight : string
  method _set_maxHeight : string -> unit
  method _get_maxWidth : string
  method _set_maxWidth : string -> unit
  method _get_minHeight : string
  method _set_minHeight : string -> unit
  method _get_minWidth : string
  method _set_minWidth : string -> unit
  method _get_orphans : string
  method _set_orphans : string -> unit
  method _get_widows : string
  method _set_widows : string -> unit
  method _get_outline : string
  method _set_outline : string -> unit
  method _get_outlineColor : string
  method _set_outlineColor : string -> unit
  method _get_outlineOffset : string
  method _set_outlineOffset : string -> unit
  method _get_outlineStyle : string
  method _set_outlineStyle : string -> unit
  method _get_outlineWidth : string
  method _set_outlineWidth : string -> unit
  method _get_overflow : string
  method _set_overflow : string -> unit
  method _get_overflowX : string
  method _set_overflowX : string -> unit
  method _get_overflowY : string
  method _set_overflowY : string -> unit
  method _get_padding : string
  method _set_padding : string -> unit
  method _get_paddingBottom : string
  method _set_paddingBottom : string -> unit
  method _get_paddingLeft : string
  method _set_paddingLeft : string -> unit
  method _get_paddingRight : string
  method _set_paddingRight : string -> unit
  method _get_paddingTop : string
  method _set_paddingTop : string -> unit
  method _get_position : string
  method _set_position : string -> unit
  method _get_right : string
  method _set_right : string -> unit
  method _get_textAlign : string
  method _set_textAlign : string -> unit
  method _get_textDecoration : string
  method _set_textDecoration : string -> unit
  method _get_textIndent : string
  method _set_textIndent : string -> unit
  method _get_textTransform : string
  method _set_textTransform : string -> unit
  method _get_top : string
  method _set_top : string -> unit
  method _get_unicodeBidi : string
  method _set_unicodeBidi : string -> unit
  method _get_verticalAlign : string
  method _set_verticalAlign : string -> unit
  method _get_visibility : string
  method _set_visibility : string -> unit
  method _get_whitespace : string
  method _set_whitespace : string -> unit
  method _get_wordspacing : string
  method _set_wordspacing : string -> unit
  method _get_zIndex : string
  method _set_zIndex : string -> unit
  method _get_zIndex_ie_ : int
  method _set_zIndex_ie_ : int -> unit
end

class type element =
object
  inherit node
  inherit eventTarget

  method getAttribute : string -> string
  method setAttribute : string -> string -> unit

  method _get_style : style

  method _get_className : string
  method _set_className : string -> unit

  method _get_offsetWidth : int

  method _get_innerHTML : string
  method _set_innerHTML : string -> unit
end

class type anchor =
object
  inherit element

  method _get_accessKey : string
  method _set_accessKey : string -> unit
  method _get_charset : string
  method _set_charset : string -> unit
  method _get_coords : string
  method _set_coords : string -> unit
  method _get_hash : string
  method _set_hash : string -> unit
  method _get_host : string
  method _set_host : string -> unit
  method _get_hostname : string
  method _set_hostname : string -> unit
  method _get_href : string
  method _set_href : string -> unit
  method _get_hreflang : string
  method _set_hreflang : string -> unit
  method _get_name : string
  method _set_name : string -> unit
  method _get_pathname : string
  method _set_pathname : string -> unit
  method _get_port : string
  method _set_port : string -> unit
  method _get_protocol : string
  method _set_protocol : string -> unit
  method _get_rel : string
  method _set_rel : string -> unit
  method _get_rev : string
  method _set_rev : string -> unit
  method _get_search : string
  method _set_search : string -> unit
  method _get_shape : string
  method _set_shape : string -> unit
  method _get_tabIndex : int
  method _set_tabIndex : int -> unit
  method _get_target : string
  method _set_target : string -> unit
  method _get_type : string
  method _set_type : string -> unit

  method blur : unit
  method focus : unit

  method _set_onclick : (mouseEvent -> bool) Ocamljs.jsfun -> unit
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

class type location =
object
  method _get_hash : string
  method _set_hash : string -> unit
  method _get_host : string
  method _set_host : string -> unit
  method _get_hostname : string
  method _set_hostname : string -> unit
  method _get_href : string
  method _set_href : string -> unit
  method _get_pathname : string
  method _set_pathname : string -> unit
  method _get_protocol : string
  method _set_protocol : string -> unit
  method _get_search : string
  method _set_search : string -> unit

  method reload : bool -> unit
  method replace : string -> unit
end

class type window =
object
  method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit
  method _set_onbeforeunload : (unit -> string) Ocamljs.jsfun -> unit
  method _get_location : location

  method alert : string -> unit
  method confirm : string -> bool

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

class type cssRule =
object
  method _get_selectorText : string
  method _get_style : style
end

class type styleSheet =
object
  method _get_href : string
  method _get_cssRules : cssRule array
end

class type document =
object
  inherit element

  method createDocumentFragment : documentFragment
  method createElement : string -> #element
  method createTextNode : string -> text
  method getElementById : string -> #element
  method _get_body : body
  method _get_cookie : string
  method _set_cookie : string -> unit
  method _get_styleSheets : styleSheet array
end

class type form =
object
  inherit element

  method _get_elements : #element array

  method _set_onsubmit : (unit -> bool) Ocamljs.jsfun -> unit
end

class type button =
object
  inherit element

  method _get_form : form
  method _get_accessKey : string
  method _set_accessKey : string -> unit
  method _get_disabled : bool
  method _set_disabled : bool -> unit
  method _get_name : string
  method _set_name : string -> unit
  method _get_tabIndex : int
  method _set_tabIndex : int -> unit
  method _get_type : string
  method _get_value : string
  method _set_value : string -> unit

  method _set_onclick : (mouseEvent -> bool) Ocamljs.jsfun -> unit
end

class type input =
object
  inherit element

  method _get_defaultValue : string
  method _set_defaultValue : string -> bool
  method _get_defaultChecked : bool
  method _set_defaultChecked : bool -> unit
  method _get_form : form
  method _get_accept : string
  method _set_accept : string -> unit
  method _get_accessKey : string
  method _set_accessKey : string -> unit
  method _get_align : string
  method _set_align : string -> unit
  method _get_alt : string
  method _set_alt : string -> unit
  method _get_checked : bool
  method _set_checked : bool -> unit
  method _get_disabled : bool
  method _set_disabled : bool -> unit
  method _get_maxLength : int
  method _set_maxLength : int -> unit
  method _get_name : string
  method _set_name : string -> unit
  method _get_readOnly : bool
  method _set_readOnly : bool -> unit
  method _get_size : int
  method _set_size : int -> unit
  method _get_src : string
  method _set_src : string -> unit
  method _get_tabIndex : int
  method _set_tabIndex : int -> unit
  method _get_type : string
  method _get_useMap : string
  method _set_useMap : string -> unit
  method _get_value : string
  method _set_value : string -> unit

  method blur : unit
  method focus : unit
  method select : unit
  method click : unit

  method _set_onclick : (mouseEvent -> bool) Ocamljs.jsfun -> unit
  method _set_onchange : (unit -> unit) Ocamljs.jsfun -> unit
end

class type option =
object
  inherit element

  method _get_form : form
  method _get_defaultSelected : bool
  method _set_defaultSelected : bool -> unit
  method _get_text : string
  method _get_index : int
  method _get_disabled : bool
  method _set_disabled : bool -> unit
  method _get_label : string
  method _set_label : string -> unit
  method _get_selected : bool
  method _set_selected : bool -> unit
  method _get_value : string
  method _set_value : string -> unit
end

class type select =
object
  inherit element

  method _get_type : string
  method _get_selectedIndex : int
  method _set_selectedIndex : int -> unit
  method _get_value : string
  method _set_value : string -> unit
  method _get_length : int
  method _get_form : form
  method _get_options : option array
  method _get_disabled : bool
  method _set_disabled : bool -> unit
  method _get_multiple : bool
  method _set_multiple : bool -> unit
  method _get_name : string
  method _set_name : string -> unit
  method _get_size : int
  method _set_size : int -> unit
  method _get_tabIndex : int
  method _set_tabIndex : int -> unit

  method add : option -> option -> unit
  method add_ie_ : option -> int -> unit
  method remove : int -> unit
  method blur : unit
  method focus : unit
end

class type span =
object
  inherit element
end

class type image =
object
  inherit element

  method _get_src : string
  method _set_src : string -> unit

  method _set_onclick : (mouseEvent -> bool) Ocamljs.jsfun -> unit
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

  method save : unit
  method restore : unit

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
