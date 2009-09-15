(*
 * Copyright (C) 2009 Dave Benjamin
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

(* jQuery interface for ocamljs *)
(* ============================ *)

class type jQuery =
object

  (* jQuery Core *)
  (* ----------- *)

  (* jQuery Object Accessors *)
  method each : (int -> Dom.element -> bool) -> unit
  method size : int
  method _get_length : int
  method _get_selector : string
  method _get_context : Dom.element
  method get : Dom.element array
  method get_index_ : int -> Dom.element
  method index : Dom.element -> int
  method index_jQuery_ : jQuery -> int

  (* Data *)
  method data : string -> 'a
  method data_store_ : string -> 'a -> unit
  method removeData : string -> unit
  method queue : string -> (unit -> unit) array
  method queue_enqueue_ : string -> (unit -> unit) -> unit
  method queue_replace_ : string -> (unit -> unit) array -> unit
  method dequeue : string -> unit

  (* Attributes *)
  (* ---------- *)

  (* Attr *)
  method attr : string -> 'a
  method attr_obj_ : 'a -> unit
  method attr_set_ : string -> 'a -> unit
  method removeAttr : string -> unit

  (* Class *)
  method addClass : string -> unit
  method hasClass : string -> bool
  method removeClass : string -> unit
  method toggleClass : string -> unit
  method toggleClass_switch_ : string -> bool -> unit

  (* HTML *)
  method html : string
  method html_set_ : string -> unit

  (* Text *)
  method text : string
  method text_set_ : string -> unit

  (* Value *)
  method val_ : string
  method val_set_ : string -> unit
  method val_array_ : string array
  method val_setarray_ : string array -> unit

  (* Traversing *)
  (* ---------- *)

  (* Filtering *)
  method eq : int -> jQuery
  method filter : string -> jQuery
  method filter_fn_ : (int -> bool) -> jQuery
  method is : string -> bool
  method map : (int -> Dom.element -> 'a) -> jQuery
  method not : string -> jQuery
  method not_element_ : Dom.element -> jQuery
  method not_elements_ : Dom.element array -> jQuery
  method slice : int -> int -> jQuery
  method slice_from_ : int -> jQuery

  (* Finding *)
  method add : string -> jQuery
  method add_element_ : Dom.element -> jQuery
  method add_elements_ : Dom.element array -> jQuery
  method children : jQuery
  method children_expr_ : string -> jQuery
  method closest : jQuery
  method closest_expr_ : string -> jQuery
  method contents : jQuery
  method find : string -> jQuery
  method next : jQuery
  method next_expr_ : string -> jQuery
  method nextAll : jQuery
  method nextAll_expr_ : string -> jQuery
  method offsetParent : jQuery
  method parent : jQuery
  method parent_expr_ : string -> jQuery
  method parents : jQuery
  method parents_expr_ : string -> jQuery
  method prev : jQuery
  method prev_expr_ : string -> jQuery
  method prevAll : jQuery
  method prevAll_expr_ : string -> jQuery
  method siblings : jQuery
  method siblings_expr_ : string -> jQuery

  (* Chaining *)
  method andSelf : jQuery
  method end_ : jQuery

  (* Manipulation *)
  (* ------------ *)

  (* Inserting Inside *)
  method append : string -> unit
  method append_element_ : Dom.element -> unit
  method append_jQuery_ : jQuery -> unit
  method appendTo : string -> unit
  method prepend : string -> unit
  method prepend_element_ : Dom.element -> unit
  method prepend_jQuery_ : jQuery -> unit
  method prependTo : string -> unit

  (* Inserting Outside *)
  method after : string -> unit
  method after_element_ : Dom.element -> unit
  method after_jQuery_ : jQuery -> unit
  method insertAfter : string -> unit
  method before : string -> unit
  method before_element_ : Dom.element -> unit
  method before_jQuery_ : jQuery -> unit
  method insertBefore : string -> unit

  (* Inserting Around *)
  method wrap : string -> unit
  method wrap_element_ : Dom.element -> unit
  method wrapAll : string -> unit
  method wrapAll_element_ : Dom.element -> unit
  method wrapInner : string -> unit
  method wrapInner_element_ : Dom.element -> unit

  (* Replacing *)
  method replaceWith : string -> unit
  method replaceWith_element_ : Dom.element -> unit
  method replaceWith_jQuery_ : jQuery -> unit
  method replaceAll : string -> unit

  (* Removing *)
  method empty : unit
  method remove : unit
  method remove_expr_ : string -> unit

  (* Copying *)
  method clone : bool -> jQuery

  (* CSS *)
  (* --- *)

  (* CSS *)
  method css : string -> string
  method css_obj_ : 'a -> unit
  method css_set_ : string -> string -> unit

  (* Positioning *)
  method offset : < _get_top : int; _get_left : int >
  method position : < _get_top : int; _get_left : int >
  method scrollTop : int
  method scrollTop_set_ : int -> unit
  method scrollLeft : int
  method scrollLeft_set_ : int -> unit

  (* Height and Width *)
  method height : int
  method height_set_ : int -> unit
  method width : int
  method width_set_ : int -> unit
  method innerHeight : int
  method innerWidth : int
  method outerHeight : bool -> int
  method outerWidth : bool -> int

  (* Events *)
  (* ------ *)

  (* Page Load *)
  method ready : ((string -> jQuery) -> unit) -> unit

  (* Event Handling *)
  method bind : string -> (Dom.event -> bool) -> unit
  method bind_data_ : string -> 'a -> (Dom.event -> bool) -> unit
  method one : string -> (Dom.event -> bool) -> unit
  method one_data_ : string -> 'a -> (Dom.event -> bool) -> unit
  method trigger : string -> unit
  method trigger_data_ : string -> 'a array -> unit
  method trigger_event_ : Dom.event -> unit
  method trigger_eventdata_ : Dom.event -> 'a array -> unit
  method trigger_obj_ : 'a -> unit
  method trigger_objdata_ : 'a -> 'b array -> unit
  method triggerHandler : string -> unit
  method triggerHandler_data_ : string -> 'a array -> unit
  method triggerHandler_event_ : Dom.event -> unit
  method triggerHandler_eventdata_ : Dom.event -> 'a array -> unit
  method triggerHandler_obj_ : 'a -> unit
  method triggerHandler_objdata_ : 'a -> 'b array -> unit
  method unbind : string -> unit
  method unbind_fn_ : string -> (Dom.event -> bool) -> unit
  method unbind_event_ : Dom.event -> unit
  method unbind_eventfn_ :
    Dom.event -> (Dom.event -> bool) -> unit
  method unbind_obj_ : 'a -> unit
  method unbind_objfn_ : 'a -> (Dom.event -> bool) -> unit
  method unbind_all_ : unit

  (* Live Events *)
  method live : string -> (Dom.event -> bool) -> unit
  method die : string -> unit
  method die_fn_ : string -> (Dom.event -> bool) -> unit
  method die_all_ : unit

  (* Interaction Helpers *)
  method hover :
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) -> unit
  method toggle_2_ :
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) -> unit
  method toggle_3_ :
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) -> unit
  method toggle_4_ :
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) -> unit
  method toggle_5_ :
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) ->
    (Dom.mouseEvent -> unit) -> unit

  (* Event Helpers *)
  method blur : (Dom.event -> bool) -> unit
  method blur_trigger_ : unit
  method change : (Dom.event -> bool) -> unit
  method change_trigger_ : unit
  method click : (Dom.mouseEvent -> bool) -> unit
  method click_trigger_ : unit
  method dblclick : (Dom.mouseEvent -> bool) -> unit
  method dblclick_trigger_ : unit
  method error : (Dom.event -> bool) -> unit
  method error_trigger_ : unit
  method focus : (Dom.event -> bool) -> unit
  method focus_trigger_ : unit
  method keydown : (Dom.keyEvent -> bool) -> unit
  method keydown_trigger_ : unit
  method keypress : (Dom.keyEvent -> bool) -> unit
  method keypress_trigger_ : unit
  method keyup : (Dom.keyEvent -> bool) -> unit
  method keyup_trigger_ : unit
  method load_event_ : (Dom.event -> bool) -> unit
  method mousedown : (Dom.mouseEvent -> bool) -> unit
  method mouseenter : (Dom.mouseEvent -> bool) -> unit
  method mouseleave : (Dom.mouseEvent -> bool) -> unit
  method mousemove : (Dom.mouseEvent -> bool) -> unit
  method mouseout : (Dom.mouseEvent -> bool) -> unit
  method mouseover : (Dom.mouseEvent -> bool) -> unit
  method mouseup : (Dom.mouseEvent -> bool) -> unit
  method resize : (Dom.event -> bool) -> unit
  method scroll : (Dom.event -> bool) -> unit
  method select : (Dom.event -> bool) -> unit
  method select_trigger_ : unit
  method submit : (Dom.event -> bool) -> unit
  method submit_trigger_ : unit
  method unload : (Dom.event -> bool) -> unit

  (* Effects *)
  (* ------- *)

  (* Basics *)
  method show : unit
  method show_ms_ : int -> (unit -> unit) -> unit
  method show_speed_ : string -> (unit -> unit) -> unit
  method hide : unit
  method hide_ms_ : int -> (unit -> unit) -> unit
  method hide_speed_ : string -> (unit -> unit) -> unit
  method toggle : unit
  method toggle_switch_ : bool -> unit
  method toggle_ms_ : int -> (unit -> unit) -> unit
  method toggle_speed_ : string -> (unit -> unit) -> unit

  (* Sliding *)
  method slideDown : unit
  method slideDown_ms_ : int -> (unit -> unit) -> unit
  method slideDown_speed_ : string -> (unit -> unit) -> unit
  method slideUp : unit
  method slideUp_ms_ : int -> (unit -> unit) -> unit
  method slideUp_speed_ : string -> (unit -> unit) -> unit
  method slideToggle : unit
  method slideToggle_ms_ : int -> (unit -> unit) -> unit
  method slideToggle_speed_ : string -> (unit -> unit) -> unit

  (* Fading *)
  method fadeIn : unit
  method fadeIn_ms_ : int -> (unit -> unit) -> unit
  method fadeIn_speed_ : string -> (unit -> unit) -> unit
  method fadeOut : unit
  method fadeOut_ms_ : int -> (unit -> unit) -> unit
  method fadeOut_speed_ : string -> (unit -> unit) -> unit
  method fadeTo : int -> float -> unit
  method fadeTo_ms_ : int -> float -> (unit -> unit) -> unit
  method fadeTo_speed_ : string -> float -> (unit -> unit) -> unit

  (* Custom *)
  method animate : 'a -> unit
  method animate_ms_ :
    'a -> int -> string -> (unit -> unit) -> unit
  method animate_speed_ :
    'a -> string -> string -> (unit -> unit) -> unit
  method animate_custom_ : 'a -> 'b -> unit
  method stop : bool -> bool -> unit

  (* Ajax *)
  (* ---- *)

  (* Ajax Requests *)
  method load : string -> unit
  method load_fn_ :
    string -> 'a ->
    (string -> string -> Dom.xMLHttpRequest -> unit) -> unit

  (* Ajax Events *)
  method ajaxComplete :
    (Dom.event -> Dom.xMLHttpRequest -> 'a -> unit) -> unit
  method ajaxError :
    (Dom.event -> Dom.xMLHttpRequest -> 'a -> 'b -> unit) -> unit
  method ajaxSend :
    (Dom.event -> Dom.xMLHttpRequest -> 'a -> unit) -> unit
  method ajaxStart : (Dom.event -> unit) -> unit
  method ajaxStop : (Dom.event -> unit) -> unit
  method ajaxSuccess :
    (Dom.event -> Dom.xMLHttpRequest -> 'a -> unit) -> unit

  (* Misc *)
  method serialize : string
  method serializeArray : 'a array
end

(* The jQuery Function *)
let jQuery = (Ocamljs.var "jQuery" : string -> jQuery)
let jQuery_context = (Ocamljs.var "jQuery" : string -> Dom.element -> jQuery)
let jQuery_jQuery = (Ocamljs.var "jQuery" : string -> jQuery -> jQuery)
let jQuery_element = (Ocamljs.var "jQuery" : Dom.element -> jQuery)
let jQuery_elements = (Ocamljs.var "jQuery" : Dom.element array -> jQuery)

(* Shorthand for $(document).ready() *)
let jQuery_ready func =
  (jQuery_element (Dom.document :> Dom.element))
    #ready func

(* Invoking Plugins *)
let jQuery_plugin jQuery (name : string) args =
  (Ocamljs.hashref jQuery name : < apply : jQuery -> 'a array -> unit >)
    #apply jQuery args

(* Plugins *)
(* ------- *)

class type jQuery_fn =
object
  method extend : 'a -> unit
end

let jQuery_fn = (Ocamljs.var "jQuery.fn" : jQuery_fn)

(* Effects *)
(* ------- *)

class type jQuery_fx =
object
  method _get_off : bool
  method _set_off : bool -> unit
end

let jQuery_fx = (Ocamljs.var "jQuery.fx" : jQuery_fx)

(* Utilities *)
(* --------- *)

class type jQuery_util =
object
  (* Plugins *)
  method extend : 'a -> unit

  (* Interoperability *)
  method noConflict : unit
  method noConflict_extreme_ : bool -> unit

  (* Ajax Requests *)
  method ajax : 'a -> Dom.xMLHttpRequest
  method get :
    string -> 'a -> ('b -> string -> unit) -> Dom.xMLHttpRequest
  method get_typed_ :
    string -> 'a -> ('b -> string -> unit) -> string ->
    Dom.xMLHttpRequest
  method getJSON :
    string -> 'a -> ('b -> string -> unit) -> Dom.xMLHttpRequest
  method getScript :
    string -> ('b -> string -> unit) -> Dom.xMLHttpRequest
  method post :
    string -> 'a -> ('b -> string -> unit) -> Dom.xMLHttpRequest
  method post_typed_ :
    string -> 'a -> ('b -> string -> unit) -> string ->
    Dom.xMLHttpRequest

  (* Misc Ajax *)
  method ajaxSetup : 'a -> unit

  (* Browser and Feature Detection *)
  method _get_support : 'a
  method _get_browser : 'a
  method _get_boxModel : bool

  (* Array and Object operations *)
  method each : 'a -> (int -> 'b -> bool) -> unit
  method extend_obj_ : bool -> 'a -> 'b -> unit
  method grep : 'a array -> (int -> 'a -> bool) -> 'a array
  method grep_invert_ :
    'a array -> (int -> 'a -> bool) -> bool -> 'a array
  method makeArray : 'a -> 'b array
  method map : 'a array -> ('a -> 'b) -> 'b array
  method map_index_ : 'a array -> ('a -> int -> 'b) -> 'b array
  method inArray : 'a -> 'a array -> int
  method merge : 'a array -> 'a array -> 'a array
  method unique : 'a array -> 'a array

  (* Test operations *)
  method isArray : 'a -> bool
  method isFunction : 'a -> bool

  (* String operations *)
  method trim : string -> string

  (* URLs *)
  method param : Dom.element array -> string
  method param_jQuery_ : jQuery -> string
  method param_obj_ : 'a -> string
end

let jQuery_util = (Ocamljs.var "jQuery" : jQuery_util)

(* Invoking Plugins *)
let jQuery_plugin_static (name : string) =
  Ocamljs.hashref jQuery_util name
