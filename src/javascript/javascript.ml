(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
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

open Ocamljs.Inline

external encodeURIComponent : string -> string = "@encodeURIComponent"
external decodeURIComponent : string -> string = "@decodeURIComponent"
external dump : string -> unit = "@dump"
external eval : string -> 'a = "@eval"

let typeof o = << typeof $o$ >>

let true_ = << true >>
let false_ = << false >>

class type ['a] js_array =
object
  method _get_length : int
  method _set_length : int -> unit
  (* method concat : ? *)
  method join : string -> string
  method pop : 'a
  method push : 'a -> int
  method reverse : unit
  method shift : 'a
  method slice : int -> int -> 'a js_array
  method sort : 'a js_array
  method sort_compare_ : ('a -> 'a -> int) Ocamljs.jsfun -> 'a js_array
  (* method splice : ? *)
  method toLocaleString : string
  method toString : string
  method unshift : 'a -> int
end

external new_Array : unit -> 'a js_array = "$new" "Array"
external new_Array_length : int -> 'a js_array = "$new" "Array"

class type date =
object
  method getTime : float
  method getDate : int
  method getMonth : int
  method getHours : int
  method getMinutes : int
  method getFullYear : int

  method setDate : int -> unit
  method setMonth : int -> unit
  method setHours : int -> unit
  method setMinutes : int -> unit

  method toString : string
  method toDateString : string
  method toLocaleString : string
end

external new0 : string -> 'a = "$new"

let new_Date () = new0 "Date"
external new_Date_milliseconds : float -> date = "$new" "Date"
external new_Date_parts : int -> int -> int -> int -> int -> int -> int -> date = "$new" "Date"

class type regexp =
object
  method exec : string -> string array
  method test : string -> bool
end

external new_RegExp : string -> regexp = "$new" "RegExp"
external new_RegExp_attributes : string -> string -> regexp = "$new" "RegExp"

class type js_string =
object
  method match_ : regexp -> string array
  method split : string -> string array
  method indexOf : string -> int
  method replace : regexp -> string -> string
end

external js_string_of_string : string -> js_string = "%id"

module Js_string =
struct
  external fromCharCode : int -> string = "@String.fromCharCode"
end
