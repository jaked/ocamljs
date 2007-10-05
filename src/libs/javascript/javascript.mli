(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 *
 * This library is free software released under the LGPL.
 * See LICENSE for more details.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *)

external encodeURIComponent : string -> string = "@encodeURIComponent"
external decodeURIComponent : string -> string = "@decodeURIComponent"
external dump : string -> unit = "@dump"
external eval : string -> 'a = "@eval"

val true_ : bool
val false_ : bool

module Date :
  sig

    type t

    val new_ : unit -> t
    external new_milliseconds : int -> t = "$new" "Date"
    external new_parts : int -> int -> int -> int -> int -> int -> int -> t = "$new" "Date"

    external getTime : t -> int = "#getTime"
    external getDate : t -> int = "#getDate"
    external getMonth : t -> int = "#getMonth"
    external getHours : t -> int = "#getHours"
    external getMinutes : t -> int = "#getMinutes"
    external getFullYear : t -> int = "#getFullYear"

    external setDate : t -> int -> unit = "#setDate"
    external setMonth : t -> int -> unit = "#setMonth"
    external setHours : t -> int -> unit = "#setHours"
    external setMinutes : t -> int -> unit = "#setMinutes"

    external toString : t -> string = "#toString"

  end

module RegExp :
  sig

    type t

    external new_ : string -> t = "$new" "RegExp"
    external new_attributes : string -> string -> t = "$new" "RegExp"

    val exec : t -> string -> string array option
    val test : t -> string -> bool

  end

module String :
  sig

    type t = string

    val match_ : t -> RegExp.t -> string array option
    external split : t -> string -> string array = "#split"
    external indexOf : t -> string -> int = "#indexOf"
    
  end
