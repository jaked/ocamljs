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
    external new_milliseconds : float -> t = "$new" "Date"
    external new_parts : int -> int -> int -> int -> int -> int -> int -> t = "$new" "Date"

    external getTime : t -> float = "#getTime"
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
    external toDateString : t -> string = "#toDateString"
    external toLocaleString : t -> string = "#toLocaleString"

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
    external replace : t -> RegExp.t -> t -> t = "#replace"

  end
