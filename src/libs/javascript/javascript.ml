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

let true_ = Ocamljs.var "true"
let false_ = Ocamljs.var "false"

module Date =
  struct

    type t

    external _new_noargs : string -> t = "$new"

    let new_ () = _new_noargs "Date"
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

module RegExp =
  struct

    type t

    external new_ : string -> t = "$new" "RegExp"
    external new_attributes : string -> string -> t = "$new" "RegExp"

    external _exec : t -> string -> string array = "#exec"
    let exec r s = Ocamljs.option_of_nullable (_exec r s)
    external test : t -> string -> bool = "#test"

  end

module String =
  struct

    type t = string

    external _match : t -> RegExp.t -> string array = "#match"
    let match_ s r = Ocamljs.option_of_nullable (_match s r)
    external split : t -> string -> string array = "#split"
    external indexOf : t -> string -> int = "#indexOf"

  end
