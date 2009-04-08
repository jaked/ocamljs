(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
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

let init = ignore
let full_init = ignore
let self_init = ignore
let bits () = << Math.floor (Math.random() * 1073741824) >>
let int b = << Math.floor (Math.random() * $b$) >>
let int32 b = << Math.floor (Math.random() * $b$) >>
let nativeint b = << Math.floor (Math.random() * $b$) >>
let int64 _ = Int64.zero
let float b = << Math.random() * $b$ >>
let bool _ = << Math.random() < 0.5 >>

module State =
struct
  type t = unit

  let make = ignore
  let make_self_init = ignore
  let copy = ignore

  let bits _ = bits ()
  let int _ b = int b
  let int32 _ b = int32 b
  let nativeint _ b = nativeint b
  let int64 _ b = int64 b
  let float _ b = float b
  let bool _ = bool ()
end

let get_state = ignore
let set_state = ignore
