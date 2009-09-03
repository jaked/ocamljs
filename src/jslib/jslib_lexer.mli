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

(* adapted from CDuce parser/ulexer.mli *)

open Camlp4.Sig

type token =
  | KEYWORD of string
  | IDENT of string
  | INT of string
  | FLOAT of string
  | STRING1 of string
  | STRING2 of string
  | REGEXP of string
  | ANTIQUOT of string * string
  | EOI

module Loc   : Loc with type t = Camlp4.PreCast.Loc.t
module Token : Token with module Loc = Loc and type t = token
module Error : Error

val mk : unit -> (Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t)
