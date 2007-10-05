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

external assign : 'a -> 'a -> unit = "$assign"
(* external call : 'func -> 'arg1 -> ... -> 'return= "$call" *)
external false_ : unit -> bool = "$false"
external fieldref : 'a -> string -> 'b = "$fieldref"
external function_ : 'a -> 'b = "$function" (* XXX better type? *)
external hashref : 'a -> 'b -> 'c = "$hashref"
(* external new_ : = 'arg1 -> ... -> 'class "$new" "class" *)
external null : unit -> 'a = "$null"
external obj : (string * 'a) list -> 'b = "$obj"
external this : unit -> 'a = "$this"
external throw : 'a -> 'b = "$throw"
external true_ : unit -> bool = "$true"
external var : string -> 'a = "$var"

external caml_callback : ('a -> 'b) -> 'a -> 'b = "caml_callback"
external caml_callback2 : ('a1 -> 'a2 -> 'b) -> 'a1 -> 'a2 -> 'b = "caml_callback2"
external caml_callback3 : ('a1 -> 'a2 -> 'a3 -> 'b) -> 'a1 -> 'a2 -> 'a3 -> 'b = "caml_callback3"

(* XXX do these belong here? *)
let option_of_nullable x =
  if x == null()
  then None
  else Some x

let nullable_of_option x =
  match x with
    | None -> null()
    | Some x -> x
