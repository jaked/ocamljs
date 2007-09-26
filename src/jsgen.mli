(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: bytegen.mli,v 1.6 1999/11/17 18:56:59 xleroy Exp $ *)

(* Generation of bytecode from lambda terms *)

open Lambda
open Js
open Cmo_format

val compile_implementation: string -> lambda -> (stmt * (reloc_info * int) list)
val jsident_of_ident : Ident.t -> string
