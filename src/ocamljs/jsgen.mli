(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (bytecomp/bytegen.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Generation of bytecode from lambda terms *)

open Lambda
open Cmo_format

val compile_implementation: string -> lambda -> (Jslib_ast.stmt * (reloc_info * int) list)
val jsident_of_ident : Ident.t -> string
