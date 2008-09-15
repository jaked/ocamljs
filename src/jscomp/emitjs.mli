(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (bytecomp/emitcode.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Generation of Javascript for .cmjs files *)

open Cmo_format

val to_file: out_channel -> string -> (Jslib_ast.stmt * (reloc_info * int) list) -> unit
val to_packed_file: out_channel -> Jslib_ast.stmt -> (reloc_info * int) list
