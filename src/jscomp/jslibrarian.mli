(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
 * Original file (bytecomp/bytelibrarian.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Build libraries of .cmo files *)

(* Format of a library file:
      magic number (Config.cma_magic_number)
      absolute offset of content table
      blocks of relocatable bytecode
      content table = list of compilation units
*)

val create_archive: string list -> string -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

open Format

val report_error: formatter -> error -> unit
