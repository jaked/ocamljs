(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (bytecomp/bytelink.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Link .cmjs files and produce a Javascript executable. *)

val link: string list -> string -> unit

val check_consistency: string -> Cmo_format.compilation_unit -> unit

val extract_crc_interfaces: unit -> (string * Digest.t) list

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | File_exists of string

exception Error of error

open Format

val report_error: formatter -> error -> unit
