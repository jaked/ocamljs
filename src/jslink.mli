(* Link .cmj files and produce a Javascript executable. *)

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
