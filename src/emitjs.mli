(* Generation of Javascript for .cmj files *)

open Js
open Cmo_format

val to_file: out_channel -> string -> (stmt * (reloc_info * int) list) -> unit
val to_packed_file: out_channel -> stmt -> (reloc_info * int) list
