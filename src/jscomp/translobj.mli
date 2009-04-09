(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Original file (bytecomp/translobj.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

open Lambda

val oo_prim: string -> lambda

val share: structured_constant -> lambda
val meth: lambda -> string -> lambda * lambda list

val reset_labels: unit -> unit
val transl_label_init: lambda -> lambda
val transl_store_label_init:
    Ident.t -> int -> ('a -> lambda) -> 'a -> int * lambda

IFNDEF OCAML_3_10_2 THEN
val method_ids: IdentSet.t ref (* reset when starting a new wrapper *)
ENDIF

val oo_wrap: Env.t -> bool -> ('a -> lambda) -> 'a -> lambda
val oo_add_class: Ident.t -> Env.t * bool
