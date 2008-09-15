(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (driver/errors.mli in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Error report *)
open Format

val report_error: formatter -> exn -> unit
