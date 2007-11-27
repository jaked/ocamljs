(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Javascript AST; only the parts we target *)

type ident = string

type exp =
      Jthis
    | Jvar of ident
    | Jarray of exp list
    | Jobject of (ident * exp) list
    | Jstring of string
    | Jnum of float
    | Jnull
    | Jbool of bool
    | Jfun of ident list * stmt list
    | Jfieldref of exp * ident
    | Jhashref of exp * exp
    | Jtypeof of exp
    | Jminus of exp
    | Jnot of exp
    | Jmul of exp * exp
    | Jdiv of exp * exp
    | Jmod of exp * exp
    | Jadd of exp * exp
    | Jsub of exp * exp
    | Jlt of exp * exp
    | Jgt of exp * exp
    | Jleq of exp * exp
    | Jgeq of exp * exp
    | Jlsr of exp * exp
    | Jlsl of exp * exp
    | Jasr of exp * exp
    | Jeq of exp * exp
    | Jneq of exp * exp
    | Jseq of exp * exp
    | Jsneq of exp * exp
    | Jland of exp * exp
    | Jlor of exp * exp
    | Jand of exp * exp
    | Jxor of exp * exp
    | Jor of exp * exp
    | Jite of exp * exp * exp
    | Jcomma of exp * exp
    | Jcall of exp * exp list
    | Jnew of ident * exp list
    | Jassign of exp * exp
    | Jplus2 of exp
    | Jminus2 of exp

and stmt =
    | Jempty
    | Jvars of ident * exp
    | Jfuns of ident * ident list * stmt list
    | Jbreak
    | Jreturn of exp
    | Jswitch of exp * (exp * stmt list) list * stmt list option
    | Jites of exp * stmt list * stmt list
    | Jthrow of exp
    | Jexps of exp
    | Jtrycatch of stmt list * ident * stmt list
    | Jfor of stmt * exp * stmt * stmt list
    | Jwhile of exp * stmt list

let jnum_of_int i = Jnum (float_of_int i)
let jcall f es = Jcall (Jvar f, es)
let jmcall e f es = Jcall (Jfieldref (e, f), es)
