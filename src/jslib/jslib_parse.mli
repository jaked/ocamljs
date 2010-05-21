(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

module Gram : Camlp4.Sig.Grammar.Static
  with module Loc = Jslib_lexer.Loc
  and module Token = Jslib_lexer.Token

val statementList : Jslib_ast.stmt Gram.Entry.t
val expression : Jslib_ast.exp Gram.Entry.t
val parse_file : string -> Jslib_ast.stmt
val parse_stdin : unit -> Jslib_ast.stmt
val parse_string : string -> Jslib_ast.stmt

val non_exp_antiquots : bool ref
