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

module Gram :
  sig
    module Loc :
      sig
        type t = Jslib_lexer.Loc.t
        val mk : string -> t
        val ghost : t
        val of_lexing_position : Lexing.position -> t
        val to_ocaml_location : t -> Camlp4_import.Location.t
        val of_ocaml_location : Camlp4_import.Location.t -> t
        val of_lexbuf : Lexing.lexbuf -> t
        val of_tuple : string * int * int * int * int * int * int * bool -> t
        val to_tuple : t -> string * int * int * int * int * int * int * bool
        val merge : t -> t -> t
        val join : t -> t
        val move : [ `both | `start | `stop ] -> int -> t -> t
        val shift : int -> t -> t
        val move_line : int -> t -> t
        val file_name : t -> string
        val start_line : t -> int
        val stop_line : t -> int
        val start_bol : t -> int
        val stop_bol : t -> int
        val start_off : t -> int
        val stop_off : t -> int
        val start_pos : t -> Lexing.position
        val stop_pos : t -> Lexing.position
        val is_ghost : t -> bool
        val ghostify : t -> t
        val set_file_name : string -> t -> t
        val strictly_before : t -> t -> bool
        val make_absolute : t -> t
        val print : Format.formatter -> t -> unit
        val dump : Format.formatter -> t -> unit
        val to_string : t -> string
        exception Exc_located of t * exn
        val raise : t -> exn -> 'a
        val name : string ref
      end
    module Action :
      sig
        type t = Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).Action.t
        val mk : 'a -> t
        val get : t -> 'a
        val getf : t -> 'a -> 'b
        val getf2 : t -> 'a -> 'b -> 'c
      end
    module Token :
      sig
        module Loc :
          sig
            type t = Jslib_lexer.Loc.t
            val mk : string -> t
            val ghost : t
            val of_lexing_position : Lexing.position -> t
            val to_ocaml_location : t -> Camlp4_import.Location.t
            val of_ocaml_location : Camlp4_import.Location.t -> t
            val of_lexbuf : Lexing.lexbuf -> t
            val of_tuple :
              string * int * int * int * int * int * int * bool -> t
            val to_tuple :
              t -> string * int * int * int * int * int * int * bool
            val merge : t -> t -> t
            val join : t -> t
            val move : [ `both | `start | `stop ] -> int -> t -> t
            val shift : int -> t -> t
            val move_line : int -> t -> t
            val file_name : t -> string
            val start_line : t -> int
            val stop_line : t -> int
            val start_bol : t -> int
            val stop_bol : t -> int
            val start_off : t -> int
            val stop_off : t -> int
            val start_pos : t -> Lexing.position
            val stop_pos : t -> Lexing.position
            val is_ghost : t -> bool
            val ghostify : t -> t
            val set_file_name : string -> t -> t
            val strictly_before : t -> t -> bool
            val make_absolute : t -> t
            val print : Format.formatter -> t -> unit
            val dump : Format.formatter -> t -> unit
            val to_string : t -> string
            exception Exc_located of t * exn
            val raise : t -> exn -> 'a
            val name : string ref
          end
        type t = Jslib_lexer.Token.t
        val to_string : t -> string
        val print : Format.formatter -> t -> unit
        val match_keyword : string -> t -> bool
        val extract_string : t -> string
        module Filter :
          sig
            type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter
            type t = Jslib_lexer.Token.Filter.t
            val mk : (string -> bool) -> t
            val define_filter : t -> (token_filter -> token_filter) -> unit
            val filter : t -> token_filter
            val keyword_added : t -> string -> bool -> unit
            val keyword_removed : t -> string -> unit
          end
        module Error :
          sig
            type t = Jslib_lexer.Token.Error.t
            exception E of t
            val to_string : t -> string
            val print : Format.formatter -> t -> unit
          end
      end
    type gram = Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).gram
    type internal_entry =
        Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).internal_entry
    type tree = Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).tree
    type token_pattern = (Token.t -> bool) * string
    type symbol =
      Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).symbol =
        Smeta of string * symbol list * Action.t
      | Snterm of internal_entry
      | Snterml of internal_entry * string
      | Slist0 of symbol
      | Slist0sep of symbol * symbol
      | Slist1 of symbol
      | Slist1sep of symbol * symbol
      | Sopt of symbol
      | Sself
      | Snext
      | Stoken of token_pattern
      | Skeyword of string
      | Stree of tree
    type production_rule = symbol list * Action.t
    type single_extend_statment =
        string option * Camlp4.Sig.Grammar.assoc option *
        production_rule list
    type extend_statment =
        Camlp4.Sig.Grammar.position option * single_extend_statment list
    type delete_statment = symbol list
    type ('a, 'b, 'c) fold =
        internal_entry ->
        symbol list -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'c
    type ('a, 'b, 'c) foldsep =
        internal_entry ->
        symbol list ->
        ('a Stream.t -> 'b) -> ('a Stream.t -> unit) -> 'a Stream.t -> 'c
    module Entry :
      sig
        type 'a t = 'a Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).Entry.t
        val mk : string -> 'a t
        val of_parser : string -> ((Token.t * Loc.t) Stream.t -> 'a) -> 'a t
        val setup_parser : 'a t -> ((Token.t * Loc.t) Stream.t -> 'a) -> unit
        val name : 'a t -> string
        val print : Format.formatter -> 'a t -> unit
        val dump : Format.formatter -> 'a t -> unit
        val obj : 'a t -> internal_entry
        val clear : 'a t -> unit
      end
    val get_filter : unit -> Token.Filter.t
    type 'a not_filtered =
        'a Camlp4.Struct.Grammar.Static.Make(Jslib_lexer).not_filtered
    val extend : 'a Entry.t -> extend_statment -> unit
    val delete_rule : 'a Entry.t -> delete_statment -> unit
    val srules : 'a Entry.t -> (symbol list * Action.t) list -> symbol
    val sfold0 : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) fold
    val sfold1 : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) fold
    val sfold0sep : ('a -> 'b -> 'b) -> 'b -> ('c, 'a, 'b) foldsep
    val lex :
      Loc.t -> char Stream.t -> (Token.t * Loc.t) Stream.t not_filtered
    val lex_string :
      Loc.t -> string -> (Token.t * Loc.t) Stream.t not_filtered
    val filter :
      (Token.t * Loc.t) Stream.t not_filtered -> (Token.t * Loc.t) Stream.t
    val parse : 'a Entry.t -> Loc.t -> char Stream.t -> 'a
    val parse_string : 'a Entry.t -> Loc.t -> string -> 'a
    val parse_tokens_before_filter :
      'a Entry.t -> (Token.t * Loc.t) Stream.t not_filtered -> 'a
    val parse_tokens_after_filter :
      'a Entry.t -> (Token.t * Loc.t) Stream.t -> 'a
  end
val statementList : Jslib_ast.stmt Gram.Entry.t
val expression : Jslib_ast.exp Gram.Entry.t
val parse_file : string -> Jslib_ast.stmt
val parse_stdin : unit -> Jslib_ast.stmt
val parse_string : string -> Jslib_ast.stmt
