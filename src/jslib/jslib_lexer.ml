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

(* adapted from CDuce parser/ulexer.ml and camlp4/Camlp4/Struct/Lexer.mll *)

open Camlp4.PreCast

module Loc = Loc

type token =
    | KEYWORD of string
    | IDENT of string
    | INT of string
    | FLOAT of string
    | HEX of string
    | STRING1 of string
    | STRING2 of string
    | REGEXP of string
    | ANTIQUOT of string * string
    | EOI

module Token = struct
  open Format
  module Loc = Loc
  type t = token
  type token = t

  let sf = Printf.sprintf

  let to_string =
    function
      | KEYWORD s    -> sf "KEYWORD %S" s
      | IDENT s      -> sf "IDENT %S" s
      | INT s        -> sf "INT %s" s
      | FLOAT s      -> sf "FLOAT %s" s
      | HEX s        -> sf "HEX %s" s
      | STRING1 s    -> sf "STRING \"%s\"" s
      | STRING2 s    -> sf "STRING \"%s\"" s
      | REGEXP s     -> sf "REGEXP \"%s\"" s
      | ANTIQUOT (n, s) -> sf "ANTIQUOT %s: %S" n s
      | EOI          -> sf "EOI"

  let print ppf x = pp_print_string ppf (to_string x)

  let match_keyword kwd =
    function
      | KEYWORD kwd' when kwd = kwd' -> true
      | _ -> false

  let extract_string =
    function
      | KEYWORD s | IDENT s | INT s | FLOAT s | HEX s | STRING1 s | STRING2 s | REGEXP s -> s
      | tok ->
          invalid_arg ("Cannot extract a string from this token: "^
                          to_string tok)

  module Error = struct
    type t = string
    exception E of string
    let print = pp_print_string
    let to_string x = x
  end
  let _ = let module M = Camlp4.ErrorHandler.Register(Error) in ()

  module Filter = struct
    type token_filter = (t, Loc.t) Camlp4.Sig.stream_filter

    type t =
        { is_kwd : string -> bool;
          mutable filter : token_filter }

    let keyword_conversion tok is_kwd =
      match tok with
        | IDENT s when is_kwd s -> KEYWORD s
        | _ -> tok

    let mk is_kwd =
      { is_kwd = is_kwd;
        filter = (fun s -> s) }

    let filter x =
      let f tok loc =
        let tok' = keyword_conversion tok x.is_kwd in
        (tok', loc)
      in
      let rec filter =
        parser
          | [< '(tok, loc); s >] -> [< ' f tok loc; filter s >]
          | [< >] -> [< >]
      in
      fun strm -> x.filter (filter strm)

    let define_filter x f = x.filter <- f x.filter

    let keyword_added _ _ _ = ()
    let keyword_removed _ _ = ()
  end

end

module Error = struct
  open Format
  type t = string
  exception E of string
  let print = pp_print_string
  let to_string x = x
end
let _ = let module M = Camlp4.ErrorHandler.Register(Error) in ()

module L = Ulexing

type context = {
  mutable loc : Loc.t;
  mutable start_loc : Loc.t option; (* if set, start lexeme here *)
  antiquots   : bool;
  lexbuf      : Ulexing.lexbuf;
  enc         : Ulexing.enc ref;
  buffer      : Buffer.t;
}

let dump_loc loc =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple loc in
  Format.eprintf "%s %d %d %d %d %d %d\n" fn bl bb bo el eb eo

(* XXX this is kind of gross *)
let current_loc c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bl, bb, bo =
    match c.start_loc with
      | Some loc ->
          let (_, bl, bb, bo, _, _, _, _) = Loc.to_tuple loc in
          bl, bb, bo
      | None -> bl, bb, Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g);
  c.start_loc <- None;
  c.loc

let set_start_loc c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bo = Ulexing.lexeme_start c.lexbuf in
  let eo = Ulexing.lexeme_end c.lexbuf in
  c.start_loc <- Some (Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g))

let next_line c =
  let (fn, bl, bb, bo, el, eb, eo, g) = Loc.to_tuple c.loc in
  let bl = bl + 1 in
  let el = el + 1 in
  let bb = Ulexing.lexeme_end c.lexbuf in
  let eb = bb in
  c.loc <- Loc.of_tuple (fn, bl, bb, bo, el, eb, eo, g)

let regexp lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let regexp uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let regexp identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9' ]
let regexp ident = (lowercase|uppercase) identchar*
let regexp locname = ident

let regexp newline = ('\010' | '\013' | "\013\010")
let regexp blank = [' ' '\009' '\012']

let error c s = Loc.raise (current_loc c) (Token.Error.E s)

(* Buffer for string literals *)

let store_lexeme c =
  Buffer.add_string c.buffer (Ulexing.utf8_lexeme c.lexbuf)
let store_ascii c = Buffer.add_char c.buffer
let store_code c = Utf8.store c.buffer
let get_stored_string c =
  let s = Buffer.contents c.buffer in
  Buffer.reset c.buffer;
  s

(* Parse characters literals \123; \x123; *)

let hexa_digit = function
  | '0'..'9' as c -> (Char.code c) - (Char.code '0')
  | 'a'..'f' as c -> (Char.code c) - (Char.code 'a') + 10
  | 'A'..'F' as c -> (Char.code c) - (Char.code 'A') + 10
  | _ -> -1

let parse_char cx base i =
  let s = L.latin1_sub_lexeme cx.lexbuf i (L.lexeme_length cx.lexbuf - i - 1) in
  let r = ref 0 in
  for i = 0 to String.length s - 1 do
    let c = hexa_digit s.[i] in
    if (c >= base) || (c < 0) then
      error cx "invalid digit";
    r := !r * base + c;
  done;
  !r


let regexp ncname_char =
  xml_letter | xml_digit | [ '$' '_' ] | xml_combining_char | xml_extender | "\\."
let regexp ncname = ( xml_letter ncname_char* ) | ('_' ncname_char+) | ('$' '$' ncname_char* )
let regexp qname = (ncname ':')? ncname


let illegal c = error c "Illegal character"


(*
  thanks to Stephen Weeks for this idea (from javascript.lex in
  svn://mlton.org/mltonlib/trunk/com/entain/javascript/unstable)
*)
let slash = ref `Reg

let rec token c = lexer
  | newline -> next_line c; token c c.lexbuf
  | blank+ -> token c c.lexbuf

  | "return" | "throw" | "do" | "else" | "in" | "new" | "typeof" ->
      slash := `Reg;
      IDENT (L.utf8_lexeme c.lexbuf)

  | qname ->
      let id = L.utf8_lexeme c.lexbuf in
      let id =
        let len = String.length id in
        if len > 1 && id.[0] = '$' && id.[1] = '$'
        then String.sub id 1 (len - 1)
        else id in
      slash := `Div;
      IDENT (id)

  | '-'? ['0'-'9']+ '.' ['0'-'9']* ->
      slash := `Div;
      FLOAT (L.utf8_lexeme c.lexbuf)

  | '-'? ['0'-'9']+ ->
      slash := `Div;
      INT (L.utf8_lexeme c.lexbuf)

  | "0x" ['0'-'9''a'-'f''A'-'F']+ ->
      slash := `Div;
      HEX (L.utf8_lexeme c.lexbuf)

  | '/' ->
      if !slash = `Reg
      then begin
        set_start_loc c;
        regexp c c.lexbuf;
        slash := `Div;
        REGEXP (get_stored_string c)
      end
      else begin
        slash := `Reg;
        KEYWORD (L.utf8_lexeme c.lexbuf)
      end

  | "/=" ->
      if !slash = `Reg
      then begin
        set_start_loc c;
        store_ascii c '=';
        regexp c c.lexbuf;
        slash := `Div;
        REGEXP (get_stored_string c)
      end
      else begin
        slash := `Reg;
        KEYWORD (L.utf8_lexeme c.lexbuf)
      end

  | "++" | "--" (* XXX `Div is wrong when these appear as prefix ops *)
  | [ "])"] ->
      slash := `Div;
      KEYWORD (L.utf8_lexeme c.lexbuf)

  | [ "{}([.;,<>+-*%&|^!~?:=" ]
  | "<=" | ">=" | "==" | "!=" | "===" | "!==" | "++" | "--" | "<<" | ">>" | ">>>" | "&&"
  | "||" | "+=" | "-=" | "*=" | "%=" | "<<=" | ">>=" | ">>>=" | "&=" | "|=" | "^=" ->
      slash := `Reg;
      KEYWORD (L.utf8_lexeme c.lexbuf)

  | '"' | "'" ->
      let double_quote = L.latin1_lexeme_char c.lexbuf 0 = '"' in
      set_start_loc c;
      string c double_quote c.lexbuf;
      let s = get_stored_string c in
      slash := `Div;
      (if double_quote then STRING2 s else STRING1 s)
  | "/*" ->
      tcomment c c.lexbuf;
      token c c.lexbuf
  | "//" ->
      lcomment c c.lexbuf;
      token c c.lexbuf
  | "$" ->
      set_start_loc c;
      c.enc := Ulexing.Latin1;
      let aq = antiquot c lexbuf in
      c.enc := Ulexing.Utf8;
      slash := `Div; (* XXX ? *)
      aq
  | eof -> EOI
  | _ -> illegal c

and tcomment c = lexer
| "*/" -> ()
| eof -> error c "Unterminated comment"
| newline -> next_line c; tcomment c c.lexbuf
| _ -> tcomment c c.lexbuf

and lcomment c = lexer
| eof -> ()
| newline -> next_line c
| _ -> lcomment c c.lexbuf

and string c double = lexer
| '"' | "'" ->
    let d = L.latin1_lexeme_char c.lexbuf 0 = '"' in
    if d != double then (store_lexeme c; string c double c.lexbuf)
| '\\' ['\\' '"' '\''] ->
    store_ascii c (L.latin1_lexeme_char c.lexbuf 1);
    string c double c.lexbuf
| "\\n" ->
    store_ascii c '\n'; string c double c.lexbuf
| "\\t" ->
    store_ascii c '\t'; string c double c.lexbuf
| "\\r" ->
    store_ascii c '\r'; string c double c.lexbuf
| '\\' ['0'-'9']+ ';' ->
    store_code c (parse_char c 10 1);
    string c double c.lexbuf
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F']+ ';' ->
    store_code c (parse_char c 16 2);
    string c double c.lexbuf
| '\\' ->
    illegal c
| eof | newline ->
    error c "Unterminated string"
| _ ->
    store_lexeme c;
    string c double c.lexbuf

and regexp c = lexer
| '/' -> ()
(* XXX handle escapes *)
| _ ->
    store_lexeme c;
    regexp c c.lexbuf

and antiquot c = lexer
| '$' -> ANTIQUOT ("", "")
| ('`'? (identchar* |'.'+ )) ':' ->
    (* Ulex does not support as patterns like ocamllex *)
    let name = Ulexing.utf8_lexeme c.lexbuf in
    let name = String.sub name 0 (String.length name - 1) in
    antiquot_loop c c.lexbuf;
    ANTIQUOT (name, get_stored_string c)
| newline ->
    next_line c;
    store_lexeme c;
    antiquot_loop c c.lexbuf;
    ANTIQUOT ("", get_stored_string c)
| _ ->
    store_lexeme c;
    antiquot_loop c c.lexbuf;
    ANTIQUOT ("", get_stored_string c)

and antiquot_loop c = lexer
| '$' -> ()
| eof -> error c "Unterminated antiquotation"
| newline -> next_line c; antiquot_loop c c.lexbuf
| '<' (':' ident)? ('@' locname)? '<' ->
    store_lexeme c;
    quotation c c.lexbuf;
    antiquot_loop c c.lexbuf
| _ ->
    store_lexeme c;
    antiquot_loop c c.lexbuf

and quotation c = lexer
| ">>" -> store_lexeme c
| eof -> error c "Unterminated quotation"
| newline -> c.loc <- Loc.move_line 1 c.loc; quotation c c.lexbuf
| _ ->
    store_lexeme c;
    quotation c c.lexbuf

let mk () start_loc cs =
  let enc = ref Ulexing.Utf8 in
  let lb = L.from_var_enc_stream enc cs in
  let c = {
    loc        = start_loc;
    start_loc  = None;
    antiquots  = !Camlp4_config.antiquotations;
    lexbuf     = lb;
    enc        = enc;
    buffer     = Buffer.create 256;
  } in
  let next _ =
    let tok =
      try token c c.lexbuf
      with
        | Ulexing.Error -> error c "Unexpected character"
        | Ulexing.InvalidCodepoint i -> error c "Code point invalid for the current encoding"
    in
    Some (tok, current_loc c)
  in
  Stream.from next
