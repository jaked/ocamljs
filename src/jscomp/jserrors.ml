(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (driver/errors.ml in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* WARNING: if you change something in this file, you must look at
   opterrors.ml to see if you need to make the same changes there.
*)

open Format

(* Report an error *)

module Location =
struct
IFDEF OCAML_3_10_2 THEN
  let print ppf loc = Location.print ppf loc;
ELSE
  let print ppf loc = Location.print_error ppf loc;
ENDIF
end

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, loc) ->
      Location.print ppf loc;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Pparse.Error ->
      fprintf ppf "Preprocessor error"
  | Env.Error err ->
      Env.report_error ppf err
  | Ctype.Tags(l, l') -> fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, err) ->
      Location.print ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, err) ->
      Location.print ppf loc; Typeclass.report_error ppf err
  | Includemod.Error err ->
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print ppf loc; Translcore.report_error ppf err
  | Translclass.Error(loc, err) ->
      Location.print ppf loc; Translclass.report_error ppf err
  | Translmod.Error(loc, err) ->
      Location.print ppf loc; Translmod.report_error ppf err
  | Symtable.Error code ->
      Symtable.report_error ppf code
  | Jslink.Error code ->
      Jslink.report_error ppf code
  | Jslibrarian.Error code ->
      Jslibrarian.report_error ppf code
  | Jspackager.Error code ->
      Jspackager.report_error ppf code
  | Sys_error msg ->
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      fprintf ppf "@.Error: error-enabled warnings (%d occurrences)" n
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn
