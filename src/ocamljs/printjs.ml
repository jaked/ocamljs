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

open Format
open Js

exception Unimplemented of string

(* XXX figure out how Format works *)

(*
  precedence, see ECMA 262:
    http://www.ecma-international.org/publications/files/EMCA-ST/Ecma-262.pdf
*)
let p = 0
let pAssignment = 2
let pConditional = 4
let pLogicalOR = 6
let pLogicalAND = 8
let pBitwiseOR = 10
let pBitwiseXOR = 12
let pBitwiseAND = 14
let pEquality = 16
let pRelational = 18
let pShift = 20
let pAdditive = 22
let pMultiplicative = 24
let pUnary = 26
let pPostfix = 28
let pLeftHandSide = 30
let pCall = 32
let pMember = 34
let pPrimary = 36


module JSString =
  struct
    open String

(* adapted from stdlib/string.ml *)
external is_printable: char -> bool = "caml_is_printable"
external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
        (match unsafe_get s i with
           '"' | '\\' | '\n' | '\t' | '\r' -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n = length s then s else begin
      let s' = create !n in
        n := 0;
        for i = 0 to length s - 1 do
          begin
            match unsafe_get s i with
              ('"' | '\\') as c ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
            | '\n' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
            | '\t' ->
                unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
	    | '\r' ->
		unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
            | c ->
                if is_printable c then
                  unsafe_set s' !n c
                else begin
                  let a = char_code c in
                  unsafe_set s' !n '\\';
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a / 100));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
                  incr n;
                  unsafe_set s' !n (char_chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        s'
      end

  end

let ids ppf is =
  let com = ref false in
  List.iter
    (fun i ->
      if !com then fprintf ppf ",@ " else com := true;
      fprintf ppf "@[%s@]" i)
    is

let rec expp pr ppf e =
  let prec = function
    | Jthis -> pPrimary
    | Jvar _ -> pPrimary
    | Jarray _ -> pPrimary
    | Jobject _ -> pPrimary
    | Jstring _ -> pPrimary
    | Jnum _ -> pPrimary
    | Jnull -> pPrimary
    | Jfun _ -> pPrimary
    | Jbool _ -> pPrimary

    | Jfieldref _ -> pMember
    | Jnew _ -> pMember

    | Jtypeof _ -> pUnary
    | Jminus _ -> pUnary
    | Jnot _ -> pUnary

    | Jplus2 _ -> pPostfix
    | Jminus2 _ -> pPostfix

    | Jeq _ -> pEquality
    | Jneq _ -> pEquality
    | Jseq _ -> pEquality
    | Jsneq _ -> pEquality

    | Jhashref _ -> pCall
    | Jite _ -> pConditional
    | Jcall _ -> pCall

    | Jlt _ -> pRelational
    | Jgt _ -> pRelational
    | Jleq _ -> pRelational
    | Jgeq _ -> pRelational
    | Jlsr _ -> pShift
    | Jlsl _ -> pShift
    | Jasr _ -> pShift
    | Jmul _ -> pMultiplicative
    | Jdiv _ -> pMultiplicative
    | Jmod _ -> pMultiplicative
    | Jadd _ -> pAdditive
    | Jsub _ -> pAdditive
    | Jand _ -> pBitwiseAND
    | Jxor _ -> pBitwiseXOR
    | Jor _ -> pBitwiseOR
    | Jland _ -> pLogicalAND
    | Jlor _ -> pLogicalOR
    | Jcomma _ -> p
    | Jassign _ -> pAssignment in

  if prec e < pr
  then fprintf ppf "(@[%a@])" exp e
  else exp ppf e

and exp ppf = function
  | Jthis -> fprintf ppf "this"
  | Jvar i -> fprintf ppf "%s" i
  | Jarray es -> fprintf ppf "@[[%a]@]" exps es
  | Jobject kvs ->
      let keyvals ppf kvs =
	let com = ref false in
	List.iter
	  (fun (k, v) ->
	    if !com then fprintf ppf ",@ " else com := true;
	    fprintf ppf "@[%s: %a@]" k (expp pAssignment) v)
	  kvs in
      fprintf ppf "@[{%a}@]" keyvals kvs
  | Jstring s -> fprintf ppf "\"%s\"" (JSString.escaped s)
  | Jnum n ->
      let s = sprintf "%f" n in
      let rec l n =
	match s.[n - 1] with
	  | '0' -> l (n - 1)
	  | '.' -> n - 1
	  | _ -> n in
      fprintf ppf "%s" (String.sub s 0 (l (String.length s)))
  | Jnull -> fprintf ppf "null"
  | Jbool b -> fprintf ppf "%B" b
  | Jfun (is, ss) ->
      fprintf ppf "@[<hv>function (@[%a@]) %a@]" ids is block ss

  | Jfieldref (e, i) -> fprintf ppf "@[%a.%s@]" (expp pMember) e i

  | Jtypeof e ->
      fprintf ppf "@[typeof@;<1 2>%a@]" (expp pUnary) e

  | Jnot e -> preop ppf "!" e pUnary
  | Jminus e -> preop ppf "-" e pUnary

  | Jplus2 e -> postop ppf "++" e pLeftHandSide
  | Jminus2 e -> postop ppf "--" e pLeftHandSide

  | Jmul (e1, e2) -> binop ppf e1 pMultiplicative "*" e2 pUnary
  | Jdiv (e1, e2) -> binop ppf e1 pMultiplicative "/" e2 pUnary
  | Jmod (e1, e2) -> binop ppf e1 pMultiplicative "%" e2 pUnary

  | Jadd (e1, e2) -> binop ppf e1 pAdditive "+" e2 pMultiplicative
  | Jsub (e1, e2) -> binop ppf e1 pAdditive "-" e2 pMultiplicative

  | Jlsr (e1, e2) -> binop ppf e1 pShift ">>" e2 pAdditive
  | Jlsl (e1, e2) -> binop ppf e1 pShift "<<" e2 pAdditive
  | Jasr (e1, e2) -> binop ppf e1 pShift ">>>" e2 pAdditive

  | Jlt (e1, e2) -> binop ppf e1 pRelational "<" e2 pShift
  | Jgt (e1, e2) -> binop ppf e1 pRelational ">" e2 pShift
  | Jleq (e1, e2) -> binop ppf e1 pRelational "<=" e2 pShift
  | Jgeq (e1, e2) -> binop ppf e1 pRelational ">=" e2 pShift

  | Jeq (e1, e2) -> binop ppf e1 pEquality "==" e2 pRelational
  | Jneq (e1, e2) -> binop ppf e1 pEquality "!=" e2 pRelational
  | Jseq (e1, e2) -> binop ppf e1 pEquality "===" e2 pRelational
  | Jsneq (e1, e2) -> binop ppf e1 pEquality "!==" e2 pRelational

  | Jand (e1, e2) -> binop ppf e1 pBitwiseAND "&" e2 pEquality
  | Jxor (e1, e2) -> binop ppf e1 pBitwiseXOR "^" e2 pBitwiseAND
  | Jor (e1, e2) -> binop ppf e1 pBitwiseOR "|" e2 pBitwiseXOR

  | Jland (e1, e2) -> binop ppf e1 pLogicalAND "&&" e2 pBitwiseOR
  | Jlor (e1, e2) -> binop ppf e1 pLogicalOR "||" e2 pLogicalAND

  | Jhashref (e1, e2) ->
      fprintf ppf "@[%a[%a]@]"
	(expp pCall) e1 (expp p) e2

  | Jite (i, t, e) ->
      fprintf ppf "@[%a ?@ %a :@ %a@]"
	(expp pLogicalOR) i
	(expp pAssignment) t
	(expp pAssignment) e

  | Jcall (e, es) -> fprintf ppf "@[%a(%a)@]" (expp pCall) e exps es

  | Jnew (id, es) -> fprintf ppf "@[new %s(%a)@]" id exps es

  | Jcomma (e1, e2) -> binop ppf e1 p "," e2 pAssignment

  | Jassign (e1, e2) -> binop ppf e1 pLeftHandSide "=" e2 pAssignment

and preop ppf op e p =
  fprintf ppf "@[%s%a@]" op (expp p) e

and postop ppf op e p =
  fprintf ppf "@[%a%s@]" (expp p) e op

and binop ppf e1 p1 op e2 p2 =
  fprintf ppf "@[%a %s@ %a@]" (expp p1) e1 op (expp p2) e2

and exps ppf es =
  let com = ref false in
  List.iter
    (fun e ->
      if !com then fprintf ppf ",@ " else com := true;
      fprintf ppf "@[<2>%a@]" (expp pAssignment) e)
    es

and stmt ppf = function
  | Jempty -> ()
  | Jvars (i, e) ->
      fprintf ppf "@[var %s =@;<1 2>%a@]" i (expp pAssignment) e
  | Jfuns (i, is, ss) ->
      fprintf ppf "@[<hv>function %s (@[%a@]) %a@]" i ids is block ss
  | Jbreak -> fprintf ppf "break"
  | Jreturn e -> fprintf ppf "@[return %a@]" (expp p) e
  | (Jites (i, t, []) | Jites (i, t, [Jempty])) ->
      fprintf ppf
	"@[<hv>if (%a)%a@]"
	(expp p) i maybe_block t
  | Jites (i, t, e) ->
      fprintf ppf
	"@[<hv>if (%a)%aelse %a@]"
	(expp p) i maybe_block t maybe_block e
  | Jswitch (e, cs, fss) ->
      let cases ppf (cs, fss) =
	let spc = ref false in
	List.iter
	  (fun (i, ss) ->
	    if !spc then fprintf ppf "@ " else spc := true;
	    fprintf ppf "@[<hv>case %a:@;<1 2>@[%a@]@]"
	      (expp p) i stmts ss)
	  cs;
	match fss with
	  | None -> ()
	  | Some fss ->
	      if !spc then fprintf ppf "@ " else spc := true;
	      fprintf ppf "@[<hv>default:@;<1 2>@[%a@]@]" stmts fss in
      fprintf ppf
	"@[<hv>switch (%a) {@,@[%a@]}@]"
	(expp p) e cases (cs, fss)
  | Jthrow e -> fprintf ppf "@[throw %a@]" (expp p) e
  | Jexps (Jcall (Jfun _, _) as e) -> fprintf ppf "@[(%a)@]" (expp p) e
  | Jexps e -> fprintf ppf "@[%a@]" (expp p) e
  | Jtrycatch (s1, i, s2) ->
      fprintf ppf "@[<hv>try %a catch (%s) %a@]" block s1 i block s2

  | Jfor (s1, e, s2, ss) ->
      fprintf ppf "@[<hv>for (%a; %a; %a) %a@]" stmt s1 (expp p) e stmt s2 maybe_block ss

  | Jwhile (e, ss) ->
      fprintf ppf "@[<hv>while (%a) %a@]" (expp p) e maybe_block ss

  | Jlabel (i, ss) -> fprintf ppf "@[<hv>%s: %a@]" i maybe_block ss
  | Jbreakto i     -> fprintf ppf "@[break %s@]" i

and maybe_block ppf = function
  | [] -> fprintf ppf "@;<1 2>;@ "
  | [s] ->
      if st_needs_sc s
      then fprintf ppf "@;<1 2>%a;@ " stmt s
      else fprintf ppf "@;<1 2>%a@ " stmt s
  | ss -> block ppf ss

and block ppf ss = fprintf ppf "{@;<1 2>%a@ }" stmts ss

and st_needs_sc = function
  | (Jfuns _ | Jites _ | Jfor _ | Jwhile _ | Jswitch _ | Jtrycatch _ | Jlabel _) -> false
  | _ -> true

and stmts ppf ss =
  let sl ppf ss =
    let spc = ref false in
    List.iter
      (fun s ->
	if !spc then fprintf ppf "@ " else spc := true;
	stmt ppf s;
	if st_needs_sc s
	then fprintf ppf ";")
      ss in
  fprintf ppf "@[<v>%a@]" sl ss
