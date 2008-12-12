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
open Jslib_ast

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

let id ppf i = fprintf ppf "%s" i

let ids ppf is =
  let com = ref false in
  List.iter
    (fun i ->
      if !com then fprintf ppf ",@ " else com := true;
      fprintf ppf "@[%a@]" id i)
    is

let is_postop = function
  | Jadd2_post | Jsub2_post -> true
  | _ -> false

let unop_op = function
  | Jdelete -> "delete"
  | Jvoid -> "void"
  | Jtypeof -> "typeof"
  | Jadd2_pre -> "++"
  | Jsub2_pre -> "--"
  | Jadd_pre -> "+"
  | Jsub_pre -> "-"
  | Jtilde -> "~"
  | Jnot -> "!"
  | Jadd2_post -> "++"
  | Jsub2_post -> "--"

let binop_op = function
  | Jmul -> "*"
  | Jdiv -> "/"
  | Jmod -> "%"
  | Jadd -> "+"
  | Jsub -> "-"
  | Jlsr -> ">>"
  | Jlsl -> "<<"
  | Jasr -> ">>>"
  | Jlt -> "<"
  | Jgt -> ">"
  | Jleq -> "<="
  | Jgeq -> ">="
  | Jinstanceof -> assert false
  | Jeq -> "=="
  | Jneq -> "!="
  | Jseq -> "==="
  | Jsneq -> "!=="
  | Jand -> "&"
  | Jxor -> "^"
  | Jor -> "|"
  | Jland -> "&&"
  | Jlor -> "||"
  | Jcomma -> ","
  | Jhashref -> assert false
  | Jassign -> "="
  | Jmul_assign -> "*="
  | Jdiv_assign -> "/="
  | Jmod_assign -> "%="
  | Jadd_assign -> "+="
  | Jsub_assign -> "-="
  | Jlsl_assign -> "<<="
  | Jlsr_assign -> ">>="
  | Jasr_assign -> ">>>="
  | Jand_assign -> "&="
  | Jxor_assign -> "^="
  | Jor_assign -> "|="

let binop_prec = function
  | Jeq -> pEquality
  | Jneq -> pEquality
  | Jseq -> pEquality
  | Jsneq -> pEquality
  | Jhashref -> pCall
  | Jlt -> pRelational
  | Jgt -> pRelational
  | Jleq -> pRelational
  | Jgeq -> pRelational
  | Jinstanceof -> pRelational
  | Jlsr -> pShift
  | Jlsl -> pShift
  | Jasr -> pShift
  | Jmul -> pMultiplicative
  | Jdiv -> pMultiplicative
  | Jmod -> pMultiplicative
  | Jadd -> pAdditive
  | Jsub -> pAdditive
  | Jand -> pBitwiseAND
  | Jxor -> pBitwiseXOR
  | Jor -> pBitwiseOR
  | Jland -> pLogicalAND
  | Jlor -> pLogicalOR
  | Jcomma -> p
  | Jassign -> pAssignment
  | Jmul_assign -> pAssignment
  | Jdiv_assign -> pAssignment
  | Jmod_assign -> pAssignment
  | Jadd_assign -> pAssignment
  | Jsub_assign -> pAssignment
  | Jlsl_assign -> pAssignment
  | Jlsr_assign -> pAssignment
  | Jasr_assign -> pAssignment
  | Jand_assign -> pAssignment
  | Jxor_assign -> pAssignment
  | Jor_assign -> pAssignment

let prec = function
  | Jthis _ -> pPrimary
  | Jvar _ -> pPrimary
  | Jarray _ -> pPrimary
  | Jobject _ -> pPrimary
  | Jstring _ -> pPrimary
  | Jnum _ -> pPrimary
  | Jnull _ -> pPrimary
  | Jfun _ -> pPrimary
  | Jbool _ -> pPrimary

  | Jfieldref _ -> pMember
  | Jnew _ -> pMember

  | Junop (_, op, _) -> if is_postop op then pPostfix else pUnary
  | Jbinop (_, op, _, _) -> binop_prec op

  | Jite _ -> pConditional
  | Jcall _ -> pCall
  | Jexp_Ant _ -> pPrimary

let opt f ppf x =
  match x with
    | None -> ()
    | Some x -> f ppf x

let opt_nbsp f ppf x =
  match x with
    | None -> ()
    | Some x ->
        fprintf ppf " ";
        f ppf x

let rec expp pr ppf e =
  if prec e < pr
  then fprintf ppf "(@[%a@])" exp e
  else exp ppf e

and exp ppf = function
  | Jthis _  -> fprintf ppf "this"
  | Jvar (_, i) -> fprintf ppf "%s" i
  | Jarray (_, es) -> fprintf ppf "@[[%a]@]" exps es
  | Jobject (_, kvs) ->
      let keyvals ppf kvs =
        let com = ref false in
        List.iter
          (fun (k, v) ->
            if !com then fprintf ppf ",@ " else com := true;
            fprintf ppf "@[%a: %a@]" (expp pAssignment) k (expp pAssignment) v)
          kvs in
      fprintf ppf "@[{%a}@]" keyvals kvs
  | Jstring (_, s, false) -> fprintf ppf "\"%s\"" (JSString.escaped s)
  | Jstring (_, s, true) -> fprintf ppf "\'%s\'" (JSString.escaped s)
  | Jnum (_, n) -> fprintf ppf "%s" n
  | Jnull _ -> fprintf ppf "null"
  | Jbool (_, b) -> fprintf ppf "%B" b
  | Jfun (_, io, is, ss) ->
      fprintf ppf "@[<hv>function %a(@[%a@]) %a@]" (opt_nbsp id) io ids is block ss

  | Jfieldref (_, e, i) -> fprintf ppf "@[%a.%s@]" (expp pMember) e i

  | Junop (_, op, e) ->
      if is_postop op
      then
        begin
          fprintf ppf "@[%a%s@]" (expp pPostfix) e (unop_op op)
        end
      else
        begin
          match op with
            | Jdelete | Jvoid | Jtypeof -> fprintf ppf "@[%s@;<1 2>%a@]" (unop_op op) (expp pUnary) e
            | _ -> fprintf ppf "@[%s%a@]" (unop_op op) (expp pUnary) e
        end

  | Jbinop (_, op, e1, e2) ->
      begin
        match op with
          | Jhashref -> fprintf ppf "@[%a[%a]@]" (expp pCall) e1 (expp p) e2
          | Jcomma -> fprintf ppf "@[%a, %a@]" (expp p) e1 (expp pAssignment) e2
          | _ ->
              let prec = binop_prec op in
              fprintf ppf "@[%a %s@ %a@]" (expp prec) e1 (binop_op op) (expp (prec + 2)) e2
      end

  | Jite (_, i, t, e) ->
      fprintf ppf "@[%a ?@ %a :@ %a@]"
        (expp pLogicalOR) i
        (expp pAssignment) t
        (expp pAssignment) e

  | Jcall (_, e, es) -> fprintf ppf "@[%a(%a)@]" (expp pCall) e exps es

  | Jnew (_, e, None) -> fprintf ppf "@[new %a@]" (expp pMember) e
  | Jnew (_, e, Some es) -> fprintf ppf "@[new %a(%a)@]" (expp pMember) e exps es
  | Jexp_Ant (_, s) -> fprintf ppf "$%s$" s

and exps ppf es =
  match es with
    | Jexp_list (_, es) ->
        let com = ref false in
        List.iter
          (fun e ->
            if !com then fprintf ppf ",@ " else com := true;
            fprintf ppf "@[<2>%a@]" (expp pAssignment) e)
          es
    | Jexp_list_Ant (_, s) -> fprintf ppf "$%s$" s

and stmt ppf = function
  | Jempty _ -> fprintf ppf ";"

  | Jvars (_, vars) ->
      let fvars ppf vars =
        let comma = ref false in
        List.iter
          (fun (i, e) ->
            if !comma then fprintf ppf ",@ " else comma := true;
            match e with
              | Some e -> fprintf ppf "%s =@;<1 2>%a" i (expp pAssignment) e
              | None -> fprintf ppf "%s" i)
          vars in
      fprintf ppf "@[var %a;@]" fvars vars

  | Jfuns (_, i, is, ss) ->
      fprintf ppf "@[<hv>function %s (@[%a@]) %a@]" i ids is block ss

  | Jreturn (_, e) -> fprintf ppf "@[<h>return%a;@]" (opt_nbsp (expp p)) e
  | Jcontinue (_, i) -> fprintf ppf "@[continue%a;@]" (opt_nbsp id) i
  | Jbreak (_, i) -> fprintf ppf "@[break%a;@]" (opt_nbsp id) i

  | Jites (_, i, t, None) ->
      fprintf ppf
        "@[<hv>if (%a)@ %a@]"
        (expp p) i stmt t

  | Jites (_, i, t, Some e) ->
      fprintf ppf
        "@[<hv>if (%a)@ %a@ else %a@]"
        (expp p) i stmt t stmt e

  | Jswitch (_, e, cs, fss) ->
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
        "@[<hv>switch (%a)@ {@ %a@ }@]"
        (expp p) e cases (cs, fss)

  | Jthrow (_, e) -> fprintf ppf "@[throw %a;@]" (expp p) e

  | Jexps (_, (Jcall (_, Jfun _, _) as e)) -> fprintf ppf "@[(%a);@]" (expp p) e
  | Jexps (_, e) -> fprintf ppf "@[%a;@]" (expp p) e

  | Jtrycatch (_, ss, ci, css) ->
      fprintf ppf "@[<hv>try %a catch (%s) %a@]" block ss ci block css
  | Jtryfinally (_, ss, fss) ->
      fprintf ppf "@[<hv>try %a finally %a@]" block ss block fss
  | Jtrycatchfinally (_, ss, ci, css, fss) ->
      fprintf ppf "@[<hv>try %a catch (%s) %a finally %a@]" block ss ci block css block fss

  | Jfor (_, e1, e2, e3, s) ->
      fprintf ppf "@[<hv>for (%a;@ %a;@ %a) %a@]" (opt (expp p)) e1 (opt (expp p)) e2 (opt (expp p)) e3 stmt s

  | Jdowhile (_, s, e) ->
      fprintf ppf "@[<hv>do@ %a@ while (%a);@]" stmt s (expp p) e

  | Jwhile (_, e, s) ->
      fprintf ppf "@[<hv>while (%a)@ %a@]" (expp p) e stmt s

  | Jblock (_, ss) -> fprintf ppf "{@;<1 2>%a@ }" stmts ss
  | Jwith (_, e, s) -> fprintf ppf "@[<hv>with (%a)@ %a@]" (expp p) e stmt s
  | Jlabel (_, i, s) -> fprintf ppf "@[%s: %a@]" i stmt s
  | Jstmt_Ant (_, s) -> fprintf ppf "$%s$" s

and block ppf ss = fprintf ppf "{@;<1 2>%a@ }" stmts ss

and stmts ppf ss =
  let sl ppf ss =
    let spc = ref false in
    List.iter
      (fun s ->
        if !spc then fprintf ppf "@ " else spc := true;
        stmt ppf s;)
      ss in
  fprintf ppf "@[<v>%a@]" sl ss

let escaped = JSString.escaped
