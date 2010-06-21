(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Original file (bytecomp/bytegen.ml in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

open Misc
open Asttypes
open Primitive
open Types
open Lambda
open Jslib_ast
open Cmo_format (* for reloc stuff *)

exception Unimplemented of string

let unimplemented msg lambda =
  let b = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer b in
  Format.fprintf fmt "%s: %a" msg Printlambda.lambda lambda;
  Format.pp_print_flush fmt ();
  raise (Unimplemented (Buffer.contents b))

(* Relocation information -- just for consistency checking *)

(* XXX could filter dups here *)
let reloc_info = ref ([] : (reloc_info * int) list)

let enter info =
  reloc_info := (info, 0) :: !reloc_info

let enter_literal sc =
  enter (Reloc_literal sc)
and enter_getglobal id =
  enter (Reloc_getglobal id)
and enter_setglobal id =
  enter (Reloc_setglobal id)
and enter_c_prim name =
  enter (Reloc_primitive name)

(**** Compilation of a lambda expression ****)

let jsident_of_ident id =
  let b = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer b in
  Ident.print ppf id;
  Format.pp_print_flush ppf ();
  let s = Buffer.contents b in
  let s = if String.contains s '/' then s else "oc$" ^ s in

  Buffer.clear b;
  for i = 0 to String.length s - 1 do
    match s.[i] with
      | ('A' .. 'Z'|'a' .. 'z'|'0' .. '9'|'_'|'$') as c ->
          Buffer.add_char b c
      | ('/'|'!'|'#') -> (* XXX safe? *)
          Buffer.add_char b '$'
      | c ->
          Buffer.add_char b '$';
          Buffer.add_string b (Printf.sprintf "%02X" (int_of_char c))
  done;
  Buffer.contents b

let label_raise lab = "r$" ^ string_of_int lab
let flag_raised lab = "$r" ^ string_of_int lab
let raise_arg lab i = "$r" ^ string_of_int lab ^ "_" ^ string_of_int i

let _loc = Camlp4.PreCast.Loc.ghost

let makeblock tag ces =
  match tag with
    | 0 -> << $$($list:ces$) >>
    | (1|2|3|4|5|6|7|8|9) -> let id = "$" ^ string_of_int tag in << $id:id$($list:ces$) >>
    | _ -> << $$N($`int:tag$, [$list:ces$]) >>

let exp_of_stmts ss = << $Jfun (_loc, None, [], ss)$ () >>

let rec stmt_rev_append l1 l2 =
  match l1 with
    | Jstmt_nil _ -> l2
    | Jstmt_cons (_loc, a, l) -> stmt_rev_append l (Jstmt_cons (loc_of_stmt a, a, l2))
    | s -> Jstmt_cons (loc_of_stmt s, s, l2)

let comp_const c =
  match c with
    | Const_int i -> << $`int:i$ >>
    | Const_char c -> << $`int:Char.code c$ >>
    | Const_string s -> Jstring (_loc, s, false)
    | Const_float s -> Jnum (_loc, s) (* XXX different float syntax *)
    | Const_int32 i32 -> Jnum (_loc, Int32.to_string i32)
    | Const_int64 i64 -> Jstring (_loc, Int64.to_string i64, false)
    | Const_nativeint ni -> Jnum (_loc, Nativeint.to_string ni)

let rec comp_sconst c =
  match c with
    | Const_base c -> comp_const c
    | Const_pointer i -> << $`int:i$ >>
    | Const_block (tag, cs) ->
        makeblock tag (List.map comp_sconst cs)
    | Const_float_array ss ->
        makeblock 0 (List.map (fun s -> Jnum (_loc, s)) ss) (* XXX different float syntax *)
    | Const_immstring s -> Jstring (_loc, s, false) (* XXX when does this happen? *)

let kreturn e = <:stmt< return $e$; >>

let keffect = function
    (* anything that is already a value can have no effect *)
    (* mostly this arises with the compilation of () as Jnum 0 *)
  | (Jnum _ | Jvar _) -> Jstmt_nil _loc
  | e -> Jexps (_loc, e)

let comp_ccall c es =
  match c, es with
    | ("caml_int32_format", _ | "caml_nativeint_format", _ | "caml_int64_format", _) -> << caml_format_int($list:es$) >>
    | "caml_format_float", _ -> << oc$$sprintf($list:es$) >>
    | "caml_string_equal", _ -> << oc$$seq($list:es$) >>
    | "caml_string_notequal", _ -> << oc$$sneq($list:es$) >>
    | "caml_string_lessthan", _ -> << oc$$slt($list:es$) >>
    | "caml_string_greaterthan", _ -> << oc$$sgt($list:es$) >>
    | "caml_string_lessequal", _ -> << oc$$slte($list:es$) >>
    | "caml_string_greaterequal", _ -> << oc$$sgte($list:es$) >>
    | "caml_create_string", _ -> << oc$$cms($list:es$) >>

    | "caml_power_float", _ -> << Math.pow($list:es$) >>
    | "caml_exp_float", _ -> << Math.exp($list:es$) >>
    | "caml_acos_float", _ -> << Math.acos($list:es$) >>
    | "caml_asin_float", _ -> << Math.asin($list:es$) >>
    | "caml_atan_float", _ -> << Math.atan($list:es$) >>
    | "caml_atan2_float", _ -> << Math.atan2($list:es$) >>
    | "caml_cos_float", _ -> << Math.cos($list:es$) >>
    (* | "caml_cosh_float", _ -> ? *)
    | "caml_log_float", _ -> << Math.log($list:es$) >>
    (* | "caml_log10_float", _ -> ? *)
    | "caml_sin_float", _ -> << Math.sin($list:es$) >>
    (* | "caml_sinh_float", _ -> ? *)
    | "caml_sqrt_float", _ -> << Math.sqrt($list:es$) >>
    | "caml_tan_float", _ -> << Math.tan($list:es$) >>
    (* | "caml_tanh_float", _ -> ? *)
    | "caml_ceil_float", _ -> << Math.ceil($list:es$) >>
    | "caml_floor_float", _ -> << Math.floor($list:es$) >>
    | "caml_abs_float", _ -> << Math.abs($list:es$) >>

    | "$assign", [e1; e2] -> << $e1$ = $e2$ >>
    | "$call", e::es -> << $e$($list:es$) >>
    | "$false", _ -> << false >>
    | "$fieldref", [e; Jstring (_loc, id, _)] -> << $e$.$id$ >>
    | "$function", [Jcall (_loc, Jvar _, (Jfun _ as f))] -> f

    (* removes initial dummy arg; see camlinternalOO.ml *)
    | "$dummyargfun", [Jcall (_loc, Jvar (_loc2, v), (Jfun (_loc3, name, (_::args), stmts)))] ->
        Jcall (_loc, Jvar (_loc2, v), (Jfun (_loc3, name, args, stmts)))

    | "$hashref", [e1; e2] -> << $e1$[$e2$] >>
    | "$new", (Jstring (_, id, _))::es -> << new $id:id$($list:es$) >>
    | "$null", _ -> << null >>
    | "$this", _ -> << this >>
    | "$throw", [e] -> exp_of_stmts <:stmt< throw $e$; >>
    | "$true", _ -> << true >>

    | "$var", [Jstring (_loc, id, _)] -> << $id:id$ >>

    | "$obj", [e] ->
        let rec o e l =
          match e with
            | Jcall (_, Jvar _, Jexp_cons (_, Jcall (_, Jvar _, Jexp_cons (_, (Jstring _ as k), v)), e)) -> o e ((k, v)::l)
            | Jnum _ -> List.rev l
            | _ -> raise (Unimplemented "bad $obj") in
        Jobject (_loc, o e [])

    | _ ->
        match c.[0], es with
          | '#', e::es -> let met = String.sub c 1 (String.length c - 1) in << $e$.$met$($list:es$) >>
          | '.', [e] -> let fld = String.sub c 1 (String.length c - 1) in << $e$.$fld$ >>
          | '=', [e1; e2] -> let fld = String.sub c 1 (String.length c - 1) in << $e1$.$fld$ = $e2$ >>
          | '@', _ -> let id = String.sub c 1 (String.length c - 1) in << $id:id$($list:es$) >>
          | _ -> enter_c_prim c; << $id:c$($list:es$) >>

let comp_comparison c e1 e2 =
  match c with
    | Ceq -> << $e1$ === $e2$ >>
    | Cneq -> << $e1$ !== $e2$ >>
    | Clt -> << $e1$ < $e2$ >>
    | Cgt -> << $e1$ > $e2$ >>
    | Cle -> << $e1$ <= $e2$ >>
    | Cge -> << $e1$ >= $e2$ >>

let comp_prim p es =
  match p, es with
    | Pgetglobal i, [] -> enter_getglobal i; << $id:jsident_of_ident i$ >>
    | Pmakeblock (tag, _), _ -> makeblock tag es

    | (Pfield i | Pfloatfield i), [e] -> << $e$[$`int:i$] >>
    | (Psetfield (i, _) | Psetfloatfield i), [e1; e2] -> << $e1$[$`int:i$] = $e2$ >>
    | Pccall { prim_name = "$new"; prim_native_name = "" }, es -> comp_ccall "$new" es
    | Pccall { prim_name = "$new"; prim_native_name = id }, es -> << new $id:id$($list:es$) >>
    | Pccall { prim_name = n }, es -> comp_ccall n es
    | Pisout, [h; e] -> << $e$ < 0 || $e$ > $h$ >> (* XXX bind e to var? *)
    | Pabsfloat, [e] -> << Math.abs($exp:e$) >>

    | (Pintcomp c | Pbintcomp (_, c) | Pfloatcomp c), [e1; e2] ->
        comp_comparison c e1 e2

    | (Pnegint | Pnegbint _ | Pnegfloat), [e] -> << -$e$ >>
    | (Paddint | Paddbint _ | Paddfloat), [e1; e2] -> << $e1$ + $e2$ >>
    | (Psubint | Psubbint _ | Psubfloat), [e1; e2] -> << $e1$ - $e2$ >>
    | (Pmulint | Pmulbint _ | Pmulfloat), [e1; e2] -> << $e1$ * $e2$ >>
    | Pdivfloat, [e1; e2] -> << $e1$ / $e2$ >>
    | (Pdivint | Pdivbint _), [e1; e2] ->
        (* XXX << ($e1$ / $e2$) < < 0 >> *)
        Jbinop(_loc,
                        Jlsr,
                        << $e1$ / $e2$ >>,
                        << 0 >>)
    | (Pmodint | Pmodbint _), [e1; e2] -> << $e1$ % $e2$ >>

    | (Plslint | Plslbint _), [e1; e2] -> Jbinop(_loc, Jlsl, e1, e2)
    | (Plsrint | Plsrbint _), [e1; e2] -> Jbinop(_loc, Jlsr, e1, e2)
    | (Pasrint | Pasrbint _), [e1; e2] -> Jbinop(_loc, Jasr, e1, e2)

    | (Pandint | Pandbint _), [e1; e2] -> << $e1$ & $e2$ >>
    | (Porint | Porbint _), [e1; e2] -> << $e1$ | $e2$ >>
    | (Pxorint | Pxorbint _), [e1; e2] -> << $e1$ ^ $e2$ >>

    | Pnot, [e] -> << !$e$ >>
    | Psequand, [e1; e2] -> << $e1$ && $e2$ >>
    | Psequor, [e1; e2] -> << $e1$ || $e2$ >> (* XXX rhs is possibly a tail call *)

    | Poffsetint n, [e] -> << $`int:n$ + $e$ >>

    | Poffsetref 1, [e] -> << $e$[0]++ >>
    | Poffsetref -1, [e] -> << $e$[0]-- >>
    | Poffsetref n, [e] -> << $e$[0] = $`int:n$ + $e$[0] >> (* XXX bind e to var? *)

    | Pstringlength, [e] -> << $e$.length >>
    | Parraylength _, [e] -> << $e$.length >>

    | Pmakearray _, es -> makeblock 0 es

    | Pstringrefu, _ -> << oc$$srefu($list:es$) >>
    | Pstringsetu, _ -> << oc$$ssetu($list:es$) >>
    | Pstringrefs, _ -> << oc$$srefs($list:es$) >>
    | Pstringsets, _ -> << oc$$ssets($list:es$) >>

    | Parrayrefu _, [e1; e2] -> << $e1$[$e2$] >>
    | Parraysetu _, [e1; e2; e3] -> << $e1$[$e2$] = $e3$ >>
    | Parrayrefs _, _ -> << oc$$arefs($list:es$) >>
    | Parraysets _, _ -> << oc$$asets($list:es$) >>

    | Pisint, [e] -> << typeof $e$ == 'number' >>

    | Pintoffloat, [e] -> Jbinop(_loc, Jlsr, e, << 0 >>);

    | (Pidentity | Pignore | Pfloatofint |
       Pintofbint _ | Pbintofint _ | Pcvtbint _), [e] ->
        e

    | Pduprecord _, [e] -> << caml_obj_dup($exp:e$) >>

    | _ -> unimplemented "comp_prim" (Lprim (p, []))

let starts_with s sw =
  let sl = String.length s in
  let swl = String.length sw in
  sl >= swl && String.sub s 0 swl = sw

let drop s n =
  let sl = String.length s in
  if sl <= n then s
  else String.sub s n (sl - n)

let inline_string = function
  | Lconst (Const_base (Const_string s)) -> s
  | _ -> raise (Failure "bad inline string")

let rec inline_bool = function
    (* bool literals from expanded inline code get an extra $inline_exp in translcore.ml *)
  | Lprim (Pccall { prim_name = "$inline_exp" }, [ <:lam_aexp< Jbool ($_$, $b$) >> ]) -> inline_bool b

  | Lconst (Const_pointer 0) -> false
  | Lconst (Const_pointer 1) -> true

  | l ->
      Format.fprintf Format.str_formatter "bad inline bool: %a@." Printlambda.lambda l;
      raise (Failure (Format.flush_str_formatter ()))

let makeblock_of_const = function
  | Lconst (Const_block (tag, cs)) ->
      Lprim (Pmakeblock (tag, Asttypes.Immutable), List.map (fun c -> Lconst c) cs)
  | _ -> assert false

let rec inline_option inline = function
  | Lconst (Const_block _) as cb -> inline_option inline (makeblock_of_const cb)
  | Lconst (Const_pointer 0) -> None
  | Lprim (Pmakeblock (0, _), [v]) -> Some (inline v)
  | _ -> raise (Failure "bad inline option")

let rec inline_list inline = function
  | Lconst (Const_block _) as cb -> inline_list inline (makeblock_of_const cb)
  | Lconst (Const_pointer 0) -> []
  | Lprim (Pmakeblock (0, _), [h; t]) -> inline h :: inline_list inline t
  | _ -> raise (Failure "bad inline list")

let rec inline_pair inline1 inline2 = function
  | Lconst (Const_block _) as cb -> inline_pair inline1 inline2 (makeblock_of_const cb)
  | Lprim (Pmakeblock (0, _), [c1; c2]) -> (inline1 c1, inline2 c2)
  | _ -> raise (Failure "bad inline pair")

(* compile a lambda as a Js.exp *)
(* tail is true if the expression is in tail position *)
let rec comp_expr tail expr =
  match expr with
    | (Llet _ | Lletrec _) -> exp_of_stmts (comp_letrecs_st tail expr kreturn)

    | (Lswitch _ | Lprim (Praise, _) | Lstaticcatch _ | Lstaticraise _ |
          Ltrywith _ | Lfor _ | Lwhile _ |
            Lprim (Pccall { prim_name = "$inline_stmt" }, _))->
        exp_of_stmts (comp_expr_st tail expr kreturn)

    | Lvar id -> << $id:jsident_of_ident id$ >>

    | Lfunction (_, args, e) ->
        let e = Jfun (_loc, None, List.map jsident_of_ident args, comp_expr_st true e kreturn) in
        << _f($exp:e$) >>

    | IFDEF OCAML_3_10_2 THEN Lapply(e, es) ELSE Lapply(e, es, _) ENDIF ->
        let app = if tail then "__" else "_" in
        let ce = comp_expr false e in
        let ces = List.map (comp_expr false) es in
        << $id:app$($exp:ce$, [$list:ces$]) >>
 
    | Lifthenelse (i, t, e) ->
        let ci = comp_expr false i in
        let ct = comp_expr tail t in
        let ce = comp_expr tail e in
        << $ci$ ? $ct$ : $ce$ >>

    | Lconst c -> comp_sconst c

    | Lsequence (e1, e2) -> << $comp_expr false e1$, $comp_expr tail e2$ >>

    | Lassign (i, e) -> << $id:jsident_of_ident i$ = $comp_expr false e$ >> (* XXX *)

    | Lprim (Pccall { prim_name = "$inline_exp" }, [e]) -> inline_exp e

    | Lprim (p, args) -> comp_prim p (List.map (comp_expr false) args)

    | Lsend (Public, Lconst(Const_immstring m), o, args) ->
        (*
          XXX
          an OCaml-defined object should not get the special method
          name treatment. but we have no way to know at the call site.
        *)
        let app = if tail then "__m" else "_m" in
        let co = comp_expr false o in
        let cargs = List.map (comp_expr false) args in
        let op, m = match m with
          | _ when starts_with m "_get_" -> (`Get, drop m 5)
          | _ when starts_with m "_set_" -> (`Set, drop m 5)
          | _ when starts_with m "_" -> (`Call, drop m 1)
          | _ -> (`Call, m) in
        (* drop trailing _foo_ *)
        let m =
          let ml = String.length m in
          if ml >= 2 && m.[ml - 1] = '_'
          then
            try String.sub m 0 (String.rindex_from m (ml - 2) '_')
            with Not_found -> String.sub m 0 (ml - 1)
          else m in
        begin
          match op, cargs with
            | `Get, [] -> << $co$.$m$ >>
            | `Set, [e] -> << $co$.$m$ = $e$ >>
            | `Call, es ->
                begin
                  match co with
                    | Jvar _ -> << $id:app$($exp:co$.$m$, $co$, [$list:es$]) >>
                    | _ ->
                        let i = jsident_of_ident (Ident.create "v") in
                        (* here we bind i to avoid multiply evaluating co *)
                        exp_of_stmts <:stmt<
                          var $id:i$ = $co$;
                          return $id:app$($id:i$.$m$, $id:i$, [$list:es$]);
                        >>
                end
            | _ -> raise (Failure "bad method call")
        end

    | Lsend (Public, m, o, args) ->
        (*
          I think this case (where we do not know the method name at
          the call site) only comes up in the compilation of
          camlinternalOO.ml, in the send_foo functions.
        *)
        let app = if tail then "__m" else "_m" in
        let cm = comp_expr false m in
        let co = comp_expr false o in
        let cargs = List.map (comp_expr false) args in
        begin
          match co with
            | Jvar _ -> << $id:app$($exp:co$[$cm$], $co$, [$list:cargs$]) >>
            | _ ->
                let i = jsident_of_ident (Ident.create "v") in
                (* here we bind i to avoid multiply evaluating co *)
                exp_of_stmts <:stmt<
                  var $id:i$ = $co$;
                  return $id:app$($id:i$[$cm$], $id:i$, [$list:cargs$]);
                >>
        end

    | Lsend (Self, m, o, args) ->
        let inh =
          match m with
            | Lvar id ->
                (* see hack in typecore.ml *)
                let flags_field = 2 in
                let repr = Obj.repr id in
                let flags = (Obj.obj (Obj.field repr flags_field)) in
                flags land 4 > 0
            | _ -> false in
        let app = if tail then "__m" else "_m" in
        let cm = comp_expr false m in
        let co = comp_expr false o in
        let cargs = List.map (comp_expr false) args in
        if inh
        then << $id:app$($cm$, $co$, [$list:cargs$]) >>
        else << $id:app$($co$._m[$cm$], $co$, [$list:cargs$]) >>

    | _ -> unimplemented "comp_expr" expr

(* compile a lambda as a Js.stmt list *)
(* tail is true if the expression is in tail position *)
(* k is called on the AST of exps in tail position *)
and comp_expr_st tail expr k =
  match expr with
    | (Llet _ | Lletrec _) -> comp_letrecs_st tail expr k

    | Lfor (i, e1, e2, d, e3) ->
        let i = jsident_of_ident i in
        let jv = << $id:i$ >> in
        let ce1 = comp_expr false e1
        and ce2 = comp_expr false e2
        and ce3 = comp_expr_st false e3 keffect in
        let (te, ie) =
          match d with
            | Upto -> << $jv$ <= $ce2$ >>, << $jv$++ >>
            | Downto -> << $jv$ >= $ce2$ >>, << $jv$-- >> in
        (* wrap loop body in a function / call so closures over loop var work *)
        Jfor (_loc,
              [ i, Some ce1 ],
              None,
              Some te,
              Some ie,
              Jblock(_loc,
                     Jexps (_loc,
                            Jcall(_loc,
                                  Jfun(_loc, None, [i], ce3),
                                  jv))))

    | Lwhile (e1, e2) ->
        Jwhile (_loc, comp_expr false e1, comp_expr_st false e2 keffect)

    (*
      special case some constructs that arise from the compilation of pattern matching,
      to avoid deep nesting in generated Javascript
    *)
    | Lifthenelse (i, t, (Lstaticraise _ as e)) ->
        <:stmt<
          $Jites (_loc,
               << !$comp_expr false i$ >>,
               (comp_expr_st tail e k),
               None)$
          $comp_expr_st tail t k$
        >>
    | Lifthenelse (i, (Lstaticraise _ as t), e) ->
        <:stmt<
          $Jites (_loc,
               comp_expr false i,
               (comp_expr_st tail t k),
               None)$
          $comp_expr_st tail e k$
        >>

    | Lifthenelse (i, (Lifthenelse _ as t), e) when k == kreturn ->
        <:stmt<
          $Jites (_loc,
               << !$comp_expr false i$ >>,
               (comp_expr_st tail e k),
               None)$
          $comp_expr_st tail t k$
        >>

    | Lifthenelse (i, t, e) when k == kreturn ->
        <:stmt<
          $Jites (_loc,
               comp_expr false i,
               (comp_expr_st tail t k),
               None)$
          $comp_expr_st tail e k$
        >>

    | Lifthenelse (i, t, e) ->
        Jites (_loc,
              comp_expr false i,
              (comp_expr_st tail t k),
              Some (comp_expr_st tail e k))

    | Lswitch (se,
               { sw_numconsts = nc; sw_consts = cs;
                 sw_numblocks = nb; sw_blocks = bs;
                 sw_failaction = fe }) ->
        (* we don't want to evaluate the scrutinee more than once: if
           it is already a var leave it alone, otherwise bind a var *)
        let (k2, cse) =
          match se with
            | Lvar i -> ((fun x -> x), << $id:jsident_of_ident i$ >>)
            | _ ->
                let i = jsident_of_ident (Ident.create "s") in
                let cse = comp_expr false se in
                ((fun x -> <:stmt< $Jvars (_loc, [ i, Some cse ])$ $x$ >>), << $id:i$ >>) in
        let cc (i, e) =
          (* true if the sequence returns or throws; otherwise we need a break *)
          let rec exits = function
            | Jblock (_, ss) -> exits ss
            | Jbreak (_, Some _) -> true
            | Jreturn _ -> true
            | Jthrow _ -> true
            | Jites (_, _, t, Some e) -> exits t && exits e
            | Jstmt_cons _ as ss ->
                let rec stmt_last = function
                  | Jstmt_cons (_, _, s2) -> stmt_last s2
                  | s -> s in
                exits (stmt_last ss)
            | _ -> false in
          let i = << $`int:i$ >> in
          let stmts = comp_expr_st tail e k in
          let stmts =
            if exits stmts
            then stmts
            else <:stmt< $stmts$ break; >> in
        (i, stmts) in
        let fss = match fe with None -> k << null >> | Some e -> comp_expr_st tail e k in
        let cswitch = Jswitch (_loc, cse, List.map cc cs, fss) in
        let bswitch = Jswitch (_loc, << $$t($exp:cse$) >>, List.map cc bs, fss) in
        let stmt =
          if nc = 0 && nb = 0 then assert false
          else if nc = 0 then bswitch
          else if nb = 0 then cswitch
          else Jites (_loc,
                     << typeof $cse$ == 'number' >>,
                     cswitch,
                     Some bswitch) in
        k2 stmt

    | Lsequence (e1, e2) ->
        <:stmt<
          $comp_expr_st false e1 keffect$
          $comp_expr_st tail e2 k$
        >>

    | Lprim (Praise, [e]) -> Jthrow (_loc, comp_expr false e)

    | Lprim (Pignore, [e]) ->
        comp_expr_st tail (Lsequence (e, Lconst (Const_pointer 0))) k

    | Lprim (Pccall { prim_name = "$inline_stmt" }, [e]) -> inline_stmt e
    | Lprim (Pccall { prim_name = "$inline_rstmt" }, [e]) -> inline_stmt e

    | Lstaticcatch (e1, (lab, args), e2) ->
        (* The raised flag indicates whether e1 exits normally or via
           a static-raise. This variable can be eliminated with
           the introduction of another block label, which would also double
           the nesting depth. *)
        let raised = flag_raised lab in
        (* e2 expects the names of its arguments as named in args. Here we use
           raise_args, a set of variables whose names are derived predictably
           from the label, to pass the argument values from the static-raise
           to e2. We could either eliminate these raise_args, by adding
           a contextual parameter to comp_expr(_st) so that the static-raise
           knows the names of args, or eliminate the args in e2, by
           alpha-renaming them to raise_args. *)
        let rec with_raise_arg i l =
          if i <= 0 then l
          else with_raise_arg (i-1) (<:stmt< $Jvars(_loc, [ raise_arg lab (i-1), Some << null >> ])$ $l$ >>) in
        let _, dest_raise_args =
          List.fold_left
            (fun (i,l) v ->
              i+1, <:stmt< $Jvars (_loc, [ jsident_of_ident v, Some << $id:raise_arg lab i$ >> ])$ $l$ >>)
            (0,<:stmt< >>) args in
        with_raise_arg (List.length args)
          <:stmt<
            $Jvars (_loc, [ raised, Some (Jbool (_loc, false)) ])$
            $Jlabel (_loc, label_raise lab, comp_expr_st tail e1 k)$
            $Jites (_loc,
                  << $id:raised$ >>,
                  (stmt_rev_append dest_raise_args (comp_expr_st tail e2 k)),
                  None)$
          >>

    | Lstaticraise (lab, args) ->
        let _, cons_raise_args =
          List.fold_left
            (fun (i,l) v ->
              i+1, <:stmt< $id:raise_arg lab i$ = $comp_expr false v$; $l$>>)
            (0, <:stmt< >>) args in
        stmt_rev_append cons_raise_args
          <:stmt<
            $id:flag_raised lab$ = true;
            break $label_raise lab$;
          >>

    | Ltrywith (e1, i, e2) ->
        Jtrycatch (_loc,
                  comp_expr_st false e1 k,
                  Some (jsident_of_ident i, comp_expr_st tail e2 k),
                  <:stmt< >>)

    | _ -> k (comp_expr tail expr)

and backpatch bs =
  let rec range from upto =
    if from = upto
    then []
    else from::range (from +1) upto in
  let rec bp path e bps =
    match e with
      | Lvar id ->
          if List.mem_assoc id bs
          then <:stmt< $exp:path$ = $id:jsident_of_ident id$; $bps$ >>
          else bps
      | Lprim (Pmakeblock _, args) ->
          List.fold_right2
            (fun i e bps -> bp << $path$[$`int:i$] >> e bps)
            (range 0 (List.length args))
            args
            bps
      | _ -> bps in
  List.fold_right (fun (id, e) bps -> bp << $id:jsident_of_ident id$ >> e bps) bs <:stmt< >>

(* compile nested let/letrecs into a Js.stmt list *)
(* k is called on the AST of exps in tail position *)
and comp_letrecs_st tail expr k =
  let rec cl expr =
    match expr with
      | Llet (_, i, e1, e2) ->
          <:stmt<
            var $id:jsident_of_ident i$ = $comp_expr false e1$;
            $cl e2$
          >>
      | Lletrec (bs, e) ->
          let cb (id, e) = <:stmt< var $id:jsident_of_ident id$ = $comp_expr false e$; >> in
          <:stmt<
            $list:List.map cb bs$
            $backpatch bs$
            $cl e$
          >>
      | e -> comp_expr_st tail e k in
   cl expr

and inline_exp = function
    (* XXX actually we never get these because of the _loc arg *)
  | Lconst (Const_block _) as cb -> inline_exp (makeblock_of_const cb)
  | <:lam_exp< this >> -> <:exp< this >>
  | <:lam_exp< $id:v$ >> -> <:exp< $id:inline_string v$ >>
  | <:lam_exp< [ $el$ ] >> -> <:exp< [ $inline_exp el$ ] >>
  | <:lam_aexp< Jobject ($_$, $kvl$) >> -> Jobject (_loc, inline_list (inline_pair inline_exp inline_exp) kvl)
(*
  | <:lam_exp< $str:s$ >> -> <:exp< $str:s$ >>
*) (* quote flag is not accessible *)
  | <:lam_aexp< Jstring ($_$, $s$, $qq$) >> -> Jstring (_loc, inline_string s, inline_bool qq)
  | <:lam_exp< $flo:s$ >> -> <:exp< $flo:inline_string s$ >> (* XXX :num ? *)
  | <:lam_exp< null >> -> <:exp< null >>
  | <:lam_aexp< Jbool ($_$, $b$) >> -> Jbool (_loc, inline_bool b) (* XXX :bool ? *)
  | <:lam_aexp< Jregexp ($_$, $re$, $flags$) >> -> Jregexp (_loc, inline_string re, inline_string flags)
  | <:lam_aexp< Jfun ($_$, $so$, $sl$, $stl$) >> ->
      Jfun (_loc,
           inline_option inline_string so,
           inline_list inline_string sl,
           inline_stmt stl)
  | <:lam_exp< $e$.$s$ >> -> <:exp< $inline_exp e$.$inline_string s$ >>
  | <:lam_aexp< Junop ($_$, $u$, $e$) >> -> Junop (_loc, inline_unop u, inline_exp e)
  | <:lam_aexp< Jbinop ($_$, $b$, $e1$, $e2$) >> -> Jbinop (_loc, inline_binop b, inline_exp e1, inline_exp e2)
  | <:lam_exp< $i$ ? $t$ : $e$ >> -> <:exp< $inline_exp i$ ? $inline_exp t$ : $inline_exp e$ >>
  | <:lam_exp< $e$($el$) >> -> <:exp< $inline_exp e$($inline_exp el$) >>
  | <:lam_aexp< Jnew ($_$, $e$, $elo$) >> -> Jnew (_loc, inline_exp e, inline_option inline_exp elo)
  | <:lam_aexp< Jexp_nil $_$ >> -> Jexp_nil _loc
  | <:lam_aexp< Jexp_cons ($_$, $e1$, $e2$) >> -> Jexp_cons (_loc, inline_exp e1, inline_exp e2)
  | Lprim (Pccall { prim_name = "$inline_antiexp" }, [e]) -> comp_expr false e
  | _ -> raise (Failure "bad inline exp")

and inline_variableDeclarationList seol = inline_list (inline_pair inline_string (inline_option inline_exp)) seol

and inline_stmt = function
  | Lconst (Const_block _) as cb -> inline_stmt (makeblock_of_const cb)
  | <:lam_astmt< Jvars ($_$, $seol$) >> ->
      Jvars (_loc, inline_variableDeclarationList seol)
  | <:lam_astmt< Jfuns ($_$, $s$, $sl$, $stl$) >> ->
      Jfuns (_loc, inline_string s, inline_list inline_string sl, inline_stmt stl)
  | <:lam_astmt< Jreturn ($_$, $eo$) >> -> Jreturn (_loc, inline_option inline_exp eo)
  | <:lam_astmt< Jcontinue ($_$, $so$) >> -> Jcontinue (_loc, inline_option inline_string so)
  | <:lam_astmt< Jbreak ($_$, $so$) >> -> Jbreak (_loc, inline_option inline_string so)
  | <:lam_astmt< Jswitch ($_$, $e$, $esll$, $slo$) >> ->
      Jswitch (_loc,
              inline_exp e,
              inline_list (inline_pair inline_exp inline_stmt) esll,
              inline_stmt slo)
  | <:lam_astmt< Jites ($_$, $e$, $s$, $so$) >> -> Jites (_loc, inline_exp e, inline_stmt s, inline_option inline_stmt so)
  | <:lam_astmt< Jthrow ($_$, $e$) >> -> Jthrow (_loc, inline_exp e)
  | <:lam_astmt< Jexps ($_$, $e$) >> -> Jexps (_loc, inline_exp e)
  | <:lam_astmt< Jtrycatch ($_$, $sl1$, $sslpo$, $sl2$) >> ->
      Jtrycatch (_loc,
                inline_stmt sl1,
                inline_option (inline_pair inline_string inline_stmt) sslpo,
                inline_stmt sl2)
  | <:lam_astmt< Jfor ($_$, $vars$, $eo1$, $eo2$, $eo3$, $s$) >> ->
      Jfor (_loc,
           inline_variableDeclarationList vars,
           inline_option inline_exp eo1,
           inline_option inline_exp eo2,
           inline_option inline_exp eo3,
           inline_stmt s)
  | <:lam_astmt< Jdowhile ($_$, $s$, $e$) >> -> Jdowhile (_loc, inline_stmt s, inline_exp e)
  | <:lam_astmt< Jwhile ($_$, $e$, $s$) >> -> Jwhile (_loc, inline_exp e, inline_stmt s)
  | <:lam_astmt< Jblock ($_$, $sl$) >> -> Jblock (_loc, inline_stmt sl)
  | <:lam_astmt< Jwith ($_$, $e$, $s$) >> -> Jwith (_loc, inline_exp e, inline_stmt s)
  | <:lam_astmt< Jlabel ($_$, $s$, $st$) >> -> Jlabel (_loc, inline_string s, inline_stmt st)
  | <:lam_astmt< Jstmt_nil $_$ >> -> Jstmt_nil _loc
  | <:lam_astmt< Jstmt_cons ($_$, $s1$, $s2$) >> -> Jstmt_cons (_loc, inline_stmt s1, inline_stmt s2)
  | _ -> raise (Failure "bad inline stmt")

and inline_unop = function
  | <:lam_aunop< Jdelete >> -> Jdelete
  | <:lam_aunop< Jvoid >> -> Jvoid
  | <:lam_aunop< Jtypeof >> -> Jtypeof
  | <:lam_aunop< Jadd2_pre >> -> Jadd2_pre
  | <:lam_aunop< Jsub2_pre >> -> Jsub2_pre
  | <:lam_aunop< Jadd_pre >> -> Jadd_pre
  | <:lam_aunop< Jsub_pre >> -> Jsub_pre
  | <:lam_aunop< Jtilde >> -> Jtilde
  | <:lam_aunop< Jnot >> -> Jnot
  | <:lam_aunop< Jadd2_post >> -> Jadd2_post
  | <:lam_aunop< Jsub2_post >> -> Jsub2_post
  | _ -> raise (Failure "bad inline unop")

and inline_binop = function
  | <:lam_abinop< Jhashref >> -> Jhashref
  | <:lam_abinop< Jmul >> -> Jmul
  | <:lam_abinop< Jdiv >> -> Jdiv
  | <:lam_abinop< Jmod >> -> Jmod
  | <:lam_abinop< Jadd >> -> Jadd
  | <:lam_abinop< Jsub >> -> Jsub
  | <:lam_abinop< Jlt >> -> Jlt
  | <:lam_abinop< Jgt >> -> Jgt
  | <:lam_abinop< Jleq >> -> Jleq
  | <:lam_abinop< Jgeq >> -> Jgeq
  | <:lam_abinop< Jlsr >> -> Jlsr
  | <:lam_abinop< Jlsl >> -> Jlsl
  | <:lam_abinop< Jasr >> -> Jasr
  | <:lam_abinop< Jeq >> -> Jeq
  | <:lam_abinop< Jneq >> -> Jneq
  | <:lam_abinop< Jinstanceof >> -> Jinstanceof
  | <:lam_abinop< Jseq >> -> Jseq
  | <:lam_abinop< Jsneq >> -> Jsneq
  | <:lam_abinop< Jland >> -> Jland
  | <:lam_abinop< Jlor >> -> Jlor
  | <:lam_abinop< Jand >> -> Jand
  | <:lam_abinop< Jxor >> -> Jxor
  | <:lam_abinop< Jor >> -> Jor
  | <:lam_abinop< Jcomma >> -> Jcomma
  | <:lam_abinop< Jassign >> -> Jassign
  | <:lam_abinop< Jmul_assign >> -> Jmul_assign
  | <:lam_abinop< Jdiv_assign >> -> Jdiv_assign
  | <:lam_abinop< Jmod_assign >> -> Jmod_assign
  | <:lam_abinop< Jadd_assign >> -> Jadd_assign
  | <:lam_abinop< Jsub_assign >> -> Jsub_assign
  | <:lam_abinop< Jlsl_assign >> -> Jlsl_assign
  | <:lam_abinop< Jlsr_assign >> -> Jlsr_assign
  | <:lam_abinop< Jasr_assign >> -> Jasr_assign
  | <:lam_abinop< Jand_assign >> -> Jand_assign
  | <:lam_abinop< Jxor_assign >> -> Jxor_assign
  | <:lam_abinop< Jor_assign >> -> Jor_assign
  | _ -> raise (Failure "bad inline binop")

(**** Compilation of a lambda phrase ****)

let compile_implementation modulename expr =
  let ce =
    match expr with
      | Lprim (Psetglobal id, [e]) ->
          enter_setglobal id;
          <:stmt< var $id:jsident_of_ident id$ = $comp_expr false e$; >>
      | _ -> unimplemented "compile_implementation" expr in
  let ret = (ce, !reloc_info) in
  reloc_info := [];
  ret
