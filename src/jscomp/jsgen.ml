(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
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

let jnum_of_int i = Jslib_ast.Jnum (_loc, string_of_int i) (* XXX <:exp $`int:i$ >> *)

let makeblock tag ces =
  match tag with
    | 0 -> let id = "$" in << $id:id$($ces$) >>
    | (1|2|3|4|5|6|7|8|9) -> let id = "$" ^ string_of_int tag in << $id:id$($ces$) >>
    | _ -> let id = "$N" in << $id:id$($exp:jnum_of_int tag$, [$ces$]) >>

let exp_of_stmts ss = << $Jslib_ast.Jfun (_loc, None, [], ss)$ () >>

let comp_const c =
  match c with
    | Const_int i -> jnum_of_int i
    | Const_char c -> jnum_of_int (Char.code c)
    | Const_string s -> Jslib_ast.Jstring (_loc, s, false)
    | Const_float s -> Jslib_ast.Jnum (_loc, s) (* XXX different float syntax *)
    | Const_int32 i32 -> Jslib_ast.Jnum (_loc, Int32.to_string i32)
    | Const_int64 i64 -> Jslib_ast.Jstring (_loc, Int64.to_string i64, false)
    | Const_nativeint ni -> Jslib_ast.Jnum (_loc, Nativeint.to_string ni)

let rec comp_sconst c =
  match c with
    | Const_base c -> comp_const c
    | Const_pointer i -> jnum_of_int i
    | Const_block (tag, cs) ->
	makeblock tag (List.map comp_sconst cs)
    | Const_float_array ss ->
	makeblock 0 (List.map (fun s -> Jslib_ast.Jnum (_loc, s)) ss) (* XXX different float syntax *)
    | Const_immstring s -> Jslib_ast.Jstring (_loc, s, false) (* XXX when does this happen? *)

let kreturn e = <:stmt< return $e$; >>

let keffect = function
    (* anything that is already a value can have no effect *)
    (* mostly this arises with the compilation of () as Jnum 0 *)
  | (Jslib_ast.Jnum _ | Jslib_ast.Jvar _) -> Jslib_ast.Jempty _loc
  | e -> Jslib_ast.Jexps (_loc, e)

let comp_ccall c es =
  match c, es with
    | ("caml_int32_format", _ | "caml_nativeint_format", _ | "caml_int64_format", _) -> << caml_format_int($es$) >>
    | "caml_format_float", _ -> let id = "oc$$sprintf" in << $id:id$($es$) >>
    | "caml_string_equal", _ -> let id = "oc$$seq" in << $id:id$($es$) >>
    | "caml_string_notequal", _ -> let id = "oc$$sneq" in << $id:id$($es$) >>
    | "caml_string_lessthan", _ -> let id = "oc$$slt" in << $id:id$($es$) >>
    | "caml_string_greaterthan", _ -> let id = "oc$$sgt" in << $id:id$($es$) >>
    | "caml_string_lessequal", _ -> let id = "oc$$slte" in << $id:id$($es$) >>
    | "caml_string_greaterequal", _ -> let id = "oc$$sgte" in << $id:id$($es$) >>
    | "caml_create_string", _ -> let id = "oc$$cms" in << $id:id$($es$) >>

    | "caml_power_float", _ -> << Math.pow($es$) >>
    | "caml_exp_float", _ -> << Math.exp($es$) >>
    | "caml_acos_float", _ -> << Math.acos($es$) >>
    | "caml_asin_float", _ -> << Math.asin($es$) >>
    | "caml_atan_float", _ -> << Math.atan($es$) >>
    | "caml_atan2_float", _ -> << Math.atan2($es$) >>
    | "caml_cos_float", _ -> << Math.cos($es$) >>
    (* | "caml_cosh_float", _ -> ? *)
    | "caml_log_float", _ -> << Math.log($es$) >>
    (* | "caml_log10_float", _ -> ? *)
    | "caml_sin_float", _ -> << Math.sin($es$) >>
    (* | "caml_sinh_float", _ -> ? *)
    | "caml_sqrt_float", _ -> << Math.sqrt($es$) >>
    | "caml_tan_float", _ -> << Math.tan($es$) >>
    (* | "caml_tanh_float", _ -> ? *)
    | "caml_ceil_float", _ -> << Math.ceil($es$) >>
    | "caml_floor_float", _ -> << Math.floor($es$) >>
    | "caml_abs_float", _ -> << Math.abs($es$) >>

    | "$assign", [e1; e2] -> << $e1$ = $e2$ >>
    | "$call", e::es -> << $e$($es$) >>
    | "$false", _ -> << false >>
    | "$fieldref", [e; Jslib_ast.Jstring (_loc, id, _)] -> << $e$.$id$ >>
    | "$function", [Jslib_ast.Jcall (_loc, Jslib_ast.Jvar _, Jslib_ast.Jexp_list (_, [Jslib_ast.Jfun _ as f]))] -> f
    | "$hashref", [e1; e2] -> << $e1$[$e2$] >>
    | "$new", (Jslib_ast.Jstring (_, id, _))::es -> << new $id:id$($es$) >>
    | "$null", _ -> << null >>
    | "$this", _ -> << this >>
    | "$throw", [e] -> exp_of_stmts [ <:stmt< throw $e$; >> ]
    | "$true", _ -> << true >>

    | "$var", [Jslib_ast.Jstring (_loc, id, _)] -> << $id:id$ >>

    | "$obj", [e] ->
	let rec o e l =
	  match e with
	    | Jslib_ast.Jcall (_, Jslib_ast.Jvar _, Jslib_ast.Jexp_list (_, [ Jslib_ast.Jcall (_, Jslib_ast.Jvar _, Jslib_ast.Jexp_list (_, [Jslib_ast.Jstring _ as k; v])); e ])) -> o e ((k, v)::l)
	    | Jslib_ast.Jnum _ -> List.rev l
	    | _ -> raise (Unimplemented "bad $obj") in
	Jslib_ast.Jobject (_loc, o e [])

    | _ ->
	match c.[0], es with
	  | '#', e::es -> let met = String.sub c 1 (String.length c - 1) in << $e$.$met$($es$) >>
	  | '.', [e] -> let fld = String.sub c 1 (String.length c - 1) in << $e$.$fld$ >>
	  | '=', [e1; e2] -> let fld = String.sub c 1 (String.length c - 1) in << $e1$.$fld$ = $e2$ >>
	  | '@', _ -> let id = String.sub c 1 (String.length c - 1) in << $id:id$($es$) >>
	  | _ -> enter_c_prim c; << $id:c$($es$) >>

let comp_comparison c e1 e2 =
  match c with
    | Ceq -> << $e1$ == $e2$ >>
    | Cneq -> << $e1$ != $e2$ >>
    | Clt -> << $e1$ < $e2$ >>
    | Cgt -> << $e1$ > $e2$ >>
    | Cle -> << $e1$ <= $e2$ >>
    | Cge -> << $e1$ >= $e2$ >>

let comp_prim p es =
  match p, es with
    | Pgetglobal i, [] -> enter_getglobal i; << $id:jsident_of_ident i$ >>
    | Pmakeblock (tag, _), _ -> makeblock tag es

    | (Pfield i, [e] | Pfloatfield i, [e]) -> << $e$[$jnum_of_int i$] >>
    | (Psetfield (i, _), [e1; e2] | Psetfloatfield i, [e1; e2]) -> << $e1$[$jnum_of_int i$] = $e2$ >>
    | Pccall { prim_name = "$new"; prim_native_name = "" }, es -> comp_ccall "$new" es
    | Pccall { prim_name = "$new"; prim_native_name = id }, es -> << new $id:id$($es$) >>
    | Pccall { prim_name = n }, es -> comp_ccall n es
    | Pisout, [h; e] -> << $e$ < 0 || $e$ > $h$ >> (* XXX bind e to var? *)
    | Pabsfloat, [e] -> << Math.abs($exp:e$) >>

    | (Pintcomp c, [e1; e2] | Pbintcomp (_, c), [e1; e2] | Pfloatcomp c, [e1; e2]) ->
        comp_comparison c e1 e2

    | (Pnegint, [e] | Pnegbint _, [e] | Pnegfloat, [e]) -> << -$e$ >>
    | (Paddint, [e1; e2] | Paddbint _, [e1; e2] | Paddfloat, [e1; e2]) -> << $e1$ + $e2$ >>
    | (Psubint, [e1; e2] | Psubbint _, [e1; e2] | Psubfloat, [e1; e2]) -> << $e1$ - $e2$ >>
    | (Pmulint, [e1; e2] | Pmulbint _, [e1; e2] | Pmulfloat, [e1; e2]) -> << $e1$ * $e2$ >>
    | (Pdivint, [e1; e2] | Pdivbint _, [e1; e2] | Pdivfloat, [e1; e2]) ->
        (* XXX << ($e1$ / $e2$) < < 0 >> *)
        Jslib_ast.Jbinop(_loc,
                        Jslib_ast.Jlsr,
                        << $e1$ / $e2$ >>,
                        << 0 >>)
    | (Pmodint, [e1; e2] | Pmodbint _, [e1; e2]) -> << $e1$ % $e2$ >>

    | (Plslint, [e1; e2] | Plslbint _, [e1; e2]) -> Jslib_ast.Jbinop(_loc, Jslib_ast.Jlsl, e1, e2)
    | (Plsrint, [e1; e2] | Plsrbint _, [e1; e2]) -> Jslib_ast.Jbinop(_loc, Jslib_ast.Jlsr, e1, e2)
    | (Pasrint, [e1; e2] | Pasrbint _, [e1; e2]) -> Jslib_ast.Jbinop(_loc, Jslib_ast.Jasr, e1, e2)

    | (Pandint, [e1; e2] | Pandbint _, [e1; e2]) -> << $e1$ & $e2$ >>
    | (Porint, [e1; e2] | Porbint _, [e1; e2]) -> << $e1$ | $e2$ >>
    | (Pxorint, [e1; e2] | Pxorbint _, [e1; e2]) -> << $e1$ ^ $e2$ >>

    | Pnot, [e] -> << !$e$ >>
    | Psequand, [e1; e2] -> << $e1$ && $e2$ >>
    | Psequor, [e1; e2] -> << $e1$ || $e2$ >> (* XXX rhs is possibly a tail call *)

    | Poffsetint n, [e] -> << $jnum_of_int n$ + $e$ >>

    | Poffsetref 1, [e] -> << $e$[0]++ >>
    | Poffsetref -1, [e] -> << $e$[0]-- >>
    | Poffsetref n, [e] -> << $e$[0] = $jnum_of_int n$ + $e$[0] >> (* XXX bind e to var? *)

    | Pstringlength, [e] -> << $e$.length >>
    | Parraylength _, [e] -> << $e$.length >>

    | Pmakearray _, es -> makeblock 0 es

    | Pstringrefu, _ -> let id = "oc$$srefu" in << $id:id$($es$) >>
    | Pstringsetu, _ -> let id = "oc$$ssetu" in << $id:id$($es$) >>
    | Pstringrefs, _ -> let id = "oc$$srefs" in << $id:id$($es$) >>
    | Pstringsets, _ -> let id = "oc$$ssets" in << $id:id$($es$) >>

    | Parrayrefu _, [e1; e2] -> << $e1$[$e2$] >>
    | Parraysetu _, [e1; e2; e3] -> << $e1$[$e2$] = $e3$ >>
    | Parrayrefs _, _ -> let id = "oc$$arefs" in << $id:id$($es$) >>
    | Parraysets _, _ -> let id = "oc$$asets" in << $id:id$($es$) >>

    | Pisint, [e] -> << typeof $e$ == 'number' >>

    | (Pidentity, [e] | Pignore, [e] |
       Pfloatofint, [e] | Pintoffloat, [e] |
       Pintofbint _, [e] | Pbintofint _, [e] |
       Pcvtbint _, [e]) ->
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

let maybe_block ss =
  match ss with
    | [] -> Jslib_ast.Jempty _loc
    | [s] -> s
    | _ -> Jslib_ast.Jblock (_loc, ss)

let inline_string = function
  | Lconst (Const_base (Const_string s)) -> s
  | _ -> raise (Failure "bad inline string")

let inline_bool = function
  | Lconst (Const_pointer 0) -> false
  | Lconst (Const_pointer 1) -> true
  | _ -> raise (Failure "bad inline bool")

let makeblock_of_const = function
  | Lconst (Const_block (tag, cs)) ->
      Lprim (Pmakeblock (tag, Asttypes.Mutable), List.map (fun c -> Lconst c) cs)
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

(* compile a lambda as a Js.exp *)
(* tail is true if the expression is in tail position *)
let rec comp_expr tail expr =
  match expr with
    | (Llet _ | Lletrec _) -> exp_of_stmts (comp_letrecs_st tail expr kreturn)

    | (Lswitch _ | Lprim (Praise, _) | Lstaticcatch _ | Lstaticraise _ |
       Ltrywith _ | Lfor _ | Lwhile _) ->
	exp_of_stmts (comp_expr_st tail expr kreturn)

    | Lvar i -> << $id:jsident_of_ident i$ >>

    | Lfunction (_, args, e) ->
        let e = Jslib_ast.Jfun (_loc, None, List.map jsident_of_ident args, comp_expr_st true e kreturn) in
	<< _f($exp:e$) >>

    | Lapply(e, es) ->
	let app = if tail then "__" else "_" in
        let ce = comp_expr false e in
        let ces = List.map (comp_expr false) es in
        << $id:app$($exp:ce$, [$ces$]) >>

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

    | Lsend (_, Lconst(Const_immstring m), o, args) ->
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
            with Not_found -> m
          else m in
        begin
          match op, cargs with
            | `Get, [] -> << $co$.$m$ >>
            | `Set, [e] -> << $co$.$m$ = $e$ >>
            | `Call, es ->
                begin
                  match co with
                    | Jslib_ast.Jvar _ -> << $id:app$($exp:co$.$m$, $co$, [$es$]) >>
                    | _ ->
                        let i = jsident_of_ident (Ident.create "v") in
                        (* here we bind i to avoid multiply evaluating co *)
                        exp_of_stmts [
                          <:stmt< var $id:i$ = $co$; >>;
                          <:stmt< return $id:app$($id:i$.$m$, $id:i$, [$es$]); >>
                        ]
                end
            | _ -> raise (Failure "bad method call")
        end

    | Lsend _ -> << null >> (* XXX temporary *)

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
	[ Jslib_ast.Jfor (_loc,
                         Some << $id:i$ = $ce1$ >>,
                         Some te,
                         Some ie,
                         maybe_block ce3) ]

    | Lwhile (e1, e2) ->
	[ Jslib_ast.Jwhile (_loc, comp_expr false e1, maybe_block (comp_expr_st false e2 keffect)) ]

    (*
      special case some constructs that arise from the compilation of pattern matching,
      to avoid deep nesting in generated Javascript
    *)
    | Lifthenelse (i, t, (Lstaticraise _ as e)) ->
        (Jslib_ast.Jites (_loc,
                         << !$comp_expr false i$ >>,
                         maybe_block (comp_expr_st tail e k),
                         None)) :: (comp_expr_st tail t k)
    | Lifthenelse (i, (Lstaticraise _ as t), e) ->
	(Jslib_ast.Jites (_loc,
                         comp_expr false i,
                         maybe_block (comp_expr_st tail t k),
                         None)) :: (comp_expr_st tail e k)

    | Lifthenelse (i, t, e) ->
	[ Jslib_ast.Jites (_loc,
                          comp_expr false i,
                          maybe_block (comp_expr_st tail t k),
                          Some (maybe_block (comp_expr_st tail e k))) ]

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
		((fun x -> Jslib_ast.Jvars (_loc, [ i, Some cse ]) :: x), << $id:i$ >>) in
	let cc (i, e) =
          (* true if the sequence returns or throws; otherwise we need a break *)
          let rec exits stmts =
            match stmts with
              | Jslib_ast.Jempty _ -> false
              | Jslib_ast.Jblock (_, ss) -> exits (List.nth ss (List.length ss - 1))
              | Jslib_ast.Jbreak (_, Some _) -> true
              | Jslib_ast.Jreturn _ -> true
              | Jslib_ast.Jthrow _ -> true
              | Jslib_ast.Jites (_, _, t, Some e) -> exits t && exits e
              | _ -> false in
          let i = jnum_of_int i in
          let stmts = comp_expr_st tail e k in
          let stmts =
            if exits (maybe_block stmts)
            then stmts
            else stmts @ [ <:stmt< break; >> ] in
        (i, stmts) in
	let fss = match fe with None -> Some [k << null >> ] | Some e -> Some (comp_expr_st tail e k) in
	let cswitch = Jslib_ast.Jswitch (_loc, cse, List.map cc cs, fss) in
	let bswitch = Jslib_ast.Jswitch (_loc, (let id = "$t" in << $id:id$($exp:cse$) >>), List.map cc bs, fss) in
	let stmt =
	  if nc = 0 && nb = 0 then Jslib_ast.Jempty _loc (* shouldn't happen *)
	  else if nc = 0 then bswitch
	  else if nb = 0 then cswitch
	  else Jslib_ast.Jites (_loc,
                               << typeof $cse$ == 'number' >>,
                               cswitch,
                               Some bswitch) in
	k2 [ stmt ]

    | Lsequence (e1, e2) ->
        comp_expr_st false e1 keffect @ comp_expr_st tail e2 k

    | Lprim (Praise, [e]) -> [ Jslib_ast.Jthrow (_loc, comp_expr false e) ]

    | Lprim (Pignore, [e]) -> comp_expr_st false e keffect

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
          else with_raise_arg (i-1) (Jslib_ast.Jvars(_loc, [ raise_arg lab (i-1), Some << null >> ])::l) in
        let _, dest_raise_args =
          List.fold_left
            (fun (i,l) v ->
              i+1, Jslib_ast.Jvars (_loc, [ jsident_of_ident v, Some << $id:raise_arg lab i$ >> ]) :: l)
            (0,[]) args in
        with_raise_arg (List.length args)
          [ Jslib_ast.Jvars (_loc, [ raised, Some (Jslib_ast.Jbool (_loc, false)) ]);
            Jslib_ast.Jlabel (_loc, label_raise lab, maybe_block (comp_expr_st tail e1 k));
            Jslib_ast.Jites (_loc,
                            << $id:raised$ >>,
	                    maybe_block (List.rev_append dest_raise_args (comp_expr_st tail e2 k)),
                            None) ]

    | Lstaticraise (lab, args) ->
        let _, cons_raise_args =
          List.fold_left
            (fun (i,l) v ->
              i+1, <:stmt< $id:raise_arg lab i$ = $comp_expr false v$; >> :: l)
            (0, []) args in
        List.rev_append cons_raise_args
          [ <:stmt< $id:flag_raised lab$ = true; >>;
            <:stmt< break $label_raise lab$; >> ]

    | Ltrywith (e1, i, e2) ->
	[ Jslib_ast.Jtrycatch (_loc,
                              comp_expr_st false e1 k,
                              jsident_of_ident i,
                              comp_expr_st tail e2 k) ]

    | _ -> [ k (comp_expr tail expr) ]

and backpatch bs =
  let rec range from upto =
    if from = upto
    then []
    else from::range (from +1) upto in
  let rec bp path e bps =
    match e with
      | Lvar id ->
          if List.mem_assoc id bs
          then <:stmt< $path$ = $id:jsident_of_ident id$; >>::bps
          else bps
      | Lprim (Pmakeblock _, args) ->
          List.fold_right2
            (fun i e bps -> bp << $path$[$jnum_of_int i$] >> e bps)
            (range 0 (List.length args))
            args
            bps
      | _ -> bps in
  List.fold_right (fun (id, e) bps -> bp << $id:jsident_of_ident id$ >> e bps) bs []

(* compile nested let/letrecs into a Js.stmt list *)
(* k is called on the AST of exps in tail position *)
and comp_letrecs_st tail expr k =
  let rec cl expr =
    match expr with
      | Llet (_, i, e1, e2) -> <:stmt< var $id:jsident_of_ident i$ = $comp_expr false e1$; >> :: cl e2
      | Lletrec (bs, e) ->
	  let cb (id, e) = <:stmt< var $id:jsident_of_ident id$ = $comp_expr false e$; >> in
	  List.map cb bs @ backpatch bs @ cl e
      | e -> comp_expr_st tail e k in
   cl expr

(* XXX annoying, would be nice to Camlp4-generate this *)
and inline_exp = function
    (* XXX actually we never get these because of the _loc arg *)
  | Lconst (Const_block _) as cb -> inline_exp (makeblock_of_const cb)

  | Lprim (Pmakeblock (tag, _), args) ->
      begin
        match tag, args with
          | 0, [_] -> Jthis _loc
          | 1, [_; v] -> Jvar (_loc, inline_string v)
          | 2, [_; el] -> Jarray (_loc, inline_exp_list el)
          | 3, [_; kvs] ->
              let rec inline_kv = function
                | Lconst (Const_block _) as cb -> inline_kv (makeblock_of_const cb)
                | Lprim (Pmakeblock (0, _), [k; v]) -> (inline_exp k, inline_exp v)
                | _ -> raise (Failure "bad inline kv") in
              Jobject (_loc, inline_list inline_kv kvs)
          | 4, [_; s; qq] -> Jstring (_loc, inline_string s, inline_bool qq)
          | 5, [_; s] -> Jnum (_loc, inline_string s)
          | 6, [_] -> Jnull _loc
          | 7, [_; b] -> Jbool (_loc, inline_bool b)
          | 8, [_; so; sl; stl] ->
              Jfun (_loc,
                   inline_option inline_string so,
                   inline_list inline_string sl,
                   inline_list inline_stmt stl)
          | 9, [_; e; s] -> Jfieldref (_loc, inline_exp e, inline_string s)
          | 10, [_; u; e] -> Junop (_loc, inline_unop u, inline_exp e)
          | 11, [_; b; e1; e2] -> Jbinop (_loc, inline_binop b, inline_exp e1, inline_exp e2)
          | 12, [_; i; t; e] -> Jite (_loc, inline_exp i, inline_exp t, inline_exp e)
          | 13, [_; e; el] -> Jcall (_loc, inline_exp e, inline_exp_list el)
          | 14, [_; e; elo] -> Jnew (_loc, inline_exp e, inline_option inline_exp_list elo)
          | _ -> raise (Failure "bad inline exp")
      end

  | Lprim (Pccall { prim_name = "$inline_antiexp" }, [e]) -> comp_expr false e

  | _ -> raise (Failure "bad inline exp")

and inline_exp_list = function
  | Lconst (Const_block _) as cb -> inline_exp_list (makeblock_of_const cb)
  | Lprim (Pmakeblock (0, _), [_; el]) -> Jexp_list (_loc, inline_list inline_exp el)
  | _ -> raise (Failure "bad inline exp_list")

and inline_stmt = function
  | _ -> raise (Failure "bad inline stmt")

and inline_unop = function
  | Lconst (Const_pointer tag) ->
      let unops = [|
        Jdelete; Jvoid; Jtypeof; Jadd2_pre; Jsub2_pre; Jadd_pre; Jsub_pre; Jtilde; Jnot; Jadd2_post; Jsub2_post
      |] in
      if tag < Array.length unops
      then unops.(tag)
      else raise (Failure "bad inline unop")
  | _ -> raise (Failure "bad inline unop")

and inline_binop = function
  | Lconst (Const_pointer tag) ->
      let binops = [|
        Jhashref; Jmul; Jdiv; Jmod; Jadd; Jsub; Jlt; Jgt; Jleq; Jgeq; Jlsr; Jlsl; Jasr; Jeq; Jneq; Jinstanceof; Jseq;
        Jsneq; Jland; Jlor; Jand; Jxor; Jor; Jcomma; Jassign; Jmul_assign; Jdiv_assign; Jmod_assign; Jadd_assign; Jsub_assign;
        Jlsl_assign; Jlsr_assign; Jasr_assign; Jand_assign; Jxor_assign; Jor_assign
      |] in
      if tag < Array.length binops
      then binops.(tag)
      else raise (Failure "bad inline binop")
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
