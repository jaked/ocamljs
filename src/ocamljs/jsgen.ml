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
open Js
open Cmo_format (* for reloc stuff *)

exception Unimplemented of string

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

let makeblock tag ces =
  match tag with
    | 0 -> jcall "$" ces
    | (1|2|3|4|5|6|7|8|9) -> jcall ("$" ^ string_of_int tag) ces
    | _ -> jcall "$N" [jnum_of_int tag; Jarray ces]

let makexblock tag ces =
  match ces with
    | [] -> jcall "$xM" [jnum_of_int tag]
    | _ -> jcall "$xN" [jnum_of_int tag; Jarray ces]

let exp_of_stmts ss = Jcall (Jfun ([], ss), [])

let comp_const c =
  match c with
    | Const_int i -> jnum_of_int i
    | Const_char c -> jnum_of_int (Char.code c)
    | Const_string s -> Jstring s
    | Const_float s -> Jnum (float_of_string s)
    | Const_int32 i32 -> Jnum (Int32.to_float i32)
    | Const_int64 i64 -> Jstring (Int64.to_string i64)
    | Const_nativeint ni -> Jnum (Nativeint.to_float ni)

let rec comp_sconst c =
  match c with
    | Const_base c -> comp_const c
    | Const_pointer i -> jnum_of_int i
    | Const_block (tag, cs) ->
	makeblock tag (List.map comp_sconst cs)
    | Const_float_array ss ->
	makeblock 0 (List.map (fun s -> Jnum (float_of_string s)) ss)
    | Const_immstring s -> Jstring s (* XXX when does this happen? *)

let comp_cmp c e1 e2 =
  match c with
    | Ceq -> Jeq (e1, e2)
    | Cneq -> Jneq (e1, e2)
    | Clt -> Jlt (e1, e2)
    | Cgt -> Jgt (e1, e2)
    | Cle -> Jleq (e1, e2)
    | Cge -> Jgeq (e1, e2)

let kreturn e = Jreturn e

let keffect = function
    (* anything that is already a value can have no effect *)
    (* mostly this arises with the compilation of () as Jnum 0 *)
  | (Jnum _ | Jvar _) -> Jempty
  | e -> Jexps e

let comp_ccall c es =
  let mathcall m es = Jcall (Jfieldref (Jvar "Math", m), es) in

  match c, es with
    | ("caml_int32_format", _ | "caml_nativeint_format", _ | "caml_int64_format", _) -> jcall "caml_format_int" es
    | "caml_format_float", _ -> jcall "oc$$sprintf" es
    | "caml_string_equal", _ -> jcall "oc$$seq" es
    | "caml_string_notequal", _ -> jcall "oc$$sneq" es
    | "caml_string_lessthan", _ -> jcall "oc$$slt" es
    | "caml_string_greaterthan", _ -> jcall "oc$$sgt" es
    | "caml_string_lessequal", _ -> jcall "oc$$slte" es
    | "caml_string_greaterequal", _ -> jcall "oc$$sgte" es
    | "caml_create_string", _ -> jcall "oc$$cms" es

    | "caml_power_float", _ -> mathcall "pow" es
    | "caml_exp_float", _ -> mathcall "exp" es
    | "caml_acos_float", _ -> mathcall "acos" es
    | "caml_asin_float", _ -> mathcall "asin" es
    | "caml_atan_float", _ -> mathcall "atan" es
    | "caml_atan2_float", _ -> mathcall "atan2" es
    | "caml_cos_float", _ -> mathcall "cos" es
    (* | "caml_cosh_float", _ -> ? *)
    | "caml_log_float", _ -> mathcall "log" es
    (* | "caml_log10_float", _ -> ? *)
    | "caml_sin_float", _ -> mathcall "sin" es
    (* | "caml_sinh_float", _ -> ? *)
    | "caml_sqrt_float", _ -> mathcall "sqrt" es
    | "caml_tan_float", _ -> mathcall "tan" es
    (* | "caml_tanh_float", _ -> ? *)
    | "caml_ceil_float", _ -> mathcall "ceil" es
    | "caml_floor_float", _ -> mathcall "floor" es
    | "caml_abs_float", _ -> mathcall "abs" es

    | "$assign", [e1; e2] -> Jassign(e1, e2)
    | "$call", e::es -> Jcall (e, es)
    | "$false", _ -> Jbool false
    | "$fieldref", [e; Jstring id] -> Jfieldref (e, id)
    | "$function", [Jcall (Jvar _, [Jfun _ as f])] -> f
    | "$hashref", [e1; e2] -> Jhashref (e1, e2)
    | "$new", (Jstring id)::es -> Jnew (id, es)
    | "$null", _ -> Jnull
    | "$this", _ -> Jthis
    | "$throw", [e] -> exp_of_stmts [Jthrow e]
    | "$true", _ -> Jbool true

    | "$var", [Jstring id] -> Jvar id

    | "$obj", [e] ->
	let rec o e l =
	  match e with
	    | Jcall (Jvar _, [ Jcall (Jvar _, [Jstring k; v]); e ]) -> o e ((k, v)::l)
	    | Jnum _ -> List.rev l
	    | _ -> raise (Unimplemented "bad $obj") in
	Jobject (o e [])

    | _ ->
	match c.[0], es with
	  | '#', e::es -> jmcall e (String.sub c 1 (String.length c - 1)) es
	  | '.', [e] -> Jfieldref (e, (String.sub c 1 (String.length c - 1)))
	  | '=', [e1; e2] -> Jassign(Jfieldref (e1, (String.sub c 1 (String.length c - 1))), e2)
	  | '@', _ -> jcall (String.sub c 1 (String.length c - 1)) es
	  | _ -> enter_c_prim c; jcall c es

let comp_prim p es =
  match p, es with
    | Pgetglobal i, [] -> enter_getglobal i; Jvar (jsident_of_ident i)
    | Pmakeblock (tag, _), _ -> makeblock tag es

    | (Pfield i, [e] | Pfloatfield i, [e]) -> Jhashref (e, jnum_of_int i)
    | (Psetfield (i, _), [e1; e2] | Psetfloatfield i, [e1; e2]) ->
	Jassign (Jhashref (e1, jnum_of_int i), e2)

    | Pccall { prim_name = "$new"; prim_native_name = "" }, es -> comp_ccall "$new" es
    | Pccall { prim_name = "$new"; prim_native_name = id }, es -> Jnew (id, es)
    | Pccall { prim_name = n }, es -> comp_ccall n es

    | Pisout, [h; e] ->
	Jlor (Jlt (e, jnum_of_int 0), Jgt (e, h))  (* XXX bind e to var? *)

    | Pabsfloat, [e] -> Jcall (Jfieldref (Jvar "Math", "abs"), [e])	

    | (Pintcomp c, [e1; e2] | Pbintcomp (_, c), [e1; e2] | Pfloatcomp c, [e1; e2]) ->
	comp_cmp c e1 e2

    | (Pnegint, [e] | Pnegbint _, [e] | Pnegfloat, [e]) -> Jminus e
    | (Paddint, [e1; e2] | Paddbint _, [e1; e2] | Paddfloat, [e1; e2]) -> Jadd (e1, e2)
    | (Psubint, [e1; e2] | Psubbint _, [e1; e2] | Psubfloat, [e1; e2]) -> Jsub (e1, e2)
    | (Pmulint, [e1; e2] | Pmulbint _, [e1; e2] | Pmulfloat, [e1; e2]) -> Jmul (e1, e2)
    | (Pdivint, [e1; e2] | Pdivbint _, [e1; e2] | Pdivfloat, [e1; e2]) -> Jlsr (Jdiv (e1, e2), jnum_of_int 0)
    | (Pmodint, [e1; e2] | Pmodbint _, [e1; e2]) -> Jmod (e1, e2)

    | (Plslint, [e1; e2] | Plslbint _, [e1; e2]) -> Jlsl (e1, e2)
    | (Plsrint, [e1; e2] | Plsrbint _, [e1; e2]) -> Jlsr (e1, e2)
    | (Pasrint, [e1; e2] | Pasrbint _, [e1; e2]) -> Jasr (e1, e2)

    | (Pandint, [e1; e2] | Pandbint _, [e1; e2]) -> Jand (e1, e2)
    | (Porint, [e1; e2] | Porbint _, [e1; e2]) -> Jor (e1, e2)
    | (Pxorint, [e1; e2] | Pxorbint _, [e1; e2]) -> Jxor (e1, e2)

    | Pnot, [e] -> Jnot e
    | Psequand, [e1; e2] -> Jland (e1, e2)
    | Psequor, [e1; e2] -> Jlor (e1, e2) (* XXX rhs is possibly a tail call *)

    | Poffsetint n, [e] -> Jadd (jnum_of_int n, e)

    | Poffsetref 1, [e] -> Jplus2 e
    | Poffsetref n, [e] -> Jassign (e, Jadd (jnum_of_int n, e)) (* XXX bind e to var? *)

    | Pstringlength, [e] -> Jfieldref (e, "length")
    | Parraylength _, [e] -> Jfieldref (e, "length")

    | Pmakearray _, es -> makeblock 0 es

    | Pstringrefu, _ -> jcall "oc$$srefu" es
    | Pstringsetu, _ -> jcall "oc$$ssetu" es
    | Pstringrefs, _ -> jcall "oc$$srefs" es
    | Pstringsets, _ -> jcall "oc$$ssets" es

    | Parrayrefu _, [e1; e2] -> Jhashref (e1, e2)
    | Parraysetu _, [e1; e2; e3] -> Jassign (Jhashref (e1, e2), e3)
    | Parrayrefs _, _ -> jcall "oc$$arefs" es
    | Parraysets _, _ -> jcall "oc$$asets" es

    | Pisint, [e] -> Jeq (Jtypeof e, Jstring "number")

    | (Pidentity, [e] | Pignore, [e] |
       Pfloatofint, [e] | Pintoffloat, [e] |
       Pintofbint _, [e] | Pbintofint _, [e] |
       Pcvtbint _, [e]) ->
	e

    | _ ->
	raise (Unimplemented "comp_prim") (* Jstring "comp_prim" *)

(* compile a lambda as a Js.exp *)
(* tail is true if the expression is in tail position *)
let rec comp_expr tail expr =
  match expr with
    | (Llet _ | Lletrec _) -> exp_of_stmts (comp_letrecs_st tail expr kreturn)

    | (Lswitch _ | Lprim (Praise, _) | Lstaticcatch _ | Lstaticraise _ |
       Ltrywith _ | Lfor _ | Lwhile _) ->
	exp_of_stmts (comp_expr_st tail expr kreturn)

    | Lvar i -> Jvar (jsident_of_ident i)

    | Lfunction (_, args, e) ->
	jcall "_f" [Jfun (List.map jsident_of_ident args, comp_expr_st true e kreturn)]

    | Lapply(e, es) ->
	let app = if tail then "__" else "_" in
	jmcall (comp_expr false e) app (List.map (comp_expr false) es)

    | Lifthenelse (i, t, e) -> Jite (comp_expr false i, comp_expr tail t,  comp_expr tail e)

    | Lconst c -> comp_sconst c

    | Lsequence (e1, e2) -> Jcomma (comp_expr false e1, comp_expr tail e2)

    | Lassign (i, e) -> Jassign (Jvar (jsident_of_ident i), comp_expr false e)

    | Lprim (p, args) -> comp_prim p (List.map (comp_expr false) args)

    | _ ->  Jstring "comp_expr"

(* compile a lambda as a Js.stmt list *)
(* tail is true if the expression is in tail position *)
(* k is called on the AST of exps in tail position *)
and comp_expr_st tail expr k =
  match expr with
    | (Llet _ | Lletrec _) -> comp_letrecs_st tail expr k

    | Lfor (i, e1, e2, d, e3) ->
	let i = jsident_of_ident i in
	let jv = Jvar i in
	let ce1 = comp_expr false e1
	and ce2 = comp_expr false e2
	and ce3 = comp_expr_st false e3 keffect in
	let (te, ie) = 
	  match d with
	    | Upto -> Jleq (jv, ce2), Jplus2 jv
	    | Downto -> Jgeq (jv, ce2), Jminus2 jv in
	[ Jfor (Jvars (i, ce1), te, Jexps ie, ce3) ]

    | Lwhile (e1, e2) ->
	[ Jwhile (comp_expr false e1, comp_expr_st false e2 keffect) ]

    | Lifthenelse (i, t, e) ->
	[ Jites (comp_expr false i, comp_expr_st tail t k, comp_expr_st tail e k) ]

    | Lswitch (se,
	       { sw_numconsts = nc; sw_consts = cs;
		 sw_numblocks = nb; sw_blocks = bs;
		 sw_failaction = fe }) ->
	(* we don't want to evaluate the scrutinee more than once: if
	   it is already a var leave it alone, otherwise bind a var *)
	let (k2, cse) =
	  match se with
	    | Lvar i -> ((fun x -> x), Jvar (jsident_of_ident i))
	    | _ ->
		let i = jsident_of_ident (Ident.create "s") in
		let cse = comp_expr false se in
		((fun x -> Jvars (i, cse) :: x), Jvar i) in
	let cc (i, e) =
          (* true if the sequence returns or throws; otherwise we need a break *)
          let rec exits stmts =
            match stmts with
              | [] -> false
              | _ ->
                  match List.nth stmts (List.length stmts - 1) with
                    | Jreturn _ -> true
                    | Jthrow _ -> true
                    | Jites (_, t, e) -> exits t && exits e
                    | _ -> false in
          let i = jnum_of_int i in
          let stmts = comp_expr_st tail e k in
          let stmts =
            if exits stmts
            then stmts
            else stmts @ [ Jbreak ] in
        (i, stmts) in
	let fss = match fe with None -> Some [k Jnull] | Some e -> Some (comp_expr_st tail e k) in
	let cswitch = Jswitch (cse, List.map cc cs, fss) in
	let bswitch = Jswitch (jcall "$t" [cse], List.map cc bs, fss) in
	let stmt =
	  if nc = 0 && nb = 0 then Jempty (* shouldn't happen *)
	  else if nc = 0 then bswitch
	  else if nb = 0 then cswitch
	  else Jites (Jeq (Jtypeof cse, Jstring "number"), [cswitch], [bswitch]) in
	k2 [stmt]

    | Lsequence (e1, e2) ->
	comp_expr_st false e1 keffect @ comp_expr_st tail e2 k

    | Lprim (Praise, [e]) -> [ Jthrow (comp_expr false e) ]

    | Lprim (Pignore, [e]) -> comp_expr_st false e keffect

    | Lstaticcatch (e1, (lab, args), e2) ->
	[ Jtrycatch
	    (comp_expr_st tail e1 k,
	    "$x",
	    Jites (Jneq (jnum_of_int lab, jcall "$xt" [Jvar "$x"]),
		  [Jthrow (Jvar "$x")], [])
	    ::
	      (* if no args, no need to build a function to call *)
	      match args with
		  [] -> comp_expr_st tail e2 k
		| _ ->
		    [ k (Jcall (Jfieldref
				   (Jfun (List.map jsident_of_ident args, comp_expr_st tail e2 kreturn),
				   "apply"),
			       [ Jnull; Jvar "$x" ])) ])]

    | Lstaticraise (lab, args) ->
	[ Jthrow (makexblock lab (List.map (comp_expr false) args)) ]

    | Ltrywith (e1, i, e2) ->
	[ Jtrycatch (comp_expr_st false e1 k, jsident_of_ident i, comp_expr_st tail e2 k) ]

    | _ -> [ k (comp_expr tail expr) ]

(* compile nested let/letrecs into a Js.stmt list *)
(* k is called on the AST of exps in tail position *)
and comp_letrecs_st tail expr k =
  let rec cl expr =
    match expr with
      | Llet (_, i, e1, e2) ->
	  let ce1 = comp_expr false e1 in
	  let css = cl e2 in
	  (Jvars (jsident_of_ident i, ce1))::css
      | Lletrec (bs, e) ->
	  let cb (id, e) = Jvars (jsident_of_ident id, comp_expr false e) in
	  List.map cb bs @ cl e
      | e -> comp_expr_st tail e k in
   cl expr

(**** Compilation of a lambda phrase ****)

let compile_implementation modulename expr =
  let ce =
    match expr with
      | Lprim (Psetglobal id, [e]) ->
	  enter_setglobal id;
	  Jvars (jsident_of_ident id, comp_expr false e)
      | _ -> raise (Unimplemented "compile_implementation") in
  let ret = (ce, !reloc_info) in
  reloc_info := [];
  ret
