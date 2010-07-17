(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Original file (camlp4/Camlp4Filters/Camlp4MetaGenerator.ml in
 * the Objective Caml source distribution) is Copyright (C) INRIA.
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

open Camlp4
open PreCast
module MapTy = Map.Make(String)

let _loc = Loc.ghost

type t =
  { name : Ast.ident;
    type_decls : Ast.ctyp MapTy.t;
    acc : Ast.expr;
    app : Ast.expr;
    id  : Ast.expr;
    tup : Ast.expr;
    com : Ast.expr;
    str : Ast.expr;
    int : Ast.expr;
    flo : Ast.expr;
    chr : Ast.expr;
    ant : Ast.ident;
  }

let ex_t i type_decls = {
  name = i;
  type_decls = Lazy.force type_decls;
  app = <:expr< Ast.ExApp >>;
  acc = <:expr< Ast.ExAcc >>;
  id  = <:expr< Ast.ExId  >>;
  tup = <:expr< Ast.ExTup >>;
  com = <:expr< Ast.ExCom >>;
  str = <:expr< Ast.ExStr >>;
  int = <:expr< Ast.ExInt >>;
  flo = <:expr< Ast.ExFlo >>;
  chr = <:expr< Ast.ExChr >>;
  ant = <:ident< Ast.ExAnt >>
}

let pa_t i type_decls = {
  name = i;
  type_decls = Lazy.force type_decls;
  app = <:expr< Ast.PaApp >>;
  acc = <:expr< Ast.PaAcc >>;
  id  = <:expr< Ast.PaId  >>;
  tup = <:expr< Ast.PaTup >>;
  com = <:expr< Ast.PaCom >>;
  str = <:expr< Ast.PaStr >>;
  int = <:expr< Ast.PaInt >>;
  flo = <:expr< Ast.PaFlo >>;
  chr = <:expr< Ast.PaChr >>;
  ant = <:ident< Ast.PaAnt >>
}

let x i = <:ident< $lid:"x"^string_of_int i$ >>

let meta_ s = <:ident< $lid:"meta_"^s$ >>

let mf_ s = "mf_" ^ s

let rec string_of_ident = function
  | <:ident< $lid:s$ >> -> s
  | <:ident< $uid:s$ >> -> s
  | <:ident< $i1$.$i2$ >> -> "acc_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | <:ident< $i1$($i2$) >> -> "app_" ^ (string_of_ident i1) ^ "_" ^ (string_of_ident i2)
  | <:ident< $anti:_$ >> -> assert false

let fold_args ty f init =
  let (_, res) =
    List.fold_left begin fun (i, acc) ty ->
      (succ i, f ty i acc)
    end (0, init) ty
  in res

let fold_right_args ty f init =
  let (_, res) =
    List.fold_right begin fun ty (i, acc) ->
      (pred i, f ty i acc)
    end ty (List.length ty - 1, init)
  in res

let fold_data_ctors ty f init =
  let counter () = let n = ref 0 in fun () -> let nn = !n in incr n; nn in
  let tag = counter () in
  let data_tag = counter () in
  let rec loop acc t =
    match t with
      | <:ctyp< $uid:cons$ of $ty$ >> -> f (data_tag()) cons (Ast.list_of_ctyp ty []) acc
      | <:ctyp< $uid:cons$ >> -> f (tag()) cons [] acc
      | <:ctyp< $t1$ | $t2$ >> -> loop (loop acc t1) t2
      | <:ctyp<>> -> acc
      | _ -> assert false in
  loop init ty

let fold_type_decls m f init =
  MapTy.fold f m.type_decls init

let patt_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    Ast.PaApp(_loc, acc, <:patt< $id:x i$ >>)
  end <:patt< $id:cons$ >>

let expr_of_data_ctor_decl cons tyargs =
  fold_args tyargs begin fun _ i acc ->
    <:expr< $acc$ $id:x i$ >>
  end <:expr< $id:cons$ >>

let is_antiquot_data_ctor s =
  let ls = String.length s in
  ls > 3 && String.sub s (ls - 3) 3 = "Ant"

let rec meta_ident m = function
  | <:ident< $i1$.$i2$ >> -> <:expr< Ast.IdAcc(_loc, $meta_ident m i1$, $meta_ident m i2$) >>
  | <:ident< $i1$($i2$) >> -> <:expr< Ast.IdApp(_loc, $meta_ident m i1$, $meta_ident m i2$) >>
  | <:ident< $anti:s$ >>  -> <:expr< $anti:s$ >>
  | <:ident< $lid:s$ >>   -> <:expr< Ast.IdLid(_loc, $str:s$) >>
  | <:ident< $uid:s$ >>   -> <:expr< Ast.IdUid(_loc, $str:s$) >>
let m_app m x y = <:expr< $m.app$ _loc $x$ $y$ >>
let m_id m i = <:expr< $m.id$ _loc $i$ >>
let m_uid m s = m_id m (meta_ident m <:ident< $uid:s$ >>)

let rec pmeta_ident m = function
  | <:ident< $i1$.$i2$ >> -> <:patt< Ast.IdAcc(_, $pmeta_ident m i1$, $pmeta_ident m i2$) >>
  | <:ident< $i1$($i2$) >> -> <:patt< Ast.IdApp(_, $pmeta_ident m i1$, $pmeta_ident m i2$) >>
  | <:ident< $anti:s$ >>  -> <:patt< $anti:s$ >>
  | <:ident< $lid:s$ >>   -> <:patt< Ast.IdLid(_, $str:s$) >>
  | <:ident< $uid:s$ >>   -> <:patt< Ast.IdUid(_, $str:s$) >>
let pm_app m x y = <:patt< Ast.ExApp(_, $x$, $y$) >>
let pm_id m i = <:patt< Ast.ExId(_, $i$) >>
let pm_uid m s = pm_id m (pmeta_ident m <:ident< $uid:s$ >>)

let failure = <:expr< raise (Failure "MetaGenerator: cannot handle that kind of types") >>

let mk_meta m =
  let m_name_uid x = <:ident< $m.name$.$uid:x$ >> in
  fold_type_decls m begin fun tyname tydcl binding_acc ->
    match tydcl with
    | Ast.TyDcl (_, _, tyvars, Ast.TySum (_, ty), _) ->
      let match_case =
        fold_data_ctors ty begin fun tag cons tyargs acc ->
          let m_name_cons = m_name_uid cons in
          let p = patt_of_data_ctor_decl m_name_cons tyargs in
          let e =
            if cons = "BAnt" || cons = "OAnt" || cons = "LAnt" then
              <:expr< $id:m.ant$ _loc x0 >>
            else if is_antiquot_data_ctor cons then
              expr_of_data_ctor_decl m.ant tyargs
            else
              let tag = <:expr< $m.int$ _loc $str:string_of_int tag$ >> in
              if tyargs = []
              then
                (m_app m
                    (m_id m (meta_ident m <:ident< Lambda.Lconst >>))
                    (m_app m
                        (m_id m (meta_ident m <:ident< Lambda.Const_pointer >>))
                        tag))
              else
                let args =
                  fold_right_args tyargs begin fun ty i acc ->
                    let rec fcall_of_ctyp ty =
                      match ty with
                        | <:ctyp< $id:id$ >> ->
                            <:expr< $id:meta_ (string_of_ident id)$ >>
                        | <:ctyp< ($t1$ * $t2$) >> ->
                            <:expr< fun _loc (x1, x2) ->
                              $m.tup$ _loc
                                ($m.com$ _loc
                                    ($fcall_of_ctyp t1$ _loc x1)
                                    ($fcall_of_ctyp t2$ _loc x2)) >>
                        | <:ctyp< $t1$ $t2$ >> ->
                            <:expr< $fcall_of_ctyp t1$ $fcall_of_ctyp t2$ >>
                        | <:ctyp< '$s$ >> -> <:expr< $lid:mf_ s$ >>
                        | _ -> failure in
                    m_app m
                      (m_app m (m_uid m "::")
                        <:expr< $fcall_of_ctyp ty$ _loc $id:x i$ >>)
                      acc
                  end (m_uid m "[]") in
                m_app m
                  (m_app m
                      (m_id m (meta_ident m <:ident< Lambda.Lprim >>))
                      (m_app m
                          (m_app m
                              (m_id m (meta_ident m <:ident< Lambda.Pmakeblock >>))
                              tag)
                          (m_id m (meta_ident m <:ident< Asttypes.Immutable >>))))
                  args
          in <:match_case< $p$ -> $e$ | $acc$ >>
        end <:match_case<>> in
        let funct =
          List.fold_right begin fun tyvar acc ->
            match tyvar with
            | <:ctyp< +'$s$ >> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> ->
                <:expr< fun $lid:mf_ s$ -> $acc$ >>
            | _ -> assert false
          end tyvars <:expr< fun _loc -> fun [ $match_case$ ] >>
        in <:binding< $binding_acc$ and $lid:"meta_"^tyname$ = $funct$ >>
    | Ast.TyDcl (_, _, _, _, _) -> binding_acc
    | _ -> assert false
  end <:binding<>>

let mk_abs_meta m =
  let m_name_uid x = <:ident< $uid:x$ >> in
  fold_type_decls m begin fun tyname tydcl binding_acc ->
    match tydcl with
    | Ast.TyDcl (_, _, tyvars, Ast.TySum (_, ty), _) ->
      let match_case =
        fold_data_ctors ty begin fun tag cons tyargs acc ->
          let m_name_cons = m_name_uid cons in
          let p =
            fold_args tyargs begin fun _ i acc ->
              <:patt< Ast.ExApp(_, $acc$, $id:x i$) >>
            end (pm_id m (pmeta_ident m m_name_cons)) in
          let e =
            let tag = <:expr< $m.int$ _loc $str:string_of_int tag$ >> in
            if tyargs = []
            then
              (m_app m
                  (m_id m (meta_ident m <:ident< Lambda.Lconst >>))
                  (m_app m
                      (m_id m (meta_ident m <:ident< Lambda.Const_pointer >>))
                      tag))
            else
              let args =
                fold_right_args tyargs begin fun ty i acc ->
                  let rec fcall_of_ctyp ty =
                    match ty with
                      | <:ctyp< $id:id$ >> ->
                          <:expr< $id:meta_ (string_of_ident id)$ >>
                      | <:ctyp< ($t1$ * $t2$) >> ->
                          <:expr< fun _loc -> fun
                            [ Ast.ExTup (_, Ast.ExCom (_, x1, x2)) ->
                                $m.tup$ _loc
                                  ($m.com$ _loc
                                      ($fcall_of_ctyp t1$ _loc x1)
                                      ($fcall_of_ctyp t2$ _loc x2))
                            | _ -> invalid_arg "tuple"
                            ]
                          >>
                      | <:ctyp< $t1$ $t2$ >> ->
                          <:expr< $fcall_of_ctyp t1$ $fcall_of_ctyp t2$ >>
                      | <:ctyp< '$s$ >> -> <:expr< $lid:mf_ s$ >>
                      | _ -> failure in
                  m_app m
                    (m_app m (m_uid m "::")
                      <:expr< $fcall_of_ctyp ty$ _loc $id:x i$ >>)
                    acc
                end (m_uid m "[]") in
              m_app m
                (m_app m
                    (m_id m (meta_ident m <:ident< Lambda.Lprim >>))
                    (m_app m
                        (m_app m
                            (m_id m (meta_ident m <:ident< Lambda.Pmakeblock >>))
                            tag)
                        (m_id m (meta_ident m <:ident< Asttypes.Immutable >>))))
                args
          in <:match_case< $p$ -> $e$ | $acc$ >>
        end <:match_case<
                Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
              | _ -> invalid_arg $`str:"meta_" ^ tyname$
            >> in
        let funct =
          List.fold_right begin fun tyvar acc ->
            match tyvar with
            | <:ctyp< +'$s$ >> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> ->
                <:expr< fun $lid:mf_ s$ -> $acc$ >>
            | _ -> assert false
          end tyvars <:expr< fun _loc -> fun [ $match_case$ ] >>
        in <:binding< $binding_acc$ and $lid:"meta_"^tyname$ = $funct$ >>
    | Ast.TyDcl (_, _, _, _, _) -> binding_acc
    | _ -> assert false
  end <:binding<>>

let find_type_decls =
object
  inherit Ast.fold as super
  val accu = MapTy.empty
  method get = accu
  method ctyp = function
    | Ast.TyDcl (_, name, _, _, _) as t -> {< accu = MapTy.add name t accu >}
    | t -> super#ctyp t
end

let filter st =
  let type_decls = lazy (find_type_decls#str_item st)#get in
  object
   inherit Ast.map as super
   method module_expr me =
     let mk_meta_module m =
       let bi = mk_meta m in
       <:module_expr<
        struct
          value meta_string _loc s = $m.str$ _loc s;
          value meta_int _loc s = $m.int$ _loc s;
          value meta_float _loc s = $m.flo$ _loc s;
          value meta_char _loc s = $m.chr$ _loc s;
          value meta_bool _loc b =
            $m_app m
              (m_id m (meta_ident m <:ident< Lambda.Lconst >>))
              (m_app m
                 (m_id m (meta_ident m <:ident< Lambda.Const_pointer >>))
                 <:expr< $m.int$ _loc (if b then "1" else "0") >>)$;
          value rec meta_list mf_a _loc = fun
            [ [] -> $m_uid m "[]"$
            | [ x :: xs ] -> $m_app m (m_app m (m_uid m "::") <:expr< mf_a _loc x >>) <:expr< meta_list mf_a _loc xs >>$
            ];
          value rec $bi$;
        end >> in
     let mk_abs_meta_module m =
       let bi = mk_abs_meta m in
       <:module_expr<
        struct
          value meta_string _loc = fun
            [ Ast.ExStr (_loc, s) -> $m.str$ _loc s
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_string"
            ];
          value meta_int _loc = fun
            [ Ast.ExInt (loc, s) -> $m.int$ _loc s
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_int"
            ];
          value meta_float _loc = fun
            [ Ast.ExFlo (_loc, s) -> $m.flo$ _loc s
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_float"
            ];
          value meta_char _loc = fun
            [ Ast.ExChr (_loc, s) -> $m.chr$ _loc s
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_char"
            ];
(*
          value meta_bool _loc b =
            Lambda.Lconst
              (Lambda.Const_pointer
                  ($m.int$ _loc (match b with false -> "0" | true -> "1")));
*)
          value meta_bool _loc = fun
            [ <:expr< false >> -> $m_uid m "False"$
            | <:expr< true >> -> $m_uid m "True"$
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_bool"
            ];
          value rec meta_list mf_a _loc = fun
            [ <:expr< [] >> -> $m_uid m "[]"$
            | <:expr< [ $x$ :: $xs$ ] >> -> $m_app m (m_app m (m_uid m "::") <:expr< mf_a _loc x >>) <:expr< meta_list mf_a _loc xs >>$
            | Ast.ExAnt (_loc, s) -> $id:m.ant$ (_loc, s)
            | _ -> invalid_arg "meta_list"
            ];
          value rec $bi$;
        end >> in
     match super#module_expr me with
     | <:module_expr< LambdaMetaGeneratorExpr($id:i$) >> ->
         mk_meta_module (ex_t i type_decls)
     | <:module_expr< LambdaAbstractMetaGeneratorExpr($id:i$) >> ->
         mk_abs_meta_module (ex_t i type_decls)
     | <:module_expr< LambdaMetaGeneratorPatt($id:i$) >> ->
         mk_meta_module (pa_t i type_decls)
     | <:module_expr< LambdaAbstractMetaGeneratorPatt($id:i$) >> ->
         mk_abs_meta_module (pa_t i type_decls)
     | me -> me
  end#str_item st

;;

AstFilters.register_str_item_filter filter
