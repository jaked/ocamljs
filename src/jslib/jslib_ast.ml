(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
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

open Camlp4.PreCast
module Jslib_ast =
struct

  type loc = Loc.t

  INCLUDE "../jslib_ast.incl"

end

include Jslib_ast

external loc_of_exp : exp -> Loc.t = "%field0"

let rec exp_of_list = function
  | [] -> Jexp_nil Loc.ghost
  | [e] -> e
  | e::es -> Jexp_cons (loc_of_exp e, e, exp_of_list es)

let rec list_of_exp x acc =
  match x with
    | Jexp_nil _ -> acc
    | Jexp_cons (_, e1, e2) -> list_of_exp e1 (list_of_exp e2 acc)
    | e -> e :: acc

external loc_of_stmt : stmt -> Loc.t = "%field0"

let rec stmt_of_list = function
  | [] -> Jstmt_nil Loc.ghost
  | [e] -> e
  | e::es -> Jstmt_cons (loc_of_stmt e, e, stmt_of_list es)

let rec list_of_stmt x acc =
  match x with
    | Jstmt_nil _ -> acc
    | Jstmt_cons (_, e1, e2) -> list_of_stmt e1 (list_of_stmt e2 acc)
    | e -> e :: acc

module Meta =
struct

  (* I don't really understand what's going on here but this is how
     Camlp4Ast.mlast does it. *)

  module type META_LOC =
  sig
    val meta_loc_patt : Loc.t -> Loc.t -> Ast.patt
    val meta_loc_expr : Loc.t -> Loc.t -> Ast.expr
  end

  module MetaLoc =
  struct
    let meta_loc_patt _loc location =
      let (a, b, c, d, e, f, g, h) = Loc.to_tuple location in
      <:patt< Loc.of_tuple
        ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
        $`int:e$, $`int:f$, $`int:g$,
        $if h then <:patt< True >> else <:patt< False >> $) >>
    let meta_loc_expr _loc location =
      let (a, b, c, d, e, f, g, h) = Loc.to_tuple location in
      <:expr< Loc.of_tuple
        ($`str:a$, $`int:b$, $`int:c$, $`int:d$,
        $`int:e$, $`int:f$, $`int:g$,
        $if h then <:expr< True >> else <:expr< False >> $) >>
  end

  module MetaGhostLoc =
  struct
    let meta_loc_patt _loc _ = <:patt< Loc.ghost >>
    let meta_loc_expr _loc _ = <:expr< Loc.ghost >>
  end

  module MetaLocVar = struct
    let meta_loc_patt _loc _ = <:patt< $lid:!Loc.name$ >>
    let meta_loc_expr _loc _ = <:expr< $lid:!Loc.name$ >>
  end

  module Make (MetaLoc : META_LOC) =
  struct
    open MetaLoc

    module Expr =
    struct
      let meta_loc = meta_loc_expr

      let meta_option mf_a _loc = function
        | None -> <:expr< None >>
        | Some a -> <:expr< Some $mf_a _loc a$ >>

      include Camlp4Filters.MetaGeneratorExpr(Jslib_ast)
    end

    module Patt =
    struct
      let meta_loc = meta_loc_patt

      let meta_option mf_a _loc = function
        | None -> <:patt< None >>
        | Some a -> <:patt< Some $mf_a _loc a$ >>

      include Camlp4Filters.MetaGeneratorPatt(Jslib_ast)
    end
  end

  module MakeLambda (MetaLoc : META_LOC) =
  struct
    open MetaLoc

    module Expr =
    struct
      let meta_loc = meta_loc_expr

      let meta_option mf_a _loc = function
        | None -> <:expr< None >>
        | Some a -> <:expr< Some $mf_a _loc a$ >>

      include LambdaMetaGeneratorExpr(Jslib_ast)
    end

    module Patt =
    struct
      let meta_loc = meta_loc_patt

      let meta_option mf_a _loc = function
        | None -> <:patt< None >>
        | Some a -> <:patt< Some $mf_a _loc a$ >>

      include LambdaMetaGeneratorPatt(Jslib_ast)
    end
  end

  module MakeAbstractLambda (MetaLoc : META_LOC) =
  struct
    open MetaLoc

    module Expr =
    struct
      let meta_loc _loc = function
        | Ast.ExAnt (_loc, s) -> Ast.ExAnt (_loc, s)
        | _ ->
            (* XXX translate the argument location *)
            <:expr<
              Lambda.Lconst
                (Lambda.Const_block (0, [
                  Lambda.Const_immstring "ghost-location";
                  Lambda.Const_block (0, [
                    Lambda.Const_base (Asttypes.Const_int 1);
                    Lambda.Const_base (Asttypes.Const_int 0);
                    Lambda.Const_base (Asttypes.Const_int 0)
                  ]);
                  Lambda.Const_block (0, [
                    Lambda.Const_base (Asttypes.Const_int 1);
                    Lambda.Const_base (Asttypes.Const_int 0);
                    Lambda.Const_base (Asttypes.Const_int 0)
                  ]);
                  Lambda.Const_pointer 1
                ]))
            >>

      let meta_option mf_a _loc = function
        | <:expr< None >> -> <:expr< None >>
        | <:expr< Some $a$ >> -> <:expr< Some $mf_a _loc a$ >>
        | Ast.ExAnt (_loc, s) -> Ast.ExAnt (_loc, s)
        | _ -> invalid_arg "meta_option"

      include LambdaAbstractMetaGeneratorExpr(Jslib_ast)
    end

    module Patt =
    struct
      let meta_loc _loc = function
        | Ast.ExAnt (_loc, s) -> Ast.PaAnt (_loc, s)
        | _ ->
            (* XXX translate the argument location? *)
            <:patt< _ >>

      let meta_option mf_a _loc = function
        | <:expr< None >> -> <:patt< None >>
        | <:expr< Some $a$ >> -> <:patt< Some $mf_a _loc a$ >>
        | Ast.ExAnt (_loc, s) -> Ast.PaAnt (_loc, s)
        | _ -> invalid_arg "meta_option"

      include LambdaAbstractMetaGeneratorPatt(Jslib_ast)
    end
  end
end
