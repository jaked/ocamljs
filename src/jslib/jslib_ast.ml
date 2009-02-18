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

end
