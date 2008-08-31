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

  and unop =
      | Jdelete
      | Jvoid
      | Jtypeof
      | Jadd2_pre
      | Jsub2_pre
      | Jadd_pre
      | Jsub_pre
      | Jtilde
      | Jnot
      | Jadd2_post
      | Jsub2_post

  and binop =
      | Jhashref
      | Jmul
      | Jdiv
      | Jmod
      | Jadd
      | Jsub
      | Jlt
      | Jgt
      | Jleq
      | Jgeq
      | Jlsr
      | Jlsl
      | Jasr
      | Jeq
      | Jneq
      | Jinstanceof
      | Jseq
      | Jsneq
      | Jland
      | Jlor
      | Jand
      | Jxor
      | Jor
      | Jcomma
      | Jassign
      | Jmul_assign
      | Jdiv_assign
      | Jmod_assign
      | Jadd_assign
      | Jsub_assign
      | Jlsl_assign
      | Jlsr_assign
      | Jasr_assign
      | Jand_assign
      | Jxor_assign
      | Jor_assign

  and exp_list =
      | Jexp_list of loc * exp list
      | Jexp_list_Ant of loc * string

  and exp =
      | Jthis of     loc
      | Jvar of      loc * string
      | Jarray of    loc * exp_list
      | Jobject of   loc * (exp * exp) list
      | Jstring of   loc * string * bool (* true if double-quoted *)
      | Jnum of      loc * string
      | Jnull of     loc
      | Jbool of     loc * bool
      | Jfun of      loc * string option * string list * stmt list
      | Jfieldref of loc * exp * string
      | Junop of     loc * unop * exp
      | Jbinop of    loc * binop * exp * exp
      | Jite of      loc * exp * exp * exp
      | Jcall of     loc * exp * exp_list
      | Jnew of      loc * exp * exp_list option
      | Jexp_Ant of  loc * string

  and stmt =
      | Jempty of    loc
      | Jvars of     loc * (string * exp option) list
      | Jfuns of     loc * string * string list * stmt list
      | Jreturn of   loc * exp option
      | Jcontinue of loc * string option
      | Jbreak of    loc * string option
      | Jswitch of   loc * exp * (exp * stmt list) list * stmt list option
      | Jites of     loc * exp * stmt * stmt option
      | Jthrow of    loc * exp
      | Jexps of     loc * exp
      | Jtrycatch of loc * stmt list * string * stmt list
      | Jtryfinally of loc * stmt list * stmt list
      | Jtrycatchfinally of loc * stmt list * string * stmt list * stmt list
      | Jfor of      loc * exp option * exp option * exp option * stmt
      | Jdowhile of  loc * stmt * exp
      | Jwhile of    loc * exp * stmt
      | Jblock of    loc * stmt list
      | Jwith of     loc * exp * stmt
      | Jlabel of    loc * string * stmt
      | Jstmt_Ant of loc * string

end

include Jslib_ast

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
