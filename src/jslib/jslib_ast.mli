type loc = Camlp4.PreCast.Loc.t

INCLUDE "../jslib_ast.incl"

val loc_of_exp : exp -> Camlp4.PreCast.Loc.t
val exp_of_list : exp list -> exp
val list_of_exp : exp -> exp list -> exp list

module Meta :
  sig
    module type META_LOC =
      sig
        val meta_loc_patt :
          Camlp4.PreCast.Loc.t ->
          Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.patt
        val meta_loc_expr :
          Camlp4.PreCast.Loc.t ->
          Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.expr
      end
    module MetaLoc :
      sig
        val meta_loc_patt :
          Camlp4.PreCast.Ast.loc ->
          Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.patt
        val meta_loc_expr :
          Camlp4.PreCast.Ast.loc ->
          Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.expr
      end
    module MetaGhostLoc :
      sig
        val meta_loc_patt :
          Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.patt
        val meta_loc_expr :
          Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.expr
      end
    module MetaLocVar :
      sig
        val meta_loc_patt :
          Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.patt
        val meta_loc_expr :
          Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.expr
      end
    module Make :
      functor (MetaLoc : META_LOC) ->
        sig
          module Expr :
            sig
              val meta_loc :
                Camlp4.PreCast.Loc.t ->
                Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.expr
              val meta_option :
                (Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.expr) ->
                Camlp4.PreCast.Ast.loc ->
                'a option -> Camlp4.PreCast.Ast.expr
              val meta_string :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.expr
              val meta_int :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.expr
              val meta_float :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.expr
              val meta_char :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.expr
              val meta_bool :
                Camlp4.PreCast.Ast.loc -> bool -> Camlp4.PreCast.Ast.expr
              val meta_list :
                (Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.expr) ->
                Camlp4.PreCast.Ast.loc -> 'a list -> Camlp4.PreCast.Ast.expr
              val meta_binop :
                Camlp4.PreCast.Ast.loc ->
                binop -> Camlp4.PreCast.Ast.expr
              val meta_exp :
                Camlp4.PreCast.Ast.loc ->
                exp -> Camlp4.PreCast.Ast.expr
              val meta_stmt :
                Camlp4.PreCast.Ast.loc ->
                stmt -> Camlp4.PreCast.Ast.expr
              val meta_unop :
                Camlp4.PreCast.Ast.loc ->
                unop -> Camlp4.PreCast.Ast.expr
            end
          module Patt :
            sig
              val meta_loc :
                Camlp4.PreCast.Loc.t ->
                Camlp4.PreCast.Loc.t -> Camlp4.PreCast.Ast.patt
              val meta_option :
                (Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.patt) ->
                Camlp4.PreCast.Ast.loc ->
                'a option -> Camlp4.PreCast.Ast.patt
              val meta_string :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.patt
              val meta_int :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.patt
              val meta_float :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.patt
              val meta_char :
                Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.patt
              val meta_bool :
                Camlp4.PreCast.Ast.loc -> bool -> Camlp4.PreCast.Ast.patt
              val meta_list :
                (Camlp4.PreCast.Ast.loc -> 'a -> Camlp4.PreCast.Ast.patt) ->
                Camlp4.PreCast.Ast.loc -> 'a list -> Camlp4.PreCast.Ast.patt
              val meta_binop :
                Camlp4.PreCast.Ast.loc ->
                binop -> Camlp4.PreCast.Ast.patt
              val meta_exp :
                Camlp4.PreCast.Ast.loc ->
                exp -> Camlp4.PreCast.Ast.patt
              val meta_stmt :
                Camlp4.PreCast.Ast.loc ->
                stmt -> Camlp4.PreCast.Ast.patt
              val meta_unop :
                Camlp4.PreCast.Ast.loc ->
                unop -> Camlp4.PreCast.Ast.patt
            end
        end
  end
