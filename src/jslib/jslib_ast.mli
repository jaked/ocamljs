(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
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

type loc = Camlp4.PreCast.Loc.t

INCLUDE "../jslib_ast.incl"

val loc_of_exp : exp -> Camlp4.PreCast.Loc.t
val exp_of_list : exp list -> exp
val list_of_exp : exp -> exp list -> exp list
val loc_of_stmt : stmt -> Camlp4.PreCast.Loc.t
val stmt_of_list : stmt list -> stmt
val list_of_stmt : stmt -> stmt list -> stmt list

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

    module MakeLambda :
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

    module MakeAbstractLambda :
      functor (MetaLoc : META_LOC) ->
        sig
          module Expr :
            sig
              val meta_loc :
                Camlp4.PreCast.Loc.t ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_option :
                (Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr) ->
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_string :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_int :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_float :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_char :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_bool :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_list :
                (Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr) ->
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_binop :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_exp :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_stmt :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
              val meta_unop :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
            end
          module Patt :
            sig
              val meta_loc :
                Camlp4.PreCast.Loc.t ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_option :
                (Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt) ->
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_string :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_int :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_float :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_char :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_bool :
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_list :
                (Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt) ->
                Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_binop :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_exp :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_stmt :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
              val meta_unop :
                Camlp4.PreCast.Ast.loc ->
                Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.patt
            end
        end

  end
