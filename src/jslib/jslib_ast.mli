type loc = Camlp4.PreCast.Loc.t
and unop = Jdelete | Jvoid | Jtypeof | Jadd2_pre | Jsub2_pre | Jadd_pre | Jsub_pre | Jtilde | Jnot | Jadd2_post | Jsub2_post
and binop =
    Jhashref
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
    Jexp_list of loc * exp list
  | Jexp_list_Ant of loc * string
and exp =
    Jthis of loc
  | Jvar of loc * string
  | Jarray of loc * exp_list
  | Jobject of loc * (exp * exp) list
  | Jstring of loc * string * bool
  | Jnum of loc * string
  | Jnull of loc
  | Jbool of loc * bool
  | Jfun of loc * string option * string list * stmt list
  | Jfieldref of loc * exp * string
  | Junop of loc * unop * exp
  | Jbinop of loc * binop * exp * exp
  | Jite of loc * exp * exp * exp
  | Jcall of loc * exp * exp_list
  | Jnew of loc * exp * exp_list option
  | Jexp_Ant of loc * string
and stmt =
    Jempty of loc
  | Jvars of loc * (string * exp option) list
  | Jfuns of loc * string * string list * stmt list
  | Jreturn of loc * exp option
  | Jcontinue of loc * string option
  | Jbreak of loc * string option
  | Jswitch of loc * exp * (exp * stmt list) list * stmt list option
  | Jites of loc * exp * stmt * stmt option
  | Jthrow of loc * exp
  | Jexps of loc * exp
  | Jtrycatch of loc * stmt list * string * stmt list
  | Jtryfinally of loc * stmt list * stmt list
  | Jtrycatchfinally of loc * stmt list * string * stmt list * stmt list
  | Jfor of loc * exp option * exp option * exp option * stmt
  | Jdowhile of loc * stmt * exp
  | Jwhile of loc * exp * stmt
  | Jblock of loc * stmt list
  | Jwith of loc * exp * stmt
  | Jlabel of loc * string * stmt
  | Jstmt_Ant of loc * string
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
              val meta_exp_list :
                Camlp4.PreCast.Ast.loc ->
                exp_list -> Camlp4.PreCast.Ast.expr
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
              val meta_exp_list :
                Camlp4.PreCast.Ast.loc ->
                exp_list -> Camlp4.PreCast.Ast.patt
              val meta_stmt :
                Camlp4.PreCast.Ast.loc ->
                stmt -> Camlp4.PreCast.Ast.patt
              val meta_unop :
                Camlp4.PreCast.Ast.loc ->
                unop -> Camlp4.PreCast.Ast.patt
            end
        end
  end
