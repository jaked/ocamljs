open Camlp4.PreCast

module Q = Syntax.Quotation
module TheAntiquotSyntax = Syntax.AntiquotSyntax

(* I don't totally understand what's going on here but this is how
   Camlp4QuotationCommon.ml does it. *)

module MetaLocHere = Jslib_ast.Meta.MetaLoc
module MetaLoc =
struct
  (* module Ast = Ast *)
  let loc_name = ref None
  let meta_loc_expr _loc loc =
    match !loc_name with
      | None -> <:expr< $lid:!Loc.name$ >>
      | Some "here" -> MetaLocHere.meta_loc_expr _loc loc
      | Some x -> <:expr< $lid:x$ >>
  let meta_loc_patt _loc _ = <:patt< _ >>;
end
module MetaAst = Jslib_ast.Meta.Make(MetaLoc)
module ME = MetaAst.Expr
module MP = MetaAst.Patt

let is_antiquot s =
  let len = String.length s in
  len > 2 && s.[0] = '\\' && s.[1] = '$'

let handle_antiquot_in_string s term parse loc decorate =
  (* prerr_endline ("handle_antiquot_in_string " ^ s); *)
  if is_antiquot s then
    let pos = String.index s ':' in
    let name = String.sub s 2 (pos - 2)
    and code = String.sub s (pos + 1) (String.length s - pos - 1) in
    decorate name (parse loc code)
  else term

let antiquot_expander =
object
  inherit Ast.map as super
  method patt =
    function
      | <:patt@_loc< $anti:s$ >>
      | <:patt@_loc< $str:s$ >> as p ->
        handle_antiquot_in_string s p TheAntiquotSyntax.parse_patt _loc (fun n p -> p)
      | p -> super#patt p
  method expr =
    function
      | <:expr@_loc< $anti:s$ >>
      | <:expr@_loc< $str:s$ >> as e ->
        handle_antiquot_in_string s e TheAntiquotSyntax.parse_expr _loc (fun n e ->
          match n with
            | "`int" -> <:expr< string_of_int $e$ >>
            | "`flo" -> <:expr< string_of_float $e$ >>
            | "listexp" -> <:expr< Jslib_ast.exp_of_list $e$ >>
            (* | "`str" -> <:expr< Ast.safe_string_escaped $e$ >> *)
            | _ -> e )
      | e -> super#expr e
end

let add_quotation name entry mexpr mpatt =
  (* let entry_eoi = Jslib_parse.Gram.Entry.mk (Jslib_parse.Gram.Entry.name entry) in *)
  let entry_eoi = entry in
  let parse_quot_string entry loc s =
    let q = !Camlp4_config.antiquotations in
    let () = Camlp4_config.antiquotations := true in
    let res = Jslib_parse.Gram.parse_string entry loc s in
    let () = Camlp4_config.antiquotations := q in
    res in
  let expand_expr loc loc_name_opt s =
    let ast = parse_quot_string entry_eoi loc s in
    let () = MetaLoc.loc_name := loc_name_opt in
    let meta_ast = mexpr loc ast in
    let exp_ast = antiquot_expander#expr meta_ast in
    exp_ast in
  let expand_str_item loc loc_name_opt s =
    let exp_ast = expand_expr loc loc_name_opt s in
    <:str_item@loc< $exp:exp_ast$ >> in
  let expand_patt _loc loc_name_opt s =
    let ast = parse_quot_string entry_eoi _loc s in
    let meta_ast = mpatt _loc ast in
    let exp_ast = antiquot_expander#patt meta_ast in
    match loc_name_opt with
      | None -> exp_ast
      | Some name ->
        let rec subst_first_loc =
          function
            | <:patt@_loc< Ast.$uid:u$ $_$ >> -> <:patt< Ast.$uid:u$ $lid:name$ >>
            | <:patt@_loc< $a$ $b$ >> -> <:patt< $subst_first_loc a$ $b$ >>
            | p -> p in
        subst_first_loc exp_ast in
  (*
  EXTEND Jslib_parse.Gram
    entry_eoi:
    [ [ x = entry; `EOI -> x ] ]
  ;
  END;
  *)
  Q.add name Q.DynAst.expr_tag expand_expr;
  Q.add name Q.DynAst.patt_tag expand_patt;
  Q.add name Q.DynAst.str_item_tag expand_str_item;

;;

add_quotation "exp" Jslib_parse.expression ME.meta_exp MP.meta_exp;
add_quotation "stmt" Jslib_parse.statement ME.meta_stmt MP.meta_stmt;
Q.default := "exp";
