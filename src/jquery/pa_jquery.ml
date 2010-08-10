open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_jquery"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax

  DELETE_RULE Gram expr: SELF; "#"; label END;

  EXTEND Gram
    expr: BEFORE "apply"
      [ "#" LEFTA
        [ e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ]
      ];
  END
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
