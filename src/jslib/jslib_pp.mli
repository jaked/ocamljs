val exp : Format.formatter -> Jslib_ast.exp -> unit
val stmt : Format.formatter -> Jslib_ast.stmt -> unit
val stmts : Format.formatter -> Jslib_ast.stmt list -> unit

val escaped : string -> string
