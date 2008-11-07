open Ocamljs.Inline

let init = ignore
let full_init = ignore
let self_init = ignore
let bits () = << Math.floor (Math.random() * 1073741824) >>
let int b = << Math.floor (Math.random() * $b$) >>
let int32 b = << Math.floor (Math.random() * $b$) >>
let nativeint b = << Math.floor (Math.random() * $b$) >>
let int64 _ = Int64.zero
let float b = << Math.random() * $b$ >>
let bool _ = << Math.random() < 0.5 >>

module State =
struct
  type t = unit

  let make = ignore
  let make_self_init = ignore
  let copy = ignore

  let bits _ = bits ()
  let int _ b = int b
  let int32 _ b = int32 b
  let nativeint _ b = nativeint b
  let int64 _ b = int64 b
  let float _ b = float b
  let bool _ = bool ()
end

let get_state = ignore
let set_state = ignore
