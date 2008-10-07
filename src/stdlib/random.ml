let init = ignore
let full_init = ignore
let self_init = ignore
let bits () = 0
let int _ = 0
let int32 _ = Int32.zero
let nativeint _ = Nativeint.zero
let int64 _ = Int64.zero
let float _ = 0.
let bool _ = false

module State =
struct
  type t = unit

  let make = ignore
  let make_self_init = ignore
  let copy = ignore

  let bits _ = 0
  let int _ _ = 0
  let int32 _ _ = Int32.zero
  let nativeint _ _ = Nativeint.zero
  let int64 _ _ = Int64.zero
  let float _ _ = 0.
  let bool _ = false
end

let get_state = ignore
let set_state = ignore
