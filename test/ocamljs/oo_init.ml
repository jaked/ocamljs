open OUnit

let z = ref false

class printable_point x_init =
  let origin = (x_init / 10) * 10 in
object (self)
  val mutable x = origin
  method get_x = x
  method move d = x <- x + d
  initializer z := true
end

let tests = "Oo_init" >:: begin fun () ->
  ignore (new printable_point 17);
  assert_bool "" !z
end

