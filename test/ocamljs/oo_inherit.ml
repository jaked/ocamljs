open OUnit

class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

class colored_point x (c : string) =
object
  inherit point x
  val c = c
  method color = c
end

let tests = "Oo_inherit" >:: begin fun () ->
  let p = new colored_point 5 "red" in
  assert_equal p#get_x 5;
  assert_equal p#color "red";
end
