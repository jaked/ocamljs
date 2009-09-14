open OUnit

class adjusted_point x_init =
  let origin = (x_init / 10) * 10 in
object
  val mutable x = origin
  method get_x = x
  method get_offset = x - origin
  method move d = x <- x + d
end

let tests = "Oo_class_let" >:: begin fun () ->
  let p = new adjusted_point 7 in
  assert_equal p#get_x 0;
  assert_equal p#get_offset 0;
  p#move 10;
  assert_equal p#get_x 10;
  assert_equal p#get_offset 10
end
