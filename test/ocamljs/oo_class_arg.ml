open OUnit

class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

let tests = "Oo_class_arg" >:: begin fun () ->
  let p = new point 7 in
  assert_equal p#get_x 7;
  assert_equal p#get_offset 0;
  p#move 10;
  assert_equal p#get_x 17;
  assert_equal p#get_offset 10
end
