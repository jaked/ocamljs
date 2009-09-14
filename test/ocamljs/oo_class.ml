open OUnit

class point =
object
  val mutable x = 0
  method get_x = x
  method move d = x <- x + d
end

let tests = "Oo_class" >:: begin fun () ->
  let p = new point in
  assert_equal p#get_x 0;
  p#move 10;
  assert_equal p#get_x 10
end
