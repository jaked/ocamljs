open OUnit

class restricted_point x_init =
object (self)
  val mutable x = x_init
  method get_x = x
  method private move d = x <- x + d
  method bump = self#move 1
end

class point_again x =
object (self)
  inherit restricted_point x
  method virtual move : _
end

let tests = "Oo_private" >:: begin fun () ->
  let p = new restricted_point 7 in
  assert_equal p#get_x 7;
  p#bump;
  assert_equal p#get_x 8;
  let p = new point_again 7 in
  assert_equal p#get_x 7;
  p#move 10;
  assert_equal p#get_x 17
end
