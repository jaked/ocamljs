open OUnit

class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

let tests = "Oo_cloning" >:: begin fun () ->
  let p = new point 5 in
  let q = Oo.copy p in
  q#move 7;
  assert_equal p#get_x 5;
  assert_equal q#get_x 12;

  assert_bool "" (not (p = q));
  assert_bool "" (p = p);
end
