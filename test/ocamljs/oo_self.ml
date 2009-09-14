open OUnit

class printable_point x_init =
object (s)
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
  method print = string_of_int s#get_x
end

let tests = "Oo_self" >:: begin fun () ->
  let p = new printable_point 7 in
  assert_equal p#print "7";
  p#move 10;
  assert_equal p#print "17"
end
