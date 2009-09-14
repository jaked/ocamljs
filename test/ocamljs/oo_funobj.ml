open OUnit

class functional_point y =
object
  val x = y
  method get_x = x
  method move d = {< x = x + d >}
end

let tests = "Oo_funobj" >:: begin fun () ->
  let p = new functional_point 7 in
  assert_equal p#get_x 7;
  assert_equal (p#move 3)#get_x 10;
  assert_equal p#get_x 7;
end
