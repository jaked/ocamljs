open OUnit

class printable_point x_init =
object (s)
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
  method print = string_of_int s#get_x
end

class printable_colored_point y c =
object (self)
  val c = c
  method color = c
  inherit printable_point y as super
  method print =
    super#print ^ self#color
end

let tests = "Oo_super" >:: begin fun () ->
  let p= new printable_colored_point 5 "red" in
  assert_equal p#print "5red"
end
