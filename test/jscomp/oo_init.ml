class printable_point x_init =
  let origin = (x_init / 10) * 10 in
object (self)
  val mutable x = origin
  method get_x = x
  method move d = x <- x + d
  method print = print_endline (string_of_int self#get_x)
  initializer print_endline "new point at "; self#print
end

;;

let p = new printable_point 17
