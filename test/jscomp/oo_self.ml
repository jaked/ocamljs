class printable_point x_init =
object (s)
  val mutable x = x_init
  method get_x = x
  method move d = x <- x + d
  method print = print_endline (string_of_int s#get_x)
end

;;

let p = new printable_point 7 in
p#print;
p#move 10;
p#print
