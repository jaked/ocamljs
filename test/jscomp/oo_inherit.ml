class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

class colored_point x (c : string) =
object
  inherit point x
  val c = c
  method color = c
end

;;

let p = new colored_point 5 "red" in
print_endline (string_of_int p#get_x);
print_endline p#color;
