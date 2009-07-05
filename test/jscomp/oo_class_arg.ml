class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

;;

let p = new point 7 in
print_endline (string_of_int p#get_x);
print_endline (string_of_int p#get_offset);
p#move 10;
print_endline (string_of_int p#get_x);
print_endline (string_of_int p#get_offset);
