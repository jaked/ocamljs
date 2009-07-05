class adjusted_point x_init =
  let origin = (x_init / 10) * 10 in
object 
  val mutable x = origin
  method get_x = x
  method get_offset = x - origin
  method move d = x <- x + d
end

;;

let p = new adjusted_point 7 in
print_endline (string_of_int p#get_x);
print_endline (string_of_int p#get_offset);
p#move 10;
print_endline (string_of_int p#get_x);
print_endline (string_of_int p#get_offset);
