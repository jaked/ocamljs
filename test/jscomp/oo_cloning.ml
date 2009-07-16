class point x_init =
object
  val mutable x = x_init
  method get_x = x
  method get_offset = x - x_init
  method move d = x <- x + d
end

;;

let p = new point 5 in
let q = Oo.copy p in
q#move 7;
print_endline (string_of_int p#get_x);
print_endline (string_of_int q#get_x);

print_endline (string_of_bool (p = q));
print_endline (string_of_bool (p = p));
