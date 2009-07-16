class restricted_point x_init =
object (self)
  val mutable x = x_init
  method get_x = x
  method private move d = x <- x + d
  method bump = self#move 1
end

class point_again x =
object (self)
  inherit restricted_point x
  method virtual move : _
end

;;

let p = new restricted_point 7 in
print_endline (string_of_int p#get_x);
p#bump;
print_endline (string_of_int p#get_x);

let p = new point_again 7 in
print_endline (string_of_int p#get_x);
p#move 10;
print_endline (string_of_int p#get_x);
