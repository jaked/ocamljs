class functional_point y =
object
  val x = y
  method get_x = x
  method move d = {< x = x + d >}
end

;;

let p = new functional_point 7 in
print_endline (string_of_int p#get_x);
print_endline (string_of_int (p#move 3)#get_x);
print_endline (string_of_int p#get_x);
