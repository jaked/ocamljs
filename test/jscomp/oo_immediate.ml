let p =
object
  val mutable x = 0
  method get_x = x
  method move d = x <- x + d
end

;;

print_endline (string_of_int p#get_x);
p#move 10;
print_endline (string_of_int p#get_x);
