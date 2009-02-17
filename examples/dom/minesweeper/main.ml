let append n1 n2 = ignore (n1#appendChild (n2 :> Dom.node))

let int_input name value =
  let d = Dom.document in
  let res = d#createDocumentFragment in
  append res (d#createTextNode name);
  let input = (d#createElement "input" : Dom.input) in
  input#setAttribute "type" "text";
  input#_set_value (string_of_int !value);
  input#_set_onchange
    (Ocamljs.jsfun (fun _ ->
      (value := try int_of_string input#_get_value with _ -> !value);
      input#_set_value (string_of_int !value)));
  append res input;
  res

let button name callback =
  let d = Dom.document in
  let res = d#createDocumentFragment in
  let input = (d#createElement "input" : Dom.input) in
  input#setAttribute "type" "submit";
  input#_set_value name;
  input#_set_onclick (Ocamljs.jsfun callback);
  append res input;
  res

let div id =
  let div = Dom.document#createElement "div" in
  div#setAttribute "id" id;
  div

let uid = let uid = ref 0 in fun () -> incr uid ; "caml__" ^ string_of_int  !uid

let onload _ =
  let d = Dom.document in
  let main = d#getElementById "main" in
  let nbr, nbc, nbm = ref 10, ref 12, ref 15 in
  append main (int_input "Number of columns" nbr);
  append main (d#createElement "br");
  append main (int_input "Number of rows" nbc);
  append main (d#createElement "br");
  append main (int_input "Number of mines" nbm);
  append main (d#createElement "br");
  append main
    (button "nouvelle partie"
        (fun _ ->
          let id = uid () in
          append main (div id);
          Minesweeper.run id (string_of_int !nbc) (string_of_int !nbr) (string_of_int !nbm);
          false));

;;

Dom.window#_set_onload (Ocamljs.jsfun onload)
