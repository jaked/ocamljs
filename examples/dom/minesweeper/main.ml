let int_input name value =
  let d = Dom.document in
  let res = d#createDocumentFragment in
  ignore (res#appendChild (d#createTextNode name));
  let input = (d#createElement "input" : Dom.input) in
  input#setAttribute "type" "text";
  input#_set_value (string_of_int !value);
  input#_set_onchange
    (Ocamljs.jsfun (fun _ ->
      (value := try int_of_string input#_get_value with _ -> !value);
      input#_set_value (string_of_int !value)));
  ignore (res#appendChild input);
  res

let button name callback =
  let d = Dom.document in
  let res = d#createDocumentFragment in
  let input = (d#createElement "input" : Dom.input) in
  input#setAttribute "type" "submit";
  input#_set_value name;
  input#_set_onclick (Ocamljs.jsfun callback);
  ignore (res#appendChild input);
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
  ignore (main#appendChild (int_input "Number of columns" nbr));
  ignore (main#appendChild (d#createElement "br"));
  ignore (main#appendChild (int_input "Number of rows" nbc));
  ignore (main#appendChild (d#createElement "br"));
  ignore (main#appendChild (int_input "Number of mines" nbm));
  ignore (main#appendChild (d#createElement "br"));
  ignore (main#appendChild
             (button "nouvelle partie"
                 (fun _ ->
                   let id = uid () in
                   ignore (main#appendChild (div id));
                   Minesweeper.run
                     id
                     (string_of_int !nbc)
                     (string_of_int !nbr)
                     (string_of_int !nbm);
                   false)));

;;

Dom.window#_set_onload (Ocamljs.jsfun onload)
