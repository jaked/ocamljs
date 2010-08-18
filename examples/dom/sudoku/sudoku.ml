module D = Dom
let d = D.document

let make_board () =
  let make_input () =
    let input = (d#createElement "input" : D.input) in
    input#setAttribute "type" "text";
    input#_set_size 1;
    input#_set_maxLength 1;
    let style = input#_get_style in
    style#_set_border "none";
    style#_set_padding "0px";
    let enforce_digit () =
      match input#_get_value with
        | "1" | "2" | "3" | "4" | "5"
        | "6" | "7" | "8" | "9" -> ()
        | _ -> input#_set_value "" in
    input#_set_onchange enforce_digit;
    input in

  let make_td i j input =
    let td = d#createElement "td" in
    let style = td#_get_style in
    style#_set_borderStyle "solid";
    style#_set_borderColor "#000000";
    let widths = function
      | 0 -> 2, 0 | 2 -> 1, 1 | 3 -> 1, 0
      | 5 -> 1, 1 | 6 -> 1, 0 | 8 -> 1, 2
      | _ -> 1, 0 in
    let (top, bottom) = widths i in
    let (left, right) = widths j in
    let px k = string_of_int k ^ "px" in
    style#_set_borderTopWidth (px top);
    style#_set_borderBottomWidth (px bottom);
    style#_set_borderLeftWidth (px left);
    style#_set_borderRightWidth (px right);
    ignore (td#appendChild input);
    td in

  let rows =
    Array.init 9 (fun i ->
      Array.init 9 (fun j ->
        make_input ())) in

  let table = d#createElement "table" in
  table#setAttribute "cellpadding" "0px";
  table#setAttribute "cellspacing" "0px";
  let tbody = d#createElement "tbody" in
  ignore (table#appendChild tbody);
  ArrayLabels.iteri rows ~f:(fun i row ->
    let tr = d#createElement "tr" in
    ArrayLabels.iteri row ~f:(fun j cell ->
      let td = make_td i j cell in
      ignore (tr#appendChild td));
    ignore (tbody#appendChild tr));

  (rows, table)

let check_board rows _ =
  let error i j =
    let cell = rows.(i).(j) in
    cell#_get_style#_set_backgroundColor "#ff0000" in

  let check_set set =
    let seen = Array.make 9 None in
    ArrayLabels.iter set ~f:(fun (i,j) ->
      let cell = rows.(i).(j) in
      match cell#_get_value with
        | "" -> ()
        | v ->
            let n = int_of_string v in
            match seen.(n - 1) with
              | None ->
                  seen.(n - 1) <- Some (i,j)
              | Some (i',j') ->
                  error i j;
                  error i' j') in

  let check_row i =
    check_set (Array.init 9 (fun j -> (i,j))) in

  let check_column j =
    check_set (Array.init 9 (fun i -> (i,j))) in

  let check_square i j =
    let set = Array.init 9 (fun k ->
      i * 3 + k mod 3, j * 3 + k / 3) in
    check_set set in

  ArrayLabels.iter rows ~f:(fun row ->
    ArrayLabels.iter row ~f:(fun cell ->
      cell#_get_style#_set_backgroundColor "#ffffff"));

  for i = 0 to 8 do check_row i done;
  for j = 0 to 8 do check_column j done;
  for i = 0 to 2 do
    for j = 0 to 2 do
      check_square i j
    done
  done;
  false

let onload () =
  let (rows, table) = make_board () in
  let check = d#getElementById "check" in
  check#_set_onclick (check_board rows);
  let board = d#getElementById "board" in
  ignore (board#appendChild table)

;;

D.window#_set_onload onload
