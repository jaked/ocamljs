open Ocamljs.Inline
open Sample

let db = ref (<< null >> : Gears.database)

let displayRecentPhrases () =
  let recentPhrases = [| ""; ""; "" |] in

  let rs = !db#execute "select * from Demo order by Timestamp desc" in
  let rec loop i =
    if rs#isValidRow
    then begin
      if i < 3 then recentPhrases.(i) <- rs#field 0
      else ignore (!db#execute_bind_ "delete from Demo where Timestamp=?" [| rs#field 1 |]);
      rs#next;
      loop (i + 1)
    end in
  loop 0;
  rs#close;

  let status = getElementById "status" in
  status#_set_innerHTML "";
  ArrayLabels.iteri recentPhrases ~f:(fun i rp ->
    let id = "phrase" ^ string_of_int i in
    status#_set_innerHTML (status#_get_innerHTML ^ ("<span id=\"" ^ id ^ "\"></span><br>"));
    let bullet = "(" ^ string_of_int (i + 1) ^ ")" in
    setTextContent (getElementById id) (bullet ^ rp))

let init () =
  let success =
    if << window.google && google.gears >>
    then
      try
        db := Gears.factory#create "beta.database";
        if Ocamljs.is_null !db (* can this happen w/o raising exception? *)
        then false
        else begin
          !db#open__ "database-demo";
          ignore (!db#execute "create table if not exists Demo (Phrase varchar(255), Timestamp int)");
          displayRecentPhrases ();
          true
        end
      with ex ->
        setError ("Could not create database: " ^ << $ex$.message >>);
        false
    else false in

  let inputs = (getElementById "form" : Dom.form)#_get_elements in
  ArrayLabels.iter inputs ~f:(fun el -> el#_set_disabled (not success))

let handleSubmit () =
  let elm = (getElementById "submitValue" : Dom.input) in
  let phrase = elm#_get_value in
  let currTime = (Javascript.new_Date ())#getTime in

  (* must magic because args have different types. should maybe add an any type to avoid this? *)
  ignore (!db#execute_bind_ "insert into Demo values (?, ?)" [| phrase; Obj.magic currTime |]);

  elm#_set_value "";
  displayRecentPhrases ();
  << false >>

;;

init ();

Dom.window#_set_onload (Ocamljs.jsfun (fun () ->
  (getElementById "form" : Dom.form)#_set_onsubmit (Ocamljs.jsfun handleSubmit)));
