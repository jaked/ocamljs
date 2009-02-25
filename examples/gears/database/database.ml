open Ocamljs.Inline

class type console =
object
  method log : string -> unit
end
let console = (Ocamljs.var "console" : console)

(* stuff from sample.js *)

let isDefined type_ = type_ <> "undefined" && type_ <> "unknown"

let childNodes element =
  if isDefined << typeof $element$.childNodes >> then << $element$.childNodes >>
  else if isDefined << typeof $element$.children >> then << $element$.children >>
  else raise Not_found

let getElementById element_name =
  if isDefined << typeof document.getElementById >> then (<< document.getElementById($element_name$) >> : #Dom.element)
  else if isDefined << typeof document.all >> then (<< document.all[$element_name$] >> : #Dom.element)
  else raise Not_found

let setTextContent elem content =
  if isDefined << typeof $elem$.innerText >> then << $elem$.innerText = $content$ >>
  else if isDefined << typeof $elem$.textContent >> then << $elem$.textContent = $content$ >>
  else raise Not_found

let setupSample () =
  if
    << !window.google || !google.gears >> &&
      Dom.window#confirm "This demo requires Gears to be installed. Install now?"
  then
    Dom.window#_get_location#_set_href "http://code.google.com/apis/gears/install.html"

let addStatus message opt_class =
  let elm = getElementById "status" in
  if not (Ocamljs.is_null elm)
  then
    let id = "statusEntry" ^ string_of_int (Array.length (childNodes elm) + 1) in
    elm#_set_innerHTML
      (elm#_get_innerHTML ^
          (match opt_class with
            | Some c -> "<span id=\"" ^ id ^ "\" class=\"" ^ c ^ "\"></span>"
            | None -> "<span id=\"" ^ id ^ "\"></span>"));
    elm#_set_innerHTML (elm#_get_innerHTML ^ "<br>");
    setTextContent (getElementById id) message

let clearStatus () =
  let elm = getElementById "status" in
  elm#_set_innerHTML ""

let setError s =
  clearStatus ();
  addStatus s (Some "error")

let _ = setupSample ()

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
