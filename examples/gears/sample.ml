open Ocamljs.Inline

IFDEF DEBUG
class type console =
object
  method log : string -> unit
end
let console = (Ocamljs.var "console" : console)
ENDIF

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

let addStatus ?clas message =
  let elm = getElementById "status" in
  if not (Ocamljs.is_null elm)
  then
    let id = "statusEntry" ^ string_of_int (Array.length (childNodes elm) + 1) in
    elm#_set_innerHTML
      (elm#_get_innerHTML ^
          (match clas with
            | Some c -> "<span id=\"" ^ id ^ "\" class=\"" ^ c ^ "\"></span>"
            | None -> "<span id=\"" ^ id ^ "\"></span>"));
    elm#_set_innerHTML (elm#_get_innerHTML ^ "<br>");
    setTextContent (getElementById id) message

let clearStatus () =
  let elm = getElementById "status" in
  elm#_set_innerHTML ""

let setError s =
  clearStatus ();
  addStatus ~clas:"error" s

let _ = setupSample ()
