open Ocamljs.Inline
open Sample

let init () =
  if << !window.google || !google.gears >>
  then addStatus ~clas:"error" "Gears is not installed"

let browse _ =
  let desktop = (Gears.factory#create "beta.desktop" : Gears.desktop) in
  desktop#openFiles
    (Ocamljs.jsfun (fun files ->
      clearStatus ();
      addStatus "You picked the following files:";
      Array.iter (fun f -> addStatus f#_get_name) files))
    << { filter: ['text/plain', '.html'] } >>;
  true

;;

init ();

Dom.window#_set_onload (Ocamljs.jsfun (fun () ->
  (getElementById "browseButton" :Dom.button)#_set_onclick (Ocamljs.jsfun browse)));
