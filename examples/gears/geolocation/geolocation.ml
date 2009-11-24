open Ocamljs.Inline
open Sample

let init () =
  if << !window.google || !google.gears >>
  then addStatus ~clas:"error" "Gears is not installed"
  else begin
    addStatus "Getting location...";

    let successCallback p =
      let address =
        p#_get_gearsAddress#_get_city ^ ", " ^
          p#_get_gearsAddress#_get_region ^ ", " ^
          p#_get_gearsAddress#_get_country ^ " (" ^
          string_of_float p#_get_latitude ^ ", " ^
          string_of_float p#_get_longitude ^ ")" in
      clearStatus ();
      addStatus ("Your address is: " ^ address) in

    let errorCallback err =
      let msg = "Error retrieving your location: " ^ err#_get_message in
      setError msg in

    try
      let geolocation = (Gears.factory#create "beta.geolocation" : Gears.geolocation) in
      geolocation#getCurrentPosition
        successCallback
        errorCallback
        << { enableHighAccuracy: true, gearsRequestAddress: true } >>
    with e ->
      setError ("Error using Geolocation API: " ^ << e.message >>)
  end

;;

init ()
