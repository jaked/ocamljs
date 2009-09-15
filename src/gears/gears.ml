class type factory =
object
  method create : string -> < .. >
  method getBuildInfo : string
  method getPermission : string -> string -> string -> bool
  method _get_version : string
  method _get_hasPermission : bool
end

let factory = (Ocamljs.var "google.gears.factory" : factory)

class type blob =
object
  method _get_length : int
  method slice : int -> int -> blob
end

class type resultSet =
object
  method isValidRow : bool
  method next : unit
  method close : unit
  method fieldCount : int
  method fieldName : int -> string
  method field : int -> 'a
  method fieldByName : string -> 'a
end

class type database =
object
  method _get_lastInsertRowId : int
  method _get_rowsAffected : int
  method open_ : string -> unit
  method execute : string -> resultSet
  method execute_bind_ : 'a. string -> 'a array -> resultSet
  method close : unit
  method remove : unit
end

class type icons =
object
  method _get_128x128 : string
  method _get_48x48 : string
  method _get_32x32 : string
  method _get_16x16 : string
end

class type file =
object
  method _get_name : string
  method _get_blob : blob
end

class type openFileOptions =
object
  method _get_singleFile : bool
  method _get_filter : string array
end

class type desktop =
object
  method createShortcut : string -> string -> icons -> string -> unit
  method openFiles : (file array -> unit) -> openFileOptions -> unit
end

class type address =
object
  method _get_streetNumber : string
  method _get_street : string
  method _get_premises : string
  method _get_city : string
  method _get_county : string
  method _get_region : string
  method _get_country : string
  method _get_countryCode : string
  method _get_postalCode : string
end

class type position =
object
  method _get_latitude : float
  method _get_longitude : float
  method _get_accuracy : float
  method _get_altitude : float
  method _get_altitudeAccuracy : float
  method _get_timestamp : Javascript.date
  method _get_gearsAddress : address
end

class type positionError =
object
  method _get_code : int
  method _get_message : string
end

class type positionOptions =
object
  method _get_enableHighAccuracy : bool
  method _get_gearsRequestAddress : bool
  method _get_gearsAddressLanaguage : string
  method _get_gearsLocationProviderUrls : string array
end

type watch_id

class type geolocation =
object
  method getCurrentPosition : (position -> unit) -> (positionError -> unit) -> positionOptions -> unit
  method watchPosition : (position -> unit) -> (positionError -> unit) -> positionOptions -> watch_id
  method clearWatch : watch_id -> unit
  method getPermission : string -> string -> string -> bool
  method _get_lastPosition : position
end

class type progressEvent =
object
  method _get_total : int
  method _get_loaded : int
  method _get_lengthComputatable : bool
end

class type httpRequestUpload =
object
  method _set_onprogress : (progressEvent -> unit) -> unit
  method _get_onprogress : (progressEvent -> unit)
end

class type httpRequest =
object
  method open_ : string -> string -> unit
  method setRequestHeader : string -> string -> unit
  method send : unit
  method send_string_ : string -> unit
  method send_blob_ : blob -> unit
  method abort : unit
  method getResponseHeader : string -> string
  method getAllResponseHeaders : string
  method _set_onprogress : (progressEvent -> unit) -> unit
  method _get_onprogress : (progressEvent -> unit)
  method _set_onreadystatechange : (unit -> unit) -> unit
  method _get_onreadystatechange : (unit -> unit)
  method _get_readyState : int
  method _get_responseBlob : blob
  method _get_responseText : string
  method _get_status : int
  method _get_statusText : string
  method _get_upload : httpRequestUpload
end

class type managedResourceStore =
object
  method _get_name : string
  method _get_requiredCookie : string
  method _get_enabled : bool
  method _set_enabled : bool -> unit
  method _get_manifestUrl : string
  method _set_manifestUrl : string -> unit
  method _get_lastUpdateCheckTime : int
  method _get_updateStatus : int
  method _get_lastErrorMessage : string
  method _get_currentVersion : string
  method _set_oncomplete : (< _get_newVersion : string > -> unit) -> unit
  method _get_oncomplete : (< _get_newVersion : string > -> unit)
  method _set_onerror : (< _get_message : string > -> unit) -> unit
  method _get_onerror : (< _get_message : string > -> unit)
  method _set_onprogress : (< _get_filesComplete : int; _get_filesTotal : int > -> unit) -> unit
  method _get_onprogress : (< _get_filesComplete : int; _get_filesTotal : int > -> unit)
  method checkForUpdate : unit
end

type capture_id

class type resourceStore =
object
  method _get_name : string
  method _get_requiredCookie : string
  method _get_enabled : bool
  method _set_enabled : bool -> unit
  method capture : string -> (string -> bool -> capture_id -> unit) -> capture_id
  method capture_array_ : string array -> (string -> bool -> capture_id -> unit) -> capture_id
  method abortCapture : capture_id -> unit
  method remove : string -> unit
  method rename : string -> string -> unit
  method copy : string -> string -> unit
  method isCaptured : string -> bool
  method captureBlob : blob -> string -> unit
  method captureBlob_contentType_ : blob -> string -> string -> unit
  method getHeader : string -> string -> string
  method getAllHeaders : string -> string
  method getAsBlob : string -> blob
end

class type localServer =
object
  method canServeLocally : string -> bool
  method createStore : string -> resourceStore
  method createStore_requiredCookie_ : string -> string -> resourceStore
  method openStore : string -> resourceStore
  method openStore_requiredCookie_ : string -> string -> resourceStore
  method removeStore : string -> unit
  method removeStore_requiredCookie_ : string -> string -> unit
  method createManagedStore : string -> managedResourceStore
  method createManagedStore_requiredCookie_ : string -> string -> managedResourceStore
  method openManagedStore : string -> managedResourceStore
  method openManagedStore_requiredCookie_ : string -> string -> managedResourceStore
  method removeManagedStore : string -> unit
  method removeManagedStore_requiredCookie_ : string -> string -> unit
end

type timeout_id
type interval_id

class type timer =
object
  method setTimeout : (unit -> unit) -> float -> timeout_id
  method setTimeout_eval_ : string -> float -> timeout_id
  method clearTimeout : timeout_id -> unit
  method setInterval : (unit -> unit) -> float -> interval_id
  method setInterval_eval_ : string -> float -> interval_id
  method clearInterval : interval_id -> unit
end

type worker_id

class type workerPool =
object
  method _set_onmessage :
    (string ->
     worker_id ->
     < _get_body : 'a; _get_sender : worker_id; _get_origin : string; _get_text : string > ->
     unit) ->
    unit
  method _get_onmessage :
    (string ->
     worker_id ->
     < _get_body : 'a; _get_sender : worker_id; _get_origin : string; _get_text : string > ->
     unit)
  method _set_onerror : (< _get_message : string; _get_lineNumber : int > -> bool) -> unit
  method _get_onerror : (< _get_message : string; _get_lineNumber : int > -> bool)
  method createWorker : string -> worker_id
  method createWorkerFromUrl : string -> worker_id
  method sendMessage : 'a. 'a -> worker_id -> unit
  method allowCrossOrigin : unit
end
