class type factory =
object
  method create : string -> < .. >
  method getBuildInfo : string
  method getPermission : string -> string -> string -> bool
  method _get_version : string
  method _get_hasPermission : bool
end

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
  method open__ : string -> unit
  method execute : string -> resultSet
  method execute_bind_ : string -> 'a array -> resultSet
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
  method openFiles : (file array -> unit) Ocamljs.jsfun -> openFileOptions -> unit
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
  method getCurrentPosition : (position -> unit) Ocamljs.jsfun -> (positionError -> unit) Ocamljs.jsfun -> positionOptions -> unit
  method watchPosition : (position -> unit) Ocamljs.jsfun -> (positionError -> unit) Ocamljs.jsfun -> positionOptions -> watch_id
  method clearWatch : watch_id -> unit
  method getPermission : string -> string -> string -> bool
  method _get_lastPosition : position
end
