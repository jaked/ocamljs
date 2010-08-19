(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2009 Skydeck, Inc
 * Copyright (C) 2010 Jake Donham
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA
 *)

(** Google Maps binding *)
(**
   [Gmaps] is a binding of the Google Maps API.
*)

class type gLatLng =
object
end

external new_GLatLng : float -> float -> gLatLng = "$new" "GLatLng"

class type gLatLngBounds =
object
end

external new_GLatLngBounds : gLatLng -> gLatLng -> gLatLngBounds = "$new" "GLatLngBounds"

class type gCopyright =
object
end

external new_GCopyright : string -> gLatLngBounds -> int -> string -> gCopyright = "$new" "GCopyright"

class type gCopyrightCollection =
object
  method addCopyright : gCopyright -> unit
end

external new_GCopyrightCollection : unit -> gCopyrightCollection = "$new" "GCopyrightCollection"

class type gPoint =
object
  method _get_x : int
  method _get_y : int
end

external new_GPoint : int -> int -> gPoint = "$new" "GPoint"

class type gSize =
object
end

external new_GSize : int -> int -> gSize = "$new" "GSize"

class type gIcon =
object
  method _set_image : string -> unit
  method _set_shadow : string -> unit
  method _set_iconSize : gSize -> unit
  method _set_shadowSize : gSize -> unit
  method _set_iconAnchor : gPoint -> unit
  method _set_infoWindowAnchor : gPoint -> unit
end

external new_GIcon : unit -> gIcon = "$new" "GIcon"

class type gOverlay =
object
end

class type gMap2 =
object
  method setCenter_zoom_ : gLatLng -> int -> unit
  method addOverlay : #gOverlay -> unit
  method panTo : gLatLng -> unit
end

external new_GMap2 : Dom.element -> gMap2 = "$new" "GMap2"

class type gTileLayer =
object
  method _set_getTileUrl : (gPoint -> int -> string) -> unit
  method _set_isPng : (unit -> bool) -> unit
  method _set_getOpacity : (unit -> float) -> unit
end

external new_GTileLayer : gCopyrightCollection -> int -> int -> gTileLayer = "$new" "GTileLayer"

class type gTileLayerOverlay =
object
  inherit gOverlay
end

external new_GTileLayerOverlay : gTileLayer -> gTileLayerOverlay = "$new" "GTileLayerOverlay"

class type gMarker =
object
  inherit gOverlay

  method openInfoWindow : #Dom.node -> unit
  method openInfoWindow_string_ : string -> unit
end

external new_GMarker : gLatLng -> gIcon -> gMarker = "$new" "GMarker"
