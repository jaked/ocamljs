(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
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

module XPCOM =
struct
  type class_
  type interface
  type result

  external createInstance : class_ -> interface -> 'a = "#createInstance"
  external getService : class_ -> interface -> 'a = "#getService"

  let cc c = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "classes") c
  let ci i = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "interfaces") i
  let cr r = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "results") r

  let bufferedInputStream = ci "nsIBufferedInputStream"
  let consoleService = ci "nsIConsoleService"
  let cookie = ci "nsICookie"
  let cookieManager = ci "nsICookieManager"
  let dOMJSWindow = ci "nsIDOMJSWindow"
  let file = ci "nsIFile"
  let fileInputStream = ci "nsIFileInputStream"
  let fileOutputStream = ci "nsIFileOutputStream"
  let httpChannel = ci "nsIHttpChannel"
  let localFile = ci "nsILocalFile"
  let mIMEInputStream = ci "nsIMIMEInputStream"
  let multiplexInputStream = ci "nsIMultiplexInputStream"
  let observer = ci "nsIObserver"
  let observerService = ci "nsIObserverService"
  let passwordManager = ci "nsIPasswordManager"
  let passwordManagerInternal = ci "nsIPasswordManagerInternal"
  let prefBranch = ci "nsIPrefBranch"
  let properties = ci "nsIProperties"
  let scriptableInputStream = ci "nsIScriptableInputStream"
  let stringInputStream = ci "nsIStringInputStream"
  let supports = ci "nsISupports"
  let windowMediator = ci "nsIWindowMediator"

  let appshell_window_mediator = cc "@mozilla.org/appshell/window-mediator;1"
  let consoleservice = cc "@mozilla.org/consoleservice;1"
  let cookiemanager = cc "@mozilla.org/cookiemanager;1"
  let file_directory_service = cc "@mozilla.org/file/directory_service;1"
  let file_local = cc "@mozilla.org/file/local;1"
  let io_multiplex_input_stream = cc "@mozilla.org/io/multiplex-input-stream;1"
  let io_string_input_stream = cc "@mozilla.org/io/string-input-stream;1"
  let network_buffered_input_stream = cc "@mozilla.org/network/buffered-input-stream;1"
  let network_file_input_stream = cc "@mozilla.org/network/file-input-stream;1"
  let network_file_output_stream = cc "@mozilla.org/network/file-output-stream;1"
  let network_mime_input_stream = cc "@mozilla.org/network/mime-input-stream;1"
  let observer_service = cc "@mozilla.org/observer-service;1"
  let passwordmanager = cc "@mozilla.org/passwordmanager;1"
  let preferences_service = cc "@mozilla.org/preferences-service;1"
  let scriptableinputstream = cc "@mozilla.org/scriptableinputstream;1"

  let nOINTERFACE = cr "NS_NOINTERFACE"

  type i
  type (+'a) t

  class type bufferedInputStream = object method inputStream:i method bufferedInputStream:i end
  class type channel = object method supports:i method channel:i end
  class type consoleService = object method consoleService:i end
  class type cookie = object method cookie:i end
  class type cookieManager = object method cookieManager:i end
  class type dOMJSWindow = object method dOMJSWindow:i end
  class type file = object method file:i end
  class type fileInputStream = object method inputStream:i method fileInputStream:i end
  class type fileOutputStream = object method outputStream:i method fileOutputStream:i end
  class type httpChannel = object method httpChannel:i end
  class type inputStream = object method inputStream:i end
  class type localFile = object method file:i method localFile:i end
  class type mIMEInputStream = object method inputStream:i method mIMEInputStream:i end
  class type multiplexInputStream = object method inputStream:i method multiplexInputStream:i end
  class type observerService = object method observerService:i end
  class type outputStream = object method outputStream:i end
  class type passwordManager = object method passwordManager:i end
  class type passwordManagerInternal = object method passwordManagerInternal:i end
  class type prefBranch = object method prefBranch:i end
  class type properties = object method properties:i end
  class type scriptableInputStream = object method inputStream:i method scriptableInputStream:i end
  class type ['a] simpleEnumerator = object method simpleEnumerator:i end
  class type stringInputStream = object method inputStream:i method stringInputStream:i end
  class type supports = object method supports:i end
  class type uRI = object method uRI:i end
  class type windowMediator = object method windowMediator:i end
  class type xMLHttpRequest = object method xMLHttpRequest:i end

  let getService_appshell_window_mediator () = getService appshell_window_mediator windowMediator
  let getService_consoleservice () = getService consoleservice consoleService
  let getService_cookiemanager () = getService cookiemanager cookieManager
  let getService_file_directory_service () = getService file_directory_service properties
  let getService_observer_service () = getService observer_service observerService
  let getService_passwordmanager_passwordManager () = getService passwordmanager passwordManager
  let getService_passwordmanager_passwordManagerInternal () = getService passwordmanager passwordManagerInternal
  let getService_preferences_service () = getService preferences_service prefBranch

  let createInstance_file_local () = createInstance file_local localFile
  let createInstance_network_buffered_input_stream () = createInstance network_buffered_input_stream bufferedInputStream
  let createInstance_network_file_input_stream () = createInstance network_file_input_stream fileInputStream
  let createInstance_network_file_output_stream () = createInstance network_file_output_stream fileOutputStream
  let createInstance_network_mime_input_stream () = createInstance network_mime_input_stream mIMEInputStream
  let createInstance_io_multiplex_input_stream () = createInstance io_multiplex_input_stream multiplexInputStream
  let createInstance_io_string_input_stream () = createInstance io_string_input_stream stringInputStream
  let createInstance_scriptableinputstream () = createInstance scriptableinputstream scriptableInputStream

  type 'a out

  let out v = Ocamljs.obj [ "value", v ]
  external outv : 'a out -> 'a = ".value"
end

module Supports =
struct
  open XPCOM

  external queryInterface : #supports t -> interface -> 'a = "#QueryInterface"

  let queryInterface_httpChannel s = queryInterface s httpChannel
  let queryInterface_cookie s = queryInterface s cookie
end

module BufferedInputStream =
struct
  open XPCOM

  external init : #bufferedInputStream t -> #inputStream t -> int -> unit = "#init"
end

module Channel =
struct
  open XPCOM

  external uRI : #channel t -> uRI t = ".URI"
end

module ConsoleService =
struct
  open XPCOM

  external logStringMessage : #consoleService t -> string -> unit = "#logStringMessage"
end

module CookieManager =
struct
  open XPCOM

  external remove : #cookieManager t -> string -> string -> string -> bool -> unit = "#remove"
  external enumerator : #cookieManager t -> (supports t) simpleEnumerator t = ".enumerator"
end

module Cookie =
struct
  open XPCOM

  external name : #cookie t -> string = ".name"
  external value : #cookie t -> string = ".value"
  external host : #cookie t -> string = ".host"
  external path : #cookie t -> string = ".path"
end

module DOMJSWindow =
struct
  open XPCOM

  let w = Ocamljs.var "window"

  type timeout
  type interval

  external _setTimeout : #dOMJSWindow t -> (unit -> unit) -> float -> timeout = "#setTimeout"
  let setTimeout w f d = _setTimeout w (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ())) d
  external clearTimeout : #dOMJSWindow t -> timeout -> unit = "#clearTimeout"
  external _setInterval : #dOMJSWindow t -> (unit -> unit) -> float -> interval = "#setInterval"
  let setInterval w f d = _setInterval w (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ())) d
  external clearInterval : #dOMJSWindow t -> interval -> unit = "#clearInterval"
end

module FileInputStream =
struct
  open XPCOM

  external init : #fileInputStream t -> #file t -> int -> int -> int -> unit = "#init"
end

module File =
struct
  open XPCOM

  external remove : #file t -> bool -> unit = "#remove"
  external append : #file t -> string -> unit = "#append"
end

module FileOutputStream =
struct
  open XPCOM

  external init : #fileOutputStream t -> #file t -> int -> int -> int -> unit = "#init"
end

module HttpChannel =
struct
  open XPCOM

  external setRequestHeader : #httpChannel t -> string -> string -> bool -> unit = "#setRequestHeader"
end

module InputStream =
struct
  open XPCOM

  let coerce is = (is :> inputStream t)
end

module LocalFile =
struct
  open XPCOM

  external setRelativeDescriptor : #localFile t -> #localFile t -> string -> unit = "#setRelativeDescriptor"
end

module MIMEInputStream =
struct
  open XPCOM

  external set_addContentLength : #mIMEInputStream t -> bool -> unit = "=addContentLength"
  external addHeader : #mIMEInputStream t -> string -> string -> unit = "#addHeader"
  external setData : #mIMEInputStream t -> #inputStream t -> unit = "#setData"
end

module MultiplexInputStream =
struct
  open XPCOM

  external appendStream : #multiplexInputStream t -> #inputStream t -> unit = "#appendStream"
end

module ObserverService =
struct
  module X = XPCOM
  module O = Ocamljs

  type tp
  type 'a o

  class type none = object end
  class type httpOnModifyRequest = object method httpOnModifyRequest:tp end

  external _addObserver : #X.observerService X.t -> #none o -> string -> bool -> unit = "#addObserver"
  let addObserver s f t =
    let o = O.obj [ "observe", O.function_ (fun s t d -> O.caml_callback3 f s t d);
                    "QueryInterface", O.function_ (fun iid -> (* do we need this? *)
                      if iid = X.supports || iid = X.observer
                      then O.this()
                      else O.throw X.nOINTERFACE) ] in
    _addObserver s o t false;
    o

  let addObserver_http_on_modify_request s f =
    addObserver s (fun s t d -> f (Supports.queryInterface_httpChannel s) d) "http-on-modify-request"

  external removeObserver : #X.observerService X.t -> #none o -> string -> unit = "#removeObserver"

  let removeObserver_http_on_modify_request s o =
    removeObserver s o "http-on-modify-request"
end

module OutputStream =
struct
  open XPCOM

  external write : #outputStream t -> string -> int -> unit = "#write"
  external close : #outputStream t -> unit = "#close"
end

module PasswordManagerInternal =
struct
  open XPCOM

  external findPasswordEntry : #passwordManagerInternal t -> string -> string -> string -> string out -> string out -> string out -> unit = "#findPasswordEntry"
  external addUserFull : #passwordManagerInternal t -> string -> string -> string -> string -> string -> unit = "#addUserFull"

  let findEntry s h u p =
    let h2 = out "" in
    let u2 = out "" in
    let p2 = out "" in
    try
      findPasswordEntry s h u p h2 u2 p2;
      Some (outv h2, outv u2, outv p2)
    with e -> None

  let addEntry s h u p =
    addUserFull s h u p "" ""
end

module PasswordManager =
struct
  open XPCOM

  external addUser : #passwordManager t -> string -> string -> string -> unit = "#addUser"
  external removeUser : #passwordManager t -> string -> string -> unit = "#removeUser"
  external addReject : #passwordManager t -> string -> unit = "#addReject"
  external removeReject : #passwordManager t -> string -> unit = "#removeReject"
end

module PrefBranch =
struct
  open XPCOM

  external pREF_BOOL : #prefBranch t -> int = ".PREF_BOOL"
  external pREF_INT : #prefBranch t -> int = ".PREF_INT"
  external pREF_STRING : #prefBranch t -> int = ".PREF_STRING"
  external getPrefType : #prefBranch t -> string -> int = "#getPrefType"
  external getBoolPref : #prefBranch t -> string -> bool = "#getBoolPref"
  external getCharPref : #prefBranch t -> string -> string = "#getCharPref"
  external getIntPref : #prefBranch t -> string -> int = "#getIntPref"
  external setBoolPref : #prefBranch t -> string -> bool -> unit = "#setBoolPref"
  external setCharPref : #prefBranch t -> string -> string -> unit = "#setCharPref"
  external setIntPref : #prefBranch t -> string -> int -> unit = "#setIntPref"

  external getStringPref : #prefBranch t -> string -> string = "#getCharPref"

  type pref = Bool of bool | Int of int | String of string | None
  let getPref s p =
    try
      let t = getPrefType s p in
      if t = pREF_BOOL s then Bool (getBoolPref s p)
      else if t = pREF_INT s then Int (getIntPref s p)
      else if t = pREF_STRING s then String (getCharPref s p)
      else None
    with _ -> None
end

module Properties =
struct
  open XPCOM

  external get : #properties t -> string -> interface -> 'a = "#get"
  let getFile s p = get s p file
end

module ScriptableInputStream =
struct
  open XPCOM

  external init : #scriptableInputStream t -> #inputStream t -> unit = "#init"
end

module SimpleEnumerator =
struct
  open XPCOM

  external hasMoreElements : 'a #simpleEnumerator t -> bool = "#hasMoreElements"
  external getNext : 'a #simpleEnumerator t -> 'a = "#getNext"
end

module StringInputStream =
struct
  open XPCOM

  external setData : #stringInputStream t -> string -> int -> unit = "#setData"
end

module URI =
struct
  open XPCOM

  external spec : #uRI t -> string = ".spec"
end

module WindowMediator =
struct
  open XPCOM

  external _getEnumerator : #windowMediator t -> string -> (supports t) simpleEnumerator t = "#getEnumerator"

  let getEnumerator w t = _getEnumerator w (Ocamljs.nullable_of_option t)
end

module XMLHttpRequest =
struct
  open XPCOM

  external new_ : unit -> xMLHttpRequest t = "$new" "XMLHttpRequest"
  external _set_onreadystatechange : #xMLHttpRequest t -> (unit -> unit) -> unit = "=onreadystatechange"
  let set_onreadystatechange x f = _set_onreadystatechange x (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ()))
  external _set_onload : #xMLHttpRequest t -> (unit -> unit) -> unit = "=onload"
  let set_onload x f = _set_onload x (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ()))
  external open_ : #xMLHttpRequest t -> string -> string -> bool -> unit = "#open"
  external setRequestHeader : #xMLHttpRequest t -> string -> string -> unit = "#setRequestHeader"
  external getResponseHeader : #xMLHttpRequest t -> string -> string = "#getResponseHeader"
  external overrideMimeType : #xMLHttpRequest t -> string -> unit = "#overrideMimeType"
  external send : #xMLHttpRequest t -> #inputStream t -> unit = "#send"
  external readyState : #xMLHttpRequest t -> int = ".readyState"
  external responseText : #xMLHttpRequest t -> string = ".responseText"
  external channel : #xMLHttpRequest t -> channel t = ".channel"
  external abort : #xMLHttpRequest t -> unit = "#abort"
  external status : #xMLHttpRequest t -> int = ".status"
end

module DOM =
struct
  type i
  type (+'a) t

  (* XXX do these fall into some more sensible hierarchy than element -> everything else? *)
  class type button = object method element:i method button:i end
  class type deck = object method element:i method deck:i end
  class type dialog = object method element:i method dialog:i end
  class type document = object method document:i end
  class type element = object method element:i end
  class type event = object method event:i end
  class type label = object method label:i end
  class type menuItem = object method element:i method menuItem:i end
  class type menuList = object method element:i method menuList:i end
  class type mouseEvent = object method event:i method mouseEvent:i end
  class type statusBarPanel = object method element:i method statusBarPanel:i end
  class type stringBundle = object method stringBundle:i end
  class type style = object method style:i end
  class type textBox = object method element:i method textBox:i end
  class type radio = object method element:i method radio:i end
  class type window = object method element:i method window:i end
end

module Document =
struct
  open DOM

  let d = Ocamljs.var "document"

  external getElementById : #document t -> string -> element t = "#getElementById"

  (* XXX
     these are just advisory. it would be nice to compile out XUL files
     along with a module you can use to get at the elements in a
     typesafe way. this of course wouldn't work if you go changing the
     DOM around.
  *)
  external getElementById_button : #document t -> string -> button t = "#getElementById"
  external getElementById_deck : #document t -> string -> deck t = "#getElementById"
  external getElementById_dialog : #document t -> string -> dialog t = "#getElementById"
  external getElementById_label : #document t -> string -> label t = "#getElementById"
  external getElementById_menuItem : #document t -> string -> menuItem t = "#getElementById"
  external getElementById_menuList : #document t -> string -> menuList t = "#getElementById"
  external getElementById_radio : #document t -> string -> radio t = "#getElementById"
  external getElementById_statusBarPanel : #document t -> string -> statusBarPanel t = "#getElementById"
  external getElementById_stringBundle : #document t -> string -> stringBundle t = "#getElementById"
  external getElementById_textBox : #document t -> string -> textBox t = "#getElementById"
end

module Element =
struct
  open DOM

  type e
  type 'a l

  class type none = object end
  class type click = object method click:e end
  class type command = object method command:e end
  class type dialogaccept = object method dialogaccept:e end
  class type load = object method load:e end
  class type unload = object method unload:e end

  external _addEventListener : #element t -> string -> (#none -> bool) -> bool -> unit = "#addEventListener"
  let addEventListener o e f u =
    let l = Ocamljs.function_ (fun a -> Ocamljs.caml_callback f a) in
    _addEventListener o e l u;
    l

  let addEventListener_command o f u = addEventListener o "command" f u
  let addEventListener_click o f u = addEventListener o "click" f u
  let addEventListener_dialogaccept o f u = addEventListener o "dialogaccept" f u
  let addEventListener_load o f u = addEventListener o "load" f u
  let addEventListener_unload o f u = addEventListener o "unload" f u

  external removeEventListener : #element t -> string -> #none l -> bool -> unit = "#removeEventListener"

  let removeEventListener_command o l u = removeEventListener o "command" l u
  let removeEventListener_click o l u = removeEventListener o "click" l u
  let removeEventListener_dialogaccept o l u = removeEventListener o "dialogaccept" l u
  let removeEventListener_load o l u = removeEventListener o "load" l u
  let removeEventListener_unload o l u = removeEventListener o "unload" l u

  external getAttribute : #element t -> string -> 'a = "#getAttribute"
  external setAttribute : #element t -> string -> 'a -> unit = "#setAttribute"

  external hidden : #element t -> bool = ".hidden"
  external set_hidden : #element t -> bool -> unit = "=hidden"

  external style: #element t -> style t = ".style"
end

module Button =
struct
  open DOM

  external disabled : #button t -> bool = ".disabled"
  external set_disabled : #button t -> bool -> unit = "=disabled"
  external label : #button t -> string = ".label"
  external set_label : #button t -> string -> unit = "=label"
end

module Deck =
struct
  open DOM

  external selectedIndex : #deck t -> int = ".selectedIndex"
  external set_selectedIndex : #deck t -> int -> unit = "=selectedIndex"
end

module Label =
struct
  open DOM

  external value : #label t -> string = ".value"
  external set_value: #label t -> string -> unit = "=value"
end

module MenuList =
struct
  open DOM

  external selectedIndex : #menuList t -> int = ".selectedIndex"
  external set_selectedIndex : #menuList t -> int -> unit = "=selectedIndex"
  external value : #menuList t -> string = ".value"
  external set_value : #menuList t -> string -> unit = "=value"
end

module MouseEvent =
struct
  open DOM

  external button : #mouseEvent t -> int = ".button"
end

module Radio =
struct
  open DOM

  external selected : #radio t -> bool = ".selected"
  external set_selected : #radio t -> bool -> unit = "=selected"
end

module StringBundle =
struct
  open DOM

  external getString : #stringBundle t -> string -> string = "#getString"
end

module Style =
struct
  open DOM

  external visibility : #style t -> string = ".visibility"
  external set_visibility : #style t -> string -> unit = "=visibility"
end

module TextBox =
struct
  open DOM

  external value : #textBox t -> string = ".value"
  external set_value : #textBox t -> string -> unit = "=value"
end

module Window =
struct
  open DOM

  let w = Ocamljs.var "window"

  external close : #window t -> unit = "#close"
  external location : #window t -> string = ".location"
  external openDialog : #window t -> string -> string -> string -> unit = "#openDialog"
end
