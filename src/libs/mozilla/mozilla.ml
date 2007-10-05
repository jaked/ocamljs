(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 *
 * This library is free software released under the LGPL.
 * See LICENSE for more details.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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

  external queryInterface : <supports:i; ..> t -> interface -> 'a = "#QueryInterface"

  let queryInterface_httpChannel s = queryInterface s httpChannel
  let queryInterface_cookie s = queryInterface s cookie
end

module BufferedInputStream =
struct
  open XPCOM

  external init : <bufferedInputStream:i; ..> t -> <inputStream:i; ..> t -> int -> unit = "#init"
end

module Channel =
struct
  open XPCOM

  external uRI : <channel:i; ..> t -> <uRI:i> t = ".URI"
end

module ConsoleService =
struct
  open XPCOM

  external logStringMessage : <consoleService:i; ..> t -> string -> unit = "#logStringMessage"
end

module CookieManager =
struct
  open XPCOM

  external remove : <cookieManager:i; ..> t -> string -> string -> string -> bool -> unit = "#remove"
  external enumerator : <cookieManager:i; ..> t -> <simpleEnumerator:<supports:i> t> t = ".enumerator"
end

module Cookie =
struct
  open XPCOM

  external name : <cookie:i; ..> t -> string = ".name"
  external value : <cookie:i; ..> t -> string = ".value"
  external host : <cookie:i; ..> t -> string = ".host"
  external path : <cookie:i; ..> t -> string = ".path"
end

module DOMJSWindow =
struct
  open XPCOM

  let w = Ocamljs.var "window"

  type timeout
  type interval

  external _setTimeout : <dOMJSWindow:i; ..> t -> (unit -> unit) -> int -> timeout = "#setTimeout"
  let setTimeout w f d = _setTimeout w (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ())) d
  external clearTimeout : <dOMJSWindow:i; ..> t -> timeout -> unit = "#clearTimeout"
  external _setInterval : <dOMJSWindow:i; ..> t -> (unit -> unit) -> int -> interval = "#setInterval"
  let setInterval w f d = _setInterval w (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ())) d
  external clearInterval : <dOMJSWindow:i; ..> t -> interval -> unit = "#clearInterval"
end

module FileInputStream =
struct
  open XPCOM

  external init : <fileInputStream:i; ..> t -> <file:i; ..> t -> int -> int -> int -> unit = "#init"
end

module File =
struct
  open XPCOM

  external remove : <file:i; ..> t -> bool -> unit = "#remove"
  external append : <file:i; ..> t -> string -> unit = "#append"
end

module FileOutputStream =
struct
  open XPCOM

  external init : <fileOutputStream:i; ..> t -> <file:i; ..> t -> int -> int -> int -> unit = "#init"
end

module HttpChannel =
struct
  open XPCOM

  external setRequestHeader : <httpChannel:i; ..> t -> string -> string -> bool -> unit = "#setRequestHeader"
end

module InputStream =
struct
  open XPCOM

  let coerce is = (is :> <inputStream:i> t)
end

module LocalFile =
struct
  open XPCOM

  external setRelativeDescriptor : <localFile:i; ..> t -> <localFile:i; ..> t -> string -> unit = "#setRelativeDescriptor"
end

module MIMEInputStream =
struct
  open XPCOM

  external set_addContentLength : <mIMEInputStream:i; ..> t -> bool -> unit = "=addContentLength"
  external addHeader : <mIMEInputStream:i; ..> t -> string -> string -> unit = "#addHeader"
  external setData : <mIMEInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#setData"
end

module MultiplexInputStream =
struct
  open XPCOM

  external appendStream : <multiplexInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#appendStream"
end

module ObserverService =
struct
  module X = XPCOM
  module O = Ocamljs

  type tp
  type 'a o

  external _addObserver : <observerService:X.i; ..> X.t -> < ..> o -> string -> bool -> unit = "#addObserver"
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

  external removeObserver : <observerService:X.i; ..> X.t -> < ..> o -> string -> unit = "#removeObserver"

  let removeObserver_http_on_modify_request s o =
    removeObserver s o "http-on-modify-request"
end

module OutputStream =
struct
  open XPCOM

  external write : <outputStream:i; ..> t -> string -> int -> unit = "#write"
  external close : <outputStream:i; ..> t -> unit = "#close"
end

module PasswordManagerInternal =
struct
  open XPCOM

  external findPasswordEntry : <passwordManagerInternal:i; ..> t -> string -> string -> string -> string out -> string out -> string out -> unit = "#findPasswordEntry"
  external addUserFull : <passwordManagerInternal:i; ..> t -> string -> string -> string -> string -> string -> unit = "#addUserFull"

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

  external addUser : <passwordManager:i; ..> t -> string -> string -> string -> unit = "#addUser"
  external removeUser : <passwordManager:i; ..> t -> string -> string -> unit = "#removeUser"
  external addReject : <passwordManager:i; ..> t -> string -> unit = "#addReject"
  external removeReject : <passwordManager:i; ..> t -> string -> unit = "#removeReject"
end

module PrefBranch =
struct
  open XPCOM

  external pREF_BOOL : <prefBranch:i; ..> t -> int = ".PREF_BOOL"
  external pREF_INT : <prefBranch:i; ..> t -> int = ".PREF_INT"
  external pREF_STRING : <prefBranch:i; ..> t -> int = ".PREF_STRING"
  external getPrefType : <prefBranch:i; ..> t -> string -> int = "#getPrefType"
  external getBoolPref : <prefBranch:i; ..> t -> string -> bool = "#getBoolPref"
  external getCharPref : <prefBranch:i; ..> t -> string -> string = "#getCharPref"
  external getIntPref : <prefBranch:i; ..> t -> string -> int = "#getIntPref"
  external setBoolPref : <prefBranch:i; ..> t -> string -> bool -> unit = "#setBoolPref"
  external setCharPref : <prefBranch:i; ..> t -> string -> string -> unit = "#setCharPref"
  external setIntPref : <prefBranch:i; ..> t -> string -> int -> unit = "#setIntPref"

  external getStringPref : <prefBranch:i; ..> t -> string -> string = "#getCharPref"

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

  external get : <properties:i; ..> t -> string -> interface -> 'a = "#get"
  let getFile s p = get s p file
end

module ScriptableInputStream =
struct
  open XPCOM

  external init : <scriptableInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#init"
end

module SimpleEnumerator =
struct
  open XPCOM

  external hasMoreElements : <simpleEnumerator:'a; ..> t -> bool = "#hasMoreElements"
  external getNext : <simpleEnumerator:'a; ..> t -> 'a = "#getNext"
end

module StringInputStream =
struct
  open XPCOM

  external setData : <stringInputStream:i; ..> t -> string -> int -> unit = "#setData"
end

module URI =
struct
  open XPCOM

  external spec : <uRI:i; ..> t -> string = ".spec"
end

module WindowMediator =
struct
  open XPCOM

  external _getEnumerator : <windowMediator:i; ..> t -> string -> <simpleEnumerator:<supports:i> t> t = "#getEnumerator"

  let getEnumerator w t = _getEnumerator w (Ocamljs.nullable_of_option t)
end

module XMLHttpRequest =
struct
  open XPCOM

  external new_ : unit -> <xMLHttpRequest:i> t = "$new" "XMLHttpRequest"
  external _set_onreadystatechange : <xMLHttpRequest:i; ..> t -> (unit -> unit) -> unit = "=onreadystatechange"
  let set_onreadystatechange x f = _set_onreadystatechange x (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ()))
  external _set_onload : <xMLHttpRequest:i; ..> t -> (unit -> unit) -> unit = "=onload"
  let set_onload x f = _set_onload x (Ocamljs.function_ (fun () -> Ocamljs.caml_callback f ()))
  external open_ : <xMLHttpRequest:i; ..> t -> string -> string -> bool -> unit = "#open"
  external setRequestHeader : <xMLHttpRequest:i; ..> t -> string -> string -> unit = "#setRequestHeader"
  external getResponseHeader : <xMLHttpRequest:i; ..> t -> string -> string = "#getResponseHeader"
  external overrideMimeType : <xMLHttpRequest:i; ..> t -> string -> unit = "#overrideMimeType"
  external send : <xMLHttpRequest:i; ..> t -> <inputStream:i; ..> t -> unit = "#send"
  external readyState : <xMLHttpRequest:i; ..> t -> int = ".readyState"
  external responseText : <xMLHttpRequest:i; ..> t -> string = ".responseText"
  external channel : <xMLHttpRequest:i; ..> t -> <supports:i; channel:i> t = ".channel"
  external abort : <xMLHttpRequest:i; ..> t -> unit = "#abort"
  external status : <xMLHttpRequest:i; ..> t -> int = ".status"
end

module DOM =
struct
  type i
  type (+'a) t
end

module Document =
struct
  open DOM

  let d = Ocamljs.var "document"

  external getElementById : <document:i; ..> t -> string -> <element:i> t = "#getElementById"

  (* XXX
     these are just advisory. it would be nice to compile out XUL files
     along with a module you can use to get at the elements in a
     typesafe way. this of course wouldn't work if you go changing the
     DOM around.
  *)
  external getElementById_dialog : <document:i; ..> t -> string -> <element:i; dialog:i> t = "#getElementById"
  external getElementById_menuItem : <document:i; ..> t -> string -> <element:i; menuItem:i> t = "#getElementById"
  external getElementById_menuList : <document:i; ..> t -> string -> <element:i; menuList:i> t = "#getElementById"
  external getElementById_statusBarPanel : <document:i; ..> t -> string -> <element:i; statusBarPanel:i> t = "#getElementById"
  external getElementById_textBox : <document:i; ..> t -> string -> <element:i; textBox:i> t = "#getElementById"
end

module Element =
struct
  open DOM

  type e
  type 'a l

  external _addEventListener : <element:i; ..> t -> string -> (< ..> -> bool) -> bool -> unit = "#addEventListener"
  let addEventListener o e f u =
    let l = Ocamljs.function_ (fun a -> Ocamljs.caml_callback f a) in
    _addEventListener o e l u;
    l

  let addEventListener_command o f u = addEventListener o "command" f u
  let addEventListener_click o f u = addEventListener o "click" f u
  let addEventListener_dialogaccept o f u = addEventListener o "dialogaccept" f u
  let addEventListener_load o f u = addEventListener o "load" f u
  let addEventListener_unload o f u = addEventListener o "unload" f u

  external removeEventListener : <element:i; ..> t -> string -> < ..> l -> bool -> unit = "#removeEventListener"

  let removeEventListener_command o l u = removeEventListener o "command" l u
  let removeEventListener_click o l u = removeEventListener o "click" l u
  let removeEventListener_dialogaccept o l u = removeEventListener o "dialogaccept" l u
  let removeEventListener_load o l u = removeEventListener o "load" l u
  let removeEventListener_unload o l u = removeEventListener o "unload" l u

  external setAttribute : <element:i; ..> t -> string -> 'a -> unit = "#setAttribute"
end

module MenuList =
struct
  open DOM

  external selectedIndex : <menuList:i; ..> t -> int = ".selectedIndex"
  external set_selectedIndex : <menuList:i; ..> t -> int -> unit = "=selectedIndex"
  external value : <menuList:i; ..> t -> string = ".value"
  external set_value : <menuList:i; ..> t -> string -> unit = "=value"
end

module MouseEvent =
struct
  open DOM

  external button : <mouseEvent:i> t -> int = ".button"
end

module TextBox =
struct
  open DOM

  external value : <textBox:i; ..> t -> string = ".value"
  external set_value : <textBox:i; ..> t -> string -> unit = "=value"
end

module Window =
struct
  open DOM

  let w = Ocamljs.var "window"

  external openDialog : <window:i; ..> t -> string -> string -> string -> unit = "#openDialog"
  external location : <window:i; ..> t -> string = ".location"
end
