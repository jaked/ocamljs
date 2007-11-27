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

module XPCOM :
sig
  type class_
  type interface
  type result

  external createInstance : class_ -> interface -> 'a = "#createInstance"
  external getService : class_ -> interface -> 'a = "#getService"

  val bufferedInputStream : interface
  val consoleService : interface
  val cookie : interface
  val cookieManager : interface
  val dOMJSWindow : interface
  val file : interface
  val fileInputStream : interface
  val fileOutputStream : interface
  val httpChannel : interface
  val localFile : interface
  val mIMEInputStream : interface
  val multiplexInputStream : interface
  val observer : interface
  val observerService : interface
  val passwordManager : interface
  val passwordManagerInternal : interface
  val prefBranch : interface
  val properties : interface
  val scriptableInputStream : interface
  val stringInputStream : interface
  val supports : interface
  val windowMediator : interface

  val appshell_window_mediator : class_
  val consoleservice : class_
  val cookiemanager : class_
  val file_directory_service : class_
  val file_local : class_
  val io_multiplex_input_stream : class_
  val io_string_input_stream : class_
  val network_buffered_input_stream : class_
  val network_file_input_stream : class_
  val network_file_output_stream : class_
  val network_mime_input_stream : class_
  val observer_service : class_
  val passwordmanager : class_
  val preferences_service : class_
  val scriptableinputstream : class_

  val nOINTERFACE : result

  (*
    We write the type of an object supporting interfaces Foo and Bar as
    <foo:i; bar:i> t, and the type of a function arg that takes an object
    supporting interface Foo as <foo:i; ..> t.

    There is no notion of subtyping among object fields, so if Baz is a
    subinterface of Bar you must write the type of an object supporting
    Baz as <bar:i; baz> t rather than just <baz:i> t if you want to call
    Bar methods on the object.
  *)
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

  val getService_appshell_window_mediator : unit -> windowMediator t
  val getService_consoleservice : unit -> consoleService t
  val getService_cookiemanager : unit -> cookieManager t
  val getService_file_directory_service : unit -> properties t
  val getService_observer_service : unit -> observerService t
  val getService_passwordmanager_passwordManager : unit -> passwordManager t
  val getService_passwordmanager_passwordManagerInternal : unit -> passwordManagerInternal t
  val getService_preferences_service : unit -> prefBranch t

  val createInstance_file_local : unit -> localFile t
  val createInstance_network_buffered_input_stream : unit -> bufferedInputStream t
  val createInstance_network_file_input_stream : unit -> fileInputStream t
  val createInstance_network_file_output_stream : unit -> fileOutputStream t
  val createInstance_network_mime_input_stream : unit -> mIMEInputStream t
  val createInstance_io_multiplex_input_stream : unit -> multiplexInputStream t
  val createInstance_io_string_input_stream : unit -> stringInputStream t
  val createInstance_scriptableinputstream : unit -> scriptableInputStream t

  type 'a out

  val out : 'a -> 'a out
  external outv : 'a out -> 'a = ".value"
end

module BufferedInputStream :
sig
  open XPCOM

  external init : #bufferedInputStream t -> #inputStream t -> int -> unit = "#init"
end

module Channel :
sig
  open XPCOM

  external uRI : #channel t -> uRI t = ".URI"
end

module ConsoleService :
sig
  open XPCOM

  external logStringMessage : #consoleService t -> string -> unit = "#logStringMessage"
end

module CookieManager :
sig
  open XPCOM

  external remove : #cookieManager t -> string -> string -> string -> bool -> unit = "#remove"
  external enumerator : #cookieManager t -> (supports t) simpleEnumerator t = ".enumerator"
end

module Cookie :
sig
  open XPCOM

  external name : #cookie t -> string = ".name"
  external value : #cookie t -> string = ".value"
  external host : #cookie t -> string = ".host"
  external path : #cookie t -> string = ".path"
end

module DOMJSWindow :
sig
  open XPCOM

  val w : dOMJSWindow t

  type timeout
  type interval

  val setTimeout : #dOMJSWindow t -> (unit -> unit) -> float -> timeout
  external clearTimeout : #dOMJSWindow t -> timeout -> unit = "#clearTimeout"
  val setInterval : #dOMJSWindow t -> (unit -> unit) -> float -> interval
  external clearInterval : #dOMJSWindow t -> interval -> unit = "#clearInterval"
end

module FileInputStream :
sig
  open XPCOM

  external init : #fileInputStream t -> #file t -> int -> int -> int -> unit = "#init"
end

module File :
sig
  open XPCOM

  external remove : #file t -> bool -> unit = "#remove"
  external append : #file t -> string -> unit = "#append"
end

module FileOutputStream :
sig
  open XPCOM

  external init : #fileOutputStream t -> #file t -> int -> int -> int -> unit = "#init"
end

module HttpChannel :
sig
  open XPCOM

  external setRequestHeader : #httpChannel t -> string -> string -> bool -> unit = "#setRequestHeader"
end

module InputStream :
sig
  open XPCOM

  val coerce : #inputStream t -> inputStream t
end

module LocalFile :
sig
  open XPCOM

  external setRelativeDescriptor : #localFile t -> #localFile t -> string -> unit = "#setRelativeDescriptor"
end

module MIMEInputStream :
sig
  open XPCOM

  external set_addContentLength : #mIMEInputStream t -> bool -> unit = "=addContentLength"
  external addHeader : #mIMEInputStream t -> string -> string -> unit = "#addHeader"
  external setData : #mIMEInputStream t -> #inputStream t -> unit = "#setData"
end

module MultiplexInputStream :
sig
  open XPCOM

  external appendStream : #multiplexInputStream t -> #inputStream t -> unit = "#appendStream"
end

module ObserverService :
sig
  open XPCOM

  (*
    we wrap up the observation function in an object; this object must
    be passed to removeObserver. so we return it from addObserver and
    take it in removeObserver.

    it appears to be not OK to add/remove an observer inside an observer
    callback.
  *)

  type tp
  type 'a o

  class type none = object end
  class type httpOnModifyRequest = object method httpOnModifyRequest:tp end

  val addObserver : #observerService t -> (supports t -> string -> string -> unit) -> string -> none o
  val addObserver_http_on_modify_request : #observerService t -> (httpChannel t -> string -> unit) -> httpOnModifyRequest o

  external removeObserver : #observerService t -> #none o -> string -> unit = "#removeObserver"
  val removeObserver_http_on_modify_request : #observerService t -> #httpOnModifyRequest o -> unit
end

module OutputStream :
sig
  open XPCOM

  external write : #outputStream t -> string -> int -> unit = "#write"
  external close : #outputStream t -> unit = "#close"
end

module PasswordManagerInternal :
sig
  open XPCOM

  external findPasswordEntry : #passwordManagerInternal t -> string -> string -> string -> string out -> string out -> string out -> unit = "#findPasswordEntry"
  external addUserFull : #passwordManagerInternal t -> string -> string -> string -> string -> string -> unit = "#addUserFull"

  val findEntry : #passwordManagerInternal t -> string -> string -> string -> (string * string * string) option
  val addEntry : #passwordManagerInternal t -> string -> string -> string -> unit
end

module PasswordManager :
sig
  open XPCOM

  external addUser : #passwordManager t -> string -> string -> string -> unit = "#addUser"
  external removeUser : #passwordManager t -> string -> string -> unit = "#removeUser"
  external addReject : #passwordManager t -> string -> unit = "#addReject"
  external removeReject : #passwordManager t -> string -> unit = "#removeReject"
end

module PrefBranch :
sig
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
  val getPref : #prefBranch t -> string -> pref
end

module Properties :
sig
  open XPCOM

  external get : #properties t -> string -> interface -> 'a = "#get"
  val getFile : #properties t -> string -> file t
end

module ScriptableInputStream :
sig
  open XPCOM

  external init : #scriptableInputStream t -> #inputStream t -> unit = "#init"
end

module SimpleEnumerator :
sig
  open XPCOM

  external hasMoreElements : 'a #simpleEnumerator t -> bool = "#hasMoreElements"
  external getNext : 'a #simpleEnumerator t -> 'a = "#getNext"
end

module StringInputStream :
sig
  open XPCOM

  external setData : #stringInputStream t -> string -> int -> unit = "#setData"
end

module Supports :
sig
  open XPCOM

  external queryInterface : #supports t -> interface -> 'a = "#QueryInterface"

  val queryInterface_httpChannel : #supports t -> httpChannel t
  val queryInterface_cookie : #supports t -> cookie t
end

module URI :
sig
  open XPCOM

  external spec : #uRI t -> string = ".spec"
end

module WindowMediator :
sig
  open XPCOM

  val getEnumerator : #windowMediator t -> string option -> (supports t) simpleEnumerator t
end

module XMLHttpRequest :
sig
  open XPCOM

  external new_ : unit -> xMLHttpRequest t = "$new" "XMLHttpRequest"
  val set_onreadystatechange : #xMLHttpRequest t -> (unit -> unit) -> unit
  val set_onload : #xMLHttpRequest t -> (unit -> unit) -> unit
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

module DOM :
sig
  type i
  type (+'a) t

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

module Document :
sig
  open DOM

  val d : document t

  external getElementById : #document t -> string -> element t = "#getElementById"

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

module Element :
sig
  open DOM

  type e
  type 'a l

  class type none = object end
  class type click = object method click:e end
  class type command = object method command:e end
  class type dialogaccept = object method dialogaccept:e end
  class type load = object method load:e end
  class type unload = object method unload:e end

  val addEventListener : #element t -> string -> (#none -> bool) -> bool -> none l

  val addEventListener_command : #element t -> (event t -> bool) -> bool -> command l
  val addEventListener_click : #element t -> (mouseEvent t -> bool) -> bool -> click l
  val addEventListener_dialogaccept : #element t -> (event t -> bool) -> bool -> dialogaccept l
  val addEventListener_load : #element t -> (event t -> bool) -> bool -> load l
  val addEventListener_unload : #element t -> (event t -> bool) -> bool -> unload l

  external removeEventListener : #element t -> string -> #none l -> bool -> unit = "#removeEventListener"

  val removeEventListener_command : #element t -> #command l -> bool -> unit
  val removeEventListener_click : #element t -> #click l -> bool -> unit
  val removeEventListener_dialogaccept : #element t -> #dialogaccept l -> bool -> unit
  val removeEventListener_load : #element t -> #load l -> bool -> unit
  val removeEventListener_unload : #element t -> #unload l -> bool -> unit

  external getAttribute : #element t -> string -> 'a = "#getAttribute"
  external setAttribute : #element t -> string -> 'a -> unit = "#setAttribute"

  external hidden : #element t -> bool = ".hidden"
  external set_hidden : #element t -> bool -> unit = "=hidden"

  external style: #element t -> style t = ".style"
end

module Button :
sig
  open DOM

  external disabled : #button t -> bool = ".disabled"
  external set_disabled : #button t -> bool -> unit = "=disabled"
  external label : #button t -> string = ".label"
  external set_label : #button t -> string -> unit = "=label"
end

module Deck :
sig
  open DOM

  external selectedIndex : #deck t -> int = ".selectedIndex"
  external set_selectedIndex : #deck t -> int -> unit = "=selectedIndex"
end

module Label :
sig
  open DOM

  external value : #label t -> string = ".value"
  external set_value: #label t -> string -> unit = "=value"
end

module MenuList :
sig
  open DOM

  external selectedIndex : #menuList t -> int = ".selectedIndex"
  external set_selectedIndex : #menuList t -> int -> unit = "=selectedIndex"
  external value : #menuList t -> string = ".value"
  external set_value : #menuList t -> string -> unit = "=value"
end

module MouseEvent :
sig
  open DOM

  external button : #mouseEvent t -> int = ".button"
end

module Radio :
sig
  open DOM

  external selected : #radio t -> bool = ".selected"
  external set_selected : #radio t -> bool -> unit = "=selected"
end

module StringBundle :
sig
  open DOM

  external getString : #stringBundle t -> string -> string = "#getString"
end

module Style :
sig
  open DOM

  external visibility : #style t -> string = ".visibility"
  external set_visibility : #style t -> string -> unit = "=visibility"
end

module TextBox :
sig
  open DOM

  external value : #textBox t -> string = ".value"
  external set_value : #textBox t -> string -> unit = "=value"
end

module Window :
sig
  open DOM

  val w : window t

  external openDialog : #window t -> string -> string -> string -> unit = "#openDialog"
  external location : #window t -> string = ".location"
end
