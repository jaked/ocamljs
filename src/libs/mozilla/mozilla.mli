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

  val getService_appshell_window_mediator : unit -> <windowMediator:i> t
  val getService_consoleservice : unit -> <consoleService:i> t
  val getService_cookiemanager : unit -> <cookieManager:i> t
  val getService_file_directory_service : unit -> <properties:i> t
  val getService_observer_service : unit -> <observerService:i> t
  val getService_passwordmanager_passwordManager : unit -> <passwordManager:i> t
  val getService_passwordmanager_passwordManagerInternal : unit -> <passwordManagerInternal:i> t
  val getService_preferences_service : unit -> <prefBranch:i> t

  val createInstance_file_local : unit -> <file:i; localFile:i> t
  val createInstance_network_buffered_input_stream : unit -> <inputStream:i; bufferedInputStream:i> t
  val createInstance_network_file_input_stream : unit -> <inputStream:i; fileInputStream:i> t
  val createInstance_network_file_output_stream : unit -> <outputStream:i; fileOutputStream:i> t
  val createInstance_network_mime_input_stream : unit -> <inputStream:i; mIMEInputStream:i> t
  val createInstance_io_multiplex_input_stream : unit -> <inputStream:i; multiplexInputStream:i> t
  val createInstance_io_string_input_stream : unit -> <inputStream:i; stringInputStream:i> t
  val createInstance_scriptableinputstream : unit -> <inputStream:i; scriptableInputStream:i> t

  type 'a out

  val out : 'a -> 'a out
  external outv : 'a out -> 'a = ".value"
end

module BufferedInputStream :
sig
  open XPCOM

  external init : <bufferedInputStream:i; ..> t -> <inputStream:i; ..> t -> int -> unit = "#init"
end

module Channel :
sig
  open XPCOM

  external uRI : <channel:i; ..> t -> <uRI:i> t = ".URI"
end

module ConsoleService :
sig
  open XPCOM

  external logStringMessage : <consoleService:i; ..> t -> string -> unit = "#logStringMessage"
end

module CookieManager :
sig
  open XPCOM

  external remove : <cookieManager:i; ..> t -> string -> string -> string -> bool -> unit = "#remove"
  external enumerator : <cookieManager:i; ..> t -> <simpleEnumerator:<supports:i> t> t = ".enumerator"
end

module Cookie :
sig
  open XPCOM

  external name : <cookie:i; ..> t -> string = ".name"
  external value : <cookie:i; ..> t -> string = ".value"
  external host : <cookie:i; ..> t -> string = ".host"
  external path : <cookie:i; ..> t -> string = ".path"
end

module DOMJSWindow :
sig
  open XPCOM

  val w : <dOMJSWindow:i> t

  type timeout
  type interval

  val setTimeout : <dOMJSWindow:i; ..> t -> (unit -> unit) -> int -> timeout
  external clearTimeout : <dOMJSWindow:i; ..> t -> timeout -> unit = "#clearTimeout"
  val setInterval : <dOMJSWindow:i; ..> t -> (unit -> unit) -> int -> interval
  external clearInterval : <dOMJSWindow:i; ..> t -> interval -> unit = "#clearInterval"
end

module FileInputStream :
sig
  open XPCOM

  external init : <fileInputStream:i; ..> t -> <file:i; ..> t -> int -> int -> int -> unit = "#init"
end

module File :
sig
  open XPCOM

  external remove : <file:i; ..> t -> bool -> unit = "#remove"
  external append : <file:i; ..> t -> string -> unit = "#append"
end

module FileOutputStream :
sig
  open XPCOM

  external init : <fileOutputStream:i; ..> t -> <file:i; ..> t -> int -> int -> int -> unit = "#init"
end

module HttpChannel :
sig
  open XPCOM

  external setRequestHeader : <httpChannel:i; ..> t -> string -> string -> bool -> unit = "#setRequestHeader"
end

module InputStream :
sig
  open XPCOM

  val coerce : <inputStream:i; ..> t -> <inputStream:i> t
end

module LocalFile :
sig
  open XPCOM

  external setRelativeDescriptor : <localFile:i; ..> t -> <localFile:i; ..> t -> string -> unit = "#setRelativeDescriptor"
end

module MIMEInputStream :
sig
  open XPCOM

  external set_addContentLength : <mIMEInputStream:i; ..> t -> bool -> unit = "=addContentLength"
  external addHeader : <mIMEInputStream:i; ..> t -> string -> string -> unit = "#addHeader"
  external setData : <mIMEInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#setData"
end

module MultiplexInputStream :
sig
  open XPCOM

  external appendStream : <multiplexInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#appendStream"
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

  val addObserver : <observerService:i; ..> t -> (<supports:i> t -> string -> string -> unit) -> string -> < > o
  val addObserver_http_on_modify_request : <observerService:i; ..> t -> (<httpChannel:i> t -> string -> unit) -> <httpOnModifyRequest:tp> o

  external removeObserver : <observerService:i; ..> t -> < ..> o -> string -> unit = "#removeObserver"
  val removeObserver_http_on_modify_request : <observerService:i; ..> t -> <httpOnModifyRequest:tp; ..> o -> unit
end

module OutputStream :
sig
  open XPCOM

  external write : <outputStream:i; ..> t -> string -> int -> unit = "#write"
  external close : <outputStream:i; ..> t -> unit = "#close"
end

module PasswordManagerInternal :
sig
  open XPCOM

  external findPasswordEntry : <passwordManagerInternal:i; ..> t -> string -> string -> string -> string out -> string out -> string out -> unit = "#findPasswordEntry"
  external addUserFull : <passwordManagerInternal:i; ..> t -> string -> string -> string -> string -> string -> unit = "#addUserFull"

  val findEntry : <passwordManagerInternal:i; ..> t -> string -> string -> string -> (string * string * string) option
  val addEntry : <passwordManagerInternal:i; ..> t -> string -> string -> string -> unit
end

module PasswordManager :
sig
  open XPCOM

  external addUser : <passwordManager:i; ..> t -> string -> string -> string -> unit = "#addUser"
  external removeUser : <passwordManager:i; ..> t -> string -> string -> unit = "#removeUser"
  external addReject : <passwordManager:i; ..> t -> string -> unit = "#addReject"
  external removeReject : <passwordManager:i; ..> t -> string -> unit = "#removeReject"
end

module PrefBranch :
sig
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
  val getPref : <prefBranch:i; ..> t -> string -> pref
end

module Properties :
sig
  open XPCOM

  external get : <properties:i; ..> t -> string -> interface -> 'a = "#get"
  val getFile : <properties:i; ..> t -> string -> <file:i; ..> t
end

module ScriptableInputStream :
sig
  open XPCOM

  external init : <scriptableInputStream:i; ..> t -> <inputStream:i; ..> t -> unit = "#init"
end

module SimpleEnumerator :
sig
  open XPCOM

  external hasMoreElements : <simpleEnumerator:'a; ..> t -> bool = "#hasMoreElements"
  external getNext : <simpleEnumerator:'a; ..> t -> 'a = "#getNext"
end

module StringInputStream :
sig
  open XPCOM

  external setData : <stringInputStream:i; ..> t -> string -> int -> unit = "#setData"
end

module Supports :
sig
  open XPCOM

  external queryInterface : <supports:i; ..> t -> interface -> 'a = "#QueryInterface"

  val queryInterface_httpChannel : <supports:i; ..> t -> <httpChannel:i> t
  val queryInterface_cookie : <supports:i; ..> t -> <cookie:i> t
end

module URI :
sig
  open XPCOM

  external spec : <uRI:i; ..> t -> string = ".spec"
end

module WindowMediator :
sig
  open XPCOM

  val getEnumerator : <windowMediator:i; ..> t -> string option -> <simpleEnumerator:<supports:i> t> t
end

module XMLHttpRequest :
sig
  open XPCOM

  external new_ : unit -> <xMLHttpRequest:i> t = "$new" "XMLHttpRequest"
  val set_onreadystatechange : <xMLHttpRequest:i; ..> t -> (unit -> unit) -> unit
  val set_onload : <xMLHttpRequest:i; ..> t -> (unit -> unit) -> unit
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

module DOM :
sig
  type i
  type (+'a) t
end

module Document :
sig
  open DOM

  val d : <document:i> t

  external getElementById : <document:i; ..> t -> string -> <element:i> t = "#getElementById"

  external getElementById_dialog : <document:i; ..> t -> string -> <element:i; dialog:i> t = "#getElementById"
  external getElementById_menuItem : <document:i; ..> t -> string -> <element:i; menuItem:i> t = "#getElementById"
  external getElementById_menuList : <document:i; ..> t -> string -> <element:i; menuList:i> t = "#getElementById"
  external getElementById_statusBarPanel : <document:i; ..> t -> string -> <element:i; statusBarPanel:i> t = "#getElementById"
  external getElementById_textBox : <document:i; ..> t -> string -> <element:i; textBox:i> t = "#getElementById"
end

module Element :
sig
  open DOM

  type e
  type 'a l

  val addEventListener : <element:i; ..> t -> string -> (< ..> -> bool) -> bool -> < > l

  val addEventListener_command : <element:i; ..> t -> (<event:i> t -> bool) -> bool -> <command:e> l
  val addEventListener_click : <element:i; ..> t -> (<event:i; mouseEvent:i> t -> bool) -> bool -> <click:e> l
  val addEventListener_dialogaccept : <element:i; ..> t -> (<event:i> t -> bool) -> bool -> <dialogaccept:e> l
  val addEventListener_load : <element:i; ..> t -> (<event:i> t -> bool) -> bool -> <load:e> l
  val addEventListener_unload : <element:i; ..> t -> (<event:i> t -> bool) -> bool -> <unload:e> l

  external removeEventListener : <element:i; ..> t -> string -> < ..> l -> bool -> unit = "#removeEventListener"

  val removeEventListener_command : <element:i; ..> t -> <command:e; ..> l -> bool -> unit
  val removeEventListener_click : <element:i; ..> t -> <click:e; ..> l -> bool -> unit
  val removeEventListener_dialogaccept : <element:i; ..> t -> <dialogaccept:e; ..> l -> bool -> unit
  val removeEventListener_load : <element:i; ..> t -> <load:e; ..> l -> bool -> unit
  val removeEventListener_unload : <element:i; ..> t -> <unload:e; ..> l -> bool -> unit

  external setAttribute : <element:i; ..> t -> string -> 'a -> unit = "#setAttribute"
end

module MenuList :
sig
  open DOM

  external selectedIndex : <menuList:i; ..> t -> int = ".selectedIndex"
  external set_selectedIndex : <menuList:i; ..> t -> int -> unit = "=selectedIndex"
  external value : <menuList:i; ..> t -> string = ".value"
  external set_value : <menuList:i; ..> t -> string -> unit = "=value"
end

module MouseEvent :
sig
  open DOM

  external button : <mouseEvent:i> t -> int = ".button"
end

module TextBox :
sig
  open DOM

  external value : <textBox:i; ..> t -> string = ".value"
  external set_value : <textBox:i; ..> t -> string -> unit = "=value"
end

module Window :
sig
  open DOM

  val w : <element:i; window:i> t

  external openDialog : <window:i; ..> t -> string -> string -> string -> unit = "#openDialog"
  external location : <window:i; ..> t -> string = ".location"
end
