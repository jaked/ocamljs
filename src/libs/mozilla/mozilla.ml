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
  type 'a interface
  type result

  class type supports =
  object
    method _QueryInterface : 'a interface -> 'a
  end

  class type supportsWeakReference =
  object
  end

  class type iDRef =
  object
    method equals : 'a interface -> bool
  end

  class type ['a] out =
  object
    method _get_value : 'a
    method _set_value : 'a -> unit
  end

  class type inputStream =
  object
  end

  class type bufferedInputStream =
  object
    inherit inputStream
    method init : #inputStream -> int -> unit
  end

  class type uRI =
  object
    method _get_spec : string
    method _set_spec : string -> unit
    method resolve : string -> string
  end

  class type channel =
  object
    inherit supports
    method _get_URI : uRI
  end

  class type consoleService =
  object
    method logStringMessage : string -> unit
  end

  class type cookie =
  object
    method _get_name : string
    method _get_value : string
    method _get_host : string
    method _get_path : string
  end

  class type ['a] simpleEnumerator =
  object
    method hasMoreElements : bool
    method getNext : 'a
  end

  class type cookieManager =
  object
    method remove : string -> string -> string -> bool -> unit
    method _get_enumerator : supports simpleEnumerator
  end

  class type dOMNode =
  object
    inherit supports
    method _get_nodeName : string
    method _get_nodeValue : string
    method _get_nodeType : int
    method _get_parentNode : dOMNode
    method _get_childNodes : dOMNode array
    method _get_firstChild : dOMNode
    method _get_lastChild : dOMNode
    method _get_previousSibling : dOMNode
    method _get_nextSibling : dOMNode
  end

  class type dOMElement =
  object
    inherit dOMNode
    method getAttribute : string -> string
    method setAttribute : string -> string -> unit
  end

  class type dOMEvent =
  object
    inherit supports
    method _get_bubbles : bool
    method _get_cancelable : bool
    method _get_eventPhase : int
    (* method _get_target : dOMEventTarget *)
    method _get_timeStamp : float
    method _get_type : string

    method initEvent : string -> bool -> bool -> unit
    method preventDefault : unit
    method stopPropagation : unit
  end

  class type dOMEventListener =
  object
    inherit supports
    method handleEvent : #dOMEvent -> unit
  end

  class type dOMEventTarget =
  object
    inherit supports
    method addEventListener : string -> #dOMEventListener -> bool -> unit
    method removeEventListener : string -> #dOMEventListener -> bool -> unit
    method dispatchEvent : #dOMEvent -> bool
  end

  class type dOMAbstractView =
  object
    inherit supports
  end

  class type dOMDocumentView =
  object
    inherit supports
    method _get_defaultView : dOMAbstractView
  end

  class type dOMUIEvent =
  object
    inherit dOMEvent
    method initUIEvent : string -> bool -> bool -> #dOMAbstractView -> int -> unit
  end

  class type dOMMouseEvent =
  object
    inherit dOMUIEvent
    method _get_screenX : int
    method _get_screenY : int
    method _get_clientX : int
    method _get_clientY : int
    method _get_ctrlKey : bool
    method _get_shiftKey : bool
    method _get_altKey : bool
    method _get_metaKey : bool
    method _get_button : int
    method _get_relatedTarget : #dOMEventTarget
    method initMouseEvent :
      string -> bool -> bool -> #dOMAbstractView -> int ->
      int -> int -> int -> int ->
      bool -> bool -> bool -> bool ->
      int -> #dOMEventTarget ->
      unit
  end

  class type dOMDocument =
  object
    inherit dOMNode
    method getElementById : string -> #dOMElement
  end

  class type dOMXMLDocument =
  object
    inherit dOMDocument
  end

  class type dOMLocation =
  object
    inherit supports
    method _get_href : string
    method _set_href : string -> unit
  end

  class type dOMNSDocument =
  object
    inherit supports
    method _get_location : dOMLocation
  end

  class type dOMSerializer =
  object
    method serializeToString : #dOMNode -> string
  end

  class type dOMDocumentEvent =
  object
    inherit supports
    method createEvent : string -> #dOMEvent
  end

  class type dOMWindow =
  object
    inherit supports
    method _get_document : dOMDocument
  end

  class type dOMWindow2 =
  object
    inherit dOMWindow
  end

  class type dOMWindowInternal =
  object
    inherit dOMWindow2
    method alert : string -> unit
    method back : unit
    method close : unit
    method _get_location : string
    method _set_location : string -> unit
  end

  class type dOMJSWindow =
  object
    inherit supports
    method setTimeout : (unit -> unit) Ocamljs.jsfun -> float -> int
    method clearTimeout : int -> unit
    method setInterval : (unit -> unit) Ocamljs.jsfun -> float -> int
    method clearInterval : int -> unit
    method openDialog : string -> string -> string -> unit
  end

  class type dOMXPathNSResolver =
  object
    inherit supports
    method lookupNamespaceURI : string -> string
  end

  class type dOMXPathEvaluator =
  object
    inherit supports
    method evaluate : string -> #dOMNode -> #dOMXPathNSResolver -> int -> #supports -> #supports
  end

  class type dOMXPathResult =
  object
    method iterateNext : #dOMNode
    method _get_singleNodeValue : #dOMNode
    method _get_ANY_TYPE : int
    method _get_FIRST_ORDERED_NODE_TYPE : int
    method _get_ORDERED_NODE_ITERATOR_TYPE : int
  end

  class type file =
  object
    method remove : bool -> unit
    method append : string -> unit
  end

  class type fileInputStream =
  object
    inherit inputStream
    method init : #file -> int -> int -> int -> unit
  end

  class type outputStream =
  object
    method write : string -> int -> unit
    method close : unit
  end

  class type fileOutputStream =
  object
    inherit outputStream
    method init : #file -> int -> int -> int -> unit
  end

  class type httpChannel =
  object
    method setRequestHeader : string -> string -> bool -> unit
  end

  class type request =
  object
    inherit supports
    method _get_name : string
    method isPending : bool
    method _get_status : int
    method cancel : int -> unit
    method suspend : unit
    method resume : unit
  end

  class type requestObserver =
  object
    inherit supports
    method onStartRequest : #request -> #supports -> unit
    method onStopRequest : #request -> #supports -> int -> unit
  end

  class type streamListener =
  object
    inherit requestObserver
    method onDataAvailable : #request -> #supports -> #inputStream -> float -> float -> unit
  end

  class type inputStreamPump =
  object
    inherit request
    method init : #inputStream -> float -> float -> float -> float -> bool -> unit
    method asyncRead : #streamListener -> #supports -> unit
  end

  class type localFile =
  object
    inherit file
    method setRelativeDescriptor : localFile -> string -> unit
  end

  class type mIMEInputStream =
  object
    inherit inputStream
    method _set_addContentLength : bool -> unit
    method addHeader : string -> string -> unit
    method setData : #inputStream -> unit
  end

  class type multiplexInputStream =
  object
    inherit inputStream
    method appendStream : #inputStream -> unit
  end

  class type observer =
  object
    method observe : #supports -> string -> string -> unit
  end

  class type observerService =
  object
    method addObserver : observer -> string -> bool -> unit
    method removeObserver : observer -> string -> unit
  end

  class type passwordManager =
  object
    method addUser : string -> string -> string -> unit
    method removeUser : string -> string -> unit
    method addReject : string -> unit
    method removeReject : string -> unit
  end

  class type passwordManagerInternal =
  object
    method findPasswordEntry : string -> string -> string -> string out -> string out -> string out -> unit
    method addUserFull : string -> string -> string -> string -> string -> unit
  end

  class type prefBranch =
  object
    method _get_PREF_BOOL : int
    method _get_PREF_INT : int
    method _get_PREF_STRING : int
    method getPrefType : string -> int
    method getBoolPref : string -> bool
    method getCharPref : string -> string
    method getIntPref : string -> int
    method setBoolPref : string -> bool -> unit
    method setCharPref : string -> string -> unit
    method setIntPref : string -> int -> unit
  end

  class type prefBranch2 =
  object
    inherit prefBranch
    method addObserver : string -> observer -> bool -> unit
    method removeObserver : string -> observer -> unit
  end

  class type properties =
  object
    method get : string -> 'a interface -> 'a
  end

  class type scriptableInputStream =
  object
    inherit inputStream
    method init : #inputStream -> unit
    method close : unit
    method available : float
    method read : float -> string
  end

  class type transport =
  object
    method close : int -> unit
    method openInputStream : int -> int -> int -> inputStream
    method openOutputStream : int -> int -> int -> outputStream
  end

  class type serverSocket =
  object
    method asyncListen : serverSocketListener -> unit
    method close : unit
    method init : int -> bool -> int -> unit
  end

  and serverSocketListener =
  object
    method onSocketAccepted : serverSocket -> #transport -> unit
    method onStopListening : serverSocket -> int -> unit
  end

  class type stringInputStream =
  object
    inherit inputStream
    method setData : string -> int -> unit
  end

  class type uRIContentListener =
  object
    inherit supports
    method onStartURIOpen : #uRI -> bool
    method doContent : string -> bool -> #request -> streamListener out -> bool
    method isPreferred : string -> string out -> bool
    method canHandleContent : string -> bool -> string out
    (* loadCookie, parentContentListener *)
  end

  class type uRILoader =
  object
    inherit supports
    method registerContentListener : #uRIContentListener -> unit
    method unRegisterContentListener : #uRIContentListener -> unit
  end

  class type windowMediator =
  object
    method getEnumerator : string -> #dOMWindow simpleEnumerator
    method getMostRecentWindow : string -> #dOMWindow
  end

  class type xMLHttpRequest =
  object
    inherit supports
    method _set_onreadystatechange : (unit -> unit) Ocamljs.jsfun -> unit
    method _set_onload : (unit -> unit) Ocamljs.jsfun -> unit
    method _open : string -> string -> bool -> unit
    method setRequestHeader : string -> string -> unit
    method getResponseHeader : string -> string
    method overrideMimeType : string -> unit
    method send : #inputStream -> unit
    method _get_readyState : int
    method _get_responseText : string
    method _get_responseXML : #dOMXMLDocument
    method _get_channel : channel
    method abort : unit
    method _get_status : int
  end

  external createInstance : class_ -> 'a interface -> 'a = "#createInstance"
  external getService : class_ -> 'a interface -> 'a = "#getService"

  let cc c = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "classes") c
  let ci i = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "interfaces") i
  let cr r = Ocamljs.hashref (Ocamljs.fieldref (Ocamljs.var "Components") "results") r

  let bufferedInputStream = ci "nsIBufferedInputStream"
  let consoleService = ci "nsIConsoleService"
  let cookie = ci "nsICookie"
  let cookieManager = ci "nsICookieManager"
  let dOMSerializer = ci "nsIDOMSerializer"
  let file = ci "nsIFile"
  let fileInputStream = ci "nsIFileInputStream"
  let fileOutputStream = ci "nsIFileOutputStream"
  let httpChannel = ci "nsIHttpChannel"
  let inputStreamPump = ci "nsIInputStreamPump"
  let localFile = ci "nsILocalFile"
  let mIMEInputStream = ci "nsIMIMEInputStream"
  let multiplexInputStream = ci "nsIMultiplexInputStream"
  let observer = ci "nsIObserver"
  let observerService = ci "nsIObserverService"
  let passwordManager = ci "nsIPasswordManager"
  let passwordManagerInternal = ci "nsIPasswordManagerInternal"
  let prefBranch2 = ci "nsIPrefBranch2"
  let properties = ci "nsIProperties"
  let requestObserver = ci "nsIRequestObserver"
  let scriptableInputStream = ci "nsIScriptableInputStream"
  let serverSocket = ci "nsIServerSocket"
  let streamListener = ci "nsIStreamListener"
  let stringInputStream = ci "nsIStringInputStream"
  let supports = ci "nsISupports"
  let supportsWeakReference = ci "nsISupportsWeakReference"
  let uRI = ci "nsIURI"
  let uRIContentListener = ci "nsIURIContentListener"
  let uRILoader = ci "nsIURILoader"
  let windowMediator = ci "nsIWindowMediator"
  let xMLHttpRequest = ci "nsIXMLHttpRequest"

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
  let network_input_stream_pump = cc "@mozilla.org/network/input-stream-pump;1"
  let network_mime_input_stream = cc "@mozilla.org/network/mime-input-stream;1"
  let network_server_socket = cc "@mozilla.org/network/server-socket;1"
  let network_simple_uri = cc "@mozilla.org/network/simple-uri;1"
  let observer_service = cc "@mozilla.org/observer-service;1"
  let passwordmanager = cc "@mozilla.org/passwordmanager;1"
  let preferences_service = cc "@mozilla.org/preferences-service;1"
  let scriptableinputstream = cc "@mozilla.org/scriptableinputstream;1"
  let uriloader = cc "@mozilla.org/uriloader;1"
  let xmlextras_xmlhttprequest = cc "@mozilla.org/xmlextras/xmlhttprequest;1"
  let xmlextras_xmlserializer = cc "@mozilla.org/xmlextras/xmlserializer;1"

  let nOINTERFACE = cr "NS_NOINTERFACE"

  let getService_appshell_window_mediator () = getService appshell_window_mediator windowMediator
  let getService_consoleservice () = getService consoleservice consoleService
  let getService_cookiemanager () = getService cookiemanager cookieManager
  let getService_file_directory_service () = getService file_directory_service properties
  let getService_observer_service () = getService observer_service observerService
  let getService_passwordmanager_passwordManager () = getService passwordmanager passwordManager
  let getService_passwordmanager_passwordManagerInternal () = getService passwordmanager passwordManagerInternal
  let getService_preferences_service () = getService preferences_service prefBranch2
  let getService_uriloader () = getService uriloader uRILoader

  let createInstance_file_local () = createInstance file_local localFile
  let createInstance_network_buffered_input_stream () = createInstance network_buffered_input_stream bufferedInputStream
  let createInstance_network_file_input_stream () = createInstance network_file_input_stream fileInputStream
  let createInstance_network_file_output_stream () = createInstance network_file_output_stream fileOutputStream
  let createInstance_network_mime_input_stream () = createInstance network_mime_input_stream mIMEInputStream
  let createInstance_io_multiplex_input_stream () = createInstance io_multiplex_input_stream multiplexInputStream
  let createInstance_io_string_input_stream () = createInstance io_string_input_stream stringInputStream
  let createInstance_scriptableinputstream () = createInstance scriptableinputstream scriptableInputStream
  let createInstance_network_input_stream_pump () = createInstance network_input_stream_pump inputStreamPump
  let createInstance_network_server_socket () = createInstance network_server_socket serverSocket
  let createInstance_network_simple_uri () = createInstance network_simple_uri uRI
  let createInstance_xmlextras_xmlhttprequest () = createInstance xmlextras_xmlhttprequest xMLHttpRequest
  let createInstance_xmlextras_xmlserializer () = createInstance xmlextras_xmlserializer dOMSerializer

  let make_out v = Ocamljs.obj [ "value", v ]
end

module DOM =
struct
  class type style =
  object
    method _get_display : string
    method _set_display : string -> unit
    method _get_visibility : string
    method _set_visibility : string -> unit
  end

  class type event =
  object
    inherit XPCOM.dOMEvent
  end

  class type eventTarget =
  object
    inherit XPCOM.dOMEventTarget
    method addEventListener_fun_ : string -> (#event -> unit) Ocamljs.jsfun -> bool -> unit
    method removeEventListener_fun_ : string -> (#event -> unit) Ocamljs.jsfun -> bool -> unit
  end

  class type node =
  object
    inherit XPCOM.dOMNode
  end

  class type element =
  object
    inherit XPCOM.dOMElement
    inherit eventTarget
    method _get_hidden : bool
    method _set_hidden : bool -> unit
    method _get_style : style
    method _get_innerHTML : string
    method _set_innerHTML : string -> unit
  end

  class type document =
  object
    inherit XPCOM.dOMNSDocument
    inherit XPCOM.dOMDocumentEvent
    inherit XPCOM.dOMDocumentView
    inherit XPCOM.dOMEventTarget
    inherit XPCOM.dOMDocument
    inherit XPCOM.dOMXPathEvaluator
  end

  class type a =
  object
    inherit element
    method _get_href : string
  end

  class type area =
  object
    inherit element
  end

  class type uRI =
  object
    inherit XPCOM.uRI
  end

  class type browser =
  object
    inherit element
    method loadURI : string -> uRI -> string -> unit
    method goBack : unit
    method _get_contentDocument : document
  end

  class type button =
  object
    inherit element
    method _get_disabled : bool
    method _set_disabled : bool -> unit
    method _get_label : string
    method _set_label : string -> unit
  end

  class type deck =
  object
    inherit element
    method _get_selectedIndex : int
    method _set_selectedIndex : int -> unit
  end

  class type dialog =
  object
    inherit element
  end

  class type form =
  object
    inherit element
    method _get_method : string
    method _set_method : string -> unit
    method _get_action : string
    method _set_action : string -> unit
    method submit : unit
  end

  class type input =
  object
    inherit element
  end

  class type input_text =
  object
    inherit input
    method _get_value : string
    method _set_value : string -> unit
  end

  class type input_image =
  object
    inherit input
    method click : unit
  end

  class type label =
  object
    method _get_value : string
    method _set_value : string -> unit
  end

  class type map =
  object
    inherit element
    method _get_areas : area array
  end

  class type menuItem =
  object
    inherit element
  end

  class type menuList =
  object
    inherit element
    method _get_selectedIndex : int
    method _set_selectedIndex : int -> unit
    method _get_value : string
    method _set_value : string -> unit
  end

  class type mouseEvent =
  object
    inherit XPCOM.dOMMouseEvent
  end

  class type option =
  object
    inherit element
    method _get_text : string
    method _get_value : string
  end

  class type radio =
  object
    inherit element
    method _get_selected : int
    method _set_selected : int -> unit
  end

  class type select =
  object
    inherit element
    method _get_options : option array
    method _get_selectedIndex : int
    method _set_selectedIndex : int -> unit
  end

  class type statusBarPanel =
  object
    inherit element
  end

  class type stringBundle =
  object
    method getString : string -> string
  end

  class type tab =
  object
    inherit element
    method _get_linkedBrowser : browser
  end

  class type tabBrowser =
  object
    inherit element
    method addTab : string -> tab
    method removeTab : tab -> unit
    method _get_selectedTab : tab
    method _set_selectedTab : #tab -> unit
  end

  class type textBox =
  object
    inherit element
    method _get_value : string
    method _set_value : string -> unit
  end

  class type window =
  object
    inherit XPCOM.dOMWindow
    inherit XPCOM.dOMJSWindow
    inherit XPCOM.dOMWindowInternal
    inherit eventTarget
    inherit XPCOM.dOMAbstractView

    method getBrowser : tabBrowser
  end

  class type xMLDocument =
  object
    inherit document
    inherit XPCOM.dOMXMLDocument
  end

  class type xPathResult =
  object
    inherit XPCOM.dOMXPathResult
  end

  let document = Ocamljs.var "document"
  let window = Ocamljs.var "window"
end
