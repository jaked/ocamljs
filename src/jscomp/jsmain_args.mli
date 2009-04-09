(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Original file (driver/main_args.mli) in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

module Make_options (F :
    sig
      val _a : unit -> unit
IFNDEF OCAML_3_10_2 THEN
      val _annot : unit -> unit
ENDIF
      val _c : unit -> unit
      val _cclib : string -> unit
      val _ccopt : string -> unit
      val _config : unit -> unit
      val _custom : unit -> unit
      val _dllib : string -> unit
      val _dllpath : string -> unit
IFDEF OCAML_3_10_2 THEN
      val _dtypes : unit -> unit
ENDIF
      val _g : unit -> unit
      val _i : unit -> unit
      val _I : string -> unit
      val _impl : string -> unit
      val _intf : string -> unit
      val _intf_suffix : string -> unit
      val _labels : unit -> unit
      val _linkall : unit -> unit
      val _make_runtime : unit -> unit
      val _noassert : unit -> unit
      val _noautolink : unit -> unit
      val _nolabels : unit -> unit
      val _nostdlib : unit -> unit
      val _o : string -> unit
      val _output_obj : unit -> unit
      val _pack : unit -> unit
      val _patch : unit -> unit
      val _pp : string -> unit
      val _principal : unit -> unit
      val _rectypes : unit -> unit
      val _thread : unit -> unit
      val _vmthread : unit -> unit
      val _unsafe : unit -> unit
      val _use_prims : string -> unit
      val _use_runtime : string -> unit
      val _v : unit -> unit
      val _version : unit -> unit
      val _verbose : unit -> unit
      val _w : string -> unit
      val _warn_error : string -> unit
      val _where : unit -> unit

      val _nopervasives : unit -> unit
      val _dparsetree : unit -> unit
      val _drawlambda : unit -> unit
      val _dlambda : unit -> unit
      val _dinstr : unit -> unit
      val anonymous : string -> unit
    end) :
  sig
    val list : (string * Arg.spec * string) list
  end
