(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007-9 Skydeck, Inc
 * Original file (driver/main.ml in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

open Config
open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Misc.chop_extension_if_any oname

let process_interface_file ppf name =
  Jscompile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Jscompile.implementation ppf name opref;
  objfiles := (opref ^ ".cmjs") :: !objfiles

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    Jscompile.implementation ppf name opref;
    objfiles := (opref ^ ".cmjs") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Jscompile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmjs"
       || Filename.check_suffix name ".cmjsa" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".js" then
    ccobjs := name :: !ccobjs
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The Objective Caml compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let usage = "Usage: ocamljs <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let anonymous = process_file Format.err_formatter;;
let impl = process_implementation_file Format.err_formatter;;
let intf = process_interface_file Format.err_formatter;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

module Options = Jsmain_args.Make_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
IFNDEF OCAML_3_10_2 THEN
  let _annot = set annotations
ENDIF
  let _c = set compile_only
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = ccopts := s :: !ccopts
  let _config = show_config
  let _custom = set custom_runtime
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
IFDEF OCAML_3_10_2 THEN
  let _dtypes = set save_types
ENDIF
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pack = set make_package
  let _patch = set Jslink.patch
  let _pp s = preprocessor := Some s
  let _principal = set principal
  let _rectypes = set recursive_types
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v = print_version_and_library
  let _version = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dparsetree = set dump_parsetree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous
end)

let extract_output = function
  | Some s -> s
  | None ->
      prerr_endline
        "Please specify the name of the output file, using option -o";
      exit 2

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

let main () =
  try
    Arg.parse Options.list anonymous usage;
    if !make_archive then begin
      Jscompile.init_path();
      Jslibrarian.create_archive (List.rev !objfiles)
                                   (extract_output !output_name)
    end
    else if !make_package then begin
      Jscompile.init_path();
      Jspackager.package_files (List.rev !objfiles)
                                 (extract_output !output_name)
    end
    else if not !compile_only && !objfiles <> [] then begin
      Jscompile.init_path();
      Jslink.link (List.rev !objfiles) (default_output !output_name)
    end;
    exit 0
  with x ->
    Jserrors.report_error Format.err_formatter x;
    exit 2

let _ = main ()
