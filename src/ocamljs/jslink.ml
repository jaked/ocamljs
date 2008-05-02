(*
 * This file is part of ocamljs, OCaml to Javascript compiler
 * Copyright (C) 2007 Skydeck, Inc
 * Original file (bytecomp/bytelink.ml in the Objective Caml source
 * distribution) is Copyright (C) INRIA.
 *
 * This program is free software released under the QPL.
 * See LICENSE for more details.
 *
 * The Software is provided AS IS with NO WARRANTY OF ANY KIND,
 * INCLUDING THE WARRANTY OF DESIGN, MERCHANTABILITY AND 
 * FITNESS FOR A PARTICULAR PURPOSE.
 *)

(* Link a set of .cmjs files and produce a Javascript executable. *)

open Sys
open Misc
open Config
open Ocamljs_config
open Instruct
open Cmo_format

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | File_exists of string

exception Error of error

type link_action =
    Link_object of string * compilation_unit
      (* Name of .cmo file and descriptor of the unit *)
  | Link_archive of string * compilation_unit list
      (* Name of .cma file and descriptors of the units to be linked. *)
  | Link_js of string
      (* Name of .js file *)

(* First pass: determine which units are needed *)

module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let missing_globals = ref IdentSet.empty

let is_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      IdentSet.mem id !missing_globals
  | _ -> false

let add_required (rel, pos) =
  match rel with
    Reloc_getglobal id ->
      missing_globals := IdentSet.add id !missing_globals
  | _ -> ()

let remove_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      missing_globals := IdentSet.remove id !missing_globals
  | _ -> ()

let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  if Filename.check_suffix file_name ".js"
  then Link_js(file_name) :: tolink
  else try
    let buffer = String.create (String.length cmjs_magic_number) in
    really_input ic buffer 0 (String.length cmjs_magic_number);
    if buffer = cmjs_magic_number then begin
      (* This is a .cmjs file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      List.iter add_required compunit.cu_reloc;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cmjsa_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : library) in
      close_in ic;
      let required =
        List.fold_right
          (fun compunit reqd ->
            if compunit.cu_force_link
            || !Clflags.link_everything
            || List.exists is_required compunit.cu_reloc
            then begin
              List.iter remove_required compunit.cu_reloc;
              List.iter add_required compunit.cu_reloc;
              compunit :: reqd
            end else
              reqd)
          toc.lib_units [] in
      Link_archive(file_name, required) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

(* Second pass: link in the required units *)

(* Consistency check between interfaces *)

let crc_interfaces = Consistbl.create ()

let check_consistency file_name cu =
  try
    List.iter
      (fun (name, crc) ->
        if name = cu.cu_name
        then Consistbl.set crc_interfaces name crc file_name
        else Consistbl.check crc_interfaces name crc file_name)
      cu.cu_imports
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_import(name, user, auth)))

let extract_crc_interfaces () =
  Consistbl.extract crc_interfaces

(* Record compilation events *)

let debug_info = ref ([] : (int * string) list)

(* Link in a compilation unit *)

let link_compunit output_fun inchan file_name compunit =
  check_consistency file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = (input_value inchan : Js.stmt) in

  (* we don't need to do any relocation, but as a side effect this
     checks that we have the right globals and primitives *)
  let dummy = "1234" in
  Symtable.patch_object dummy compunit.cu_reloc;

  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 132;
  Printjs.stmts ppf [ code_block ];
  Format.fprintf ppf "\n";
  Format.pp_print_flush ppf ();
  output_fun (Buffer.contents buf);
  if !Clflags.link_everything then
    List.iter Symtable.require_primitive compunit.cu_primitives

(* Link in a .cmjs file *)

let link_object output_fun file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit output_fun inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cmjsa file *)

let link_archive output_fun file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter
      (fun cu ->
         let name = file_name ^ "(" ^ cu.cu_name ^ ")" in
         try
           link_compunit output_fun inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x

(* Link in a .js file *)

let link_js output_fun file_name =
  let inchan = open_in file_name in
  let buf = String.create 8192 in
  let rec copy () =
    match input inchan buf 0 8192 with
      | 0 -> ()
      | n -> output_fun (String.sub buf 0 n); copy () in
  copy()

(* Link in a .cmjs or .cmjsa or .js file *)

let link_file output_fun = function
    Link_object(file_name, unit) ->
      link_object output_fun file_name unit
  | Link_archive(file_name, units) ->
      link_archive output_fun file_name units
  | Link_js(file_name) ->
      link_js output_fun file_name

(* Transform a file name into an absolute file name *)

let make_absolute file =
  if Filename.is_relative file
  then Filename.concat (Sys.getcwd()) file
  else file

(* Init the symtable, taking prims from the .js files *)

let init_symtable_with_prims tolink =
  let tfn = Filename.temp_file "primitives" "" in
  let poc = open_out tfn in
  List.iter (function
    | (Link_archive _ | Link_object _) -> ()
    | Link_js(fn) ->
	let get_ident s =
	  let l = String.length s in
	  let is_ident_char = function
	    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '$') -> true
	    | _ -> false in
	  let rec gi n =
	    if n < l && is_ident_char s.[n]
	    then gi (n + 1)
	    else n in
	  if l >= 4 && String.sub s 0 4 = "var "
	  then Some (String.sub s 4 (gi 4 - 4))
	  else if l >= 9 && String.sub s 0 9 = "function "
	  then Some (String.sub s 9 (gi 9 - 9))
	  else None in
	let ic = open_in fn in
	try
	  while true do
	    match get_ident (input_line ic) with
		None -> ()
	      | Some i ->
		  output_string poc i;
		  output_char poc '\n'
	  done
	with End_of_file -> close_in ic
	  | x -> close_in ic; raise x)
    tolink;

  close_out poc;
  Clflags.use_prims := tfn;
  Symtable.init();
  try Sys.remove tfn
  with Sys_error _ -> ()

(* Create a Javascript "executable" file *)

let link_js_exec tolink exec_name =
  Misc.remove_file exec_name; (* avoid permission problems, cf PR#1911 *)
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                 0o666 exec_name in
  try
    (* The path to the Javascript interpreter (in use_runtime mode) *)
    if String.length !Clflags.use_runtime > 0 then begin
      output_string outchan "#!";
      output_string outchan (make_absolute !Clflags.use_runtime);
      output_char outchan '\n';
    end;
    (* The javascript *)
    init_symtable_with_prims tolink;
    Consistbl.clear crc_interfaces;
    let output_fun = output_string outchan in
    output_fun ("// compiled by ocamlc " ^ version ^ ", ocamljs " ^ jsversion ^ "\n");

    output_fun ("(function (){\n");
    (* exceptions *)
    List.iter
      (fun (n, i) ->
	output_fun ("var " ^ Jsgen.jsident_of_ident i ^ " = \"" ^ n ^ "\";\n"))
      Predef.builtin_values;

    List.iter (link_file output_fun) tolink;
    output_fun ("})();\n");
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x

(* Main entry point *)

let link objfiles output_name =
  let objfiles =
    if !Clflags.nopervasives then objfiles
    else ["support.js"; "primitives.js"; "stdlib.cmjsa" ] @ objfiles @ ["std_exit.cmjs"] in
  let tolink = List.fold_right scan_file objfiles [] in
  link_js_exec tolink output_name

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %s is not a Javascript object file" name
  | Symbol_error(name, err) ->
      fprintf ppf "Error while linking %s:@ %a" name
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %s@ and %s@ \
                 make inconsistent assumptions over interface %s@]"
        file1 file2 intf
  | File_exists file ->
      fprintf ppf "Cannot overwrite existing file %s" file
