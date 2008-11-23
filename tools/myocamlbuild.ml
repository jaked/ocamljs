open Ocamlbuild_plugin
open Command
open Ocamlbuild_pack.Ocaml_compiler
open Ocamlbuild_pack.Ocaml_utils
open Ocamlbuild_pack.Tools

;;

let ocamljs = ref (A"ocamljs")

let ocamljs_c tags arg out =
  let tags = tags++"ocaml"++"js" in
  Cmd (S [!ocamljs; A"-c"; T(tags++"compile");
          ocaml_ppflags tags; flags_of_pathname arg;
          ocaml_include_flags arg; A"-o"; Px out; P arg])

let ocamljs_link flag tags deps out =
  Cmd (S [!ocamljs; flag; T tags;
          atomize_paths deps; flags_of_pathname out; A"-o"; Px out])

let ocamljs_link_lib = ocamljs_link (A"-a")
let ocamljs_link_prog = ocamljs_link N

let js_lib_linker tags =
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags (* XXX ocamlmkjslib? *)
  else
    ocamljs_link_lib tags

let js_lib_linker_tags tags = tags++"ocaml"++"link"++"js"++"library"

let ocamljs_p tags deps out =
  Cmd (S [!ocamljs; A"-pack"; T tags;
          atomize_paths deps; flags_of_pathname out; A"-o"; Px out])

let js_compile_ocaml_implem ?tag ml cmjs env build =
  let ml = env ml and cmjs = env cmjs in
  prepare_compile build ml;
  ocamljs_c (tags_of_pathname ml++"implem"+++tag) ml cmjs

let js_link_gen = link_gen "cmjs" "cmjsa" "cmjsa" ["cmjs"; "cmi"]

let js_link = js_link_gen ocamljs_link_prog
  (fun tags -> tags++"ocaml"++"link"++"js"++"program")

let js_library_link = js_link_gen js_lib_linker js_lib_linker_tags

let js_library_link_modules =
  link_modules [("cmjs",[]); ("cmi",[])] "cmjs" "cmjsa" "cmjsa" js_lib_linker js_lib_linker_tags

(* from Ocaml_compiler *)
let link_from_file link modules_file cmX env build =
  let modules_file = env modules_file in
  let contents_list = string_list_of_file modules_file in
  link contents_list cmX env build

let js_library_link_mllib = link_from_file js_library_link_modules

let js_pack_modules =
  pack_modules [("cmjs",["cmi"]); ("cmi",[])] "cmjs" "cmjsa" "cmjsa" ocamljs_p
    (fun tags -> tags++"ocaml"++"pack"++"js")

let js_pack_mlpack = link_from_file js_pack_modules

;;

(* copied from the ocamlc versions in Ocaml_commpiler. *)
rule "ocaml: mlpack & cmjs* & cmi -> cmjs"
  ~tags:["ocaml"; "js"]
  ~prod:"%.cmjs"
  ~deps:["%.mli"; "%.cmi"; "%.mlpack"]
  (js_pack_mlpack "%.mlpack" "%.cmjs");;

(*
  this rule is a little hack; the right one is commented out below,
  but since apparently tags are not checked when picking a rule (??),
  the commented rule conflicts with the ocamlc one when trying to
  build a .cmi
*)

rule "ocaml: mlpack & cmjs* -> cmjs"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmjs"]
  ~dep:"%.mlpack"
  (js_pack_mlpack "%.mlpack" "%.cmjs");;

(*
rule "ocaml: mlpack & cmjs* -> cmjs & cmi"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmjs"; "%.cmi"]
  ~dep:"%.mlpack"
  (js_pack_mlpack "%.mlpack" "%.cmjs");;
*)

rule "ocaml: ml & cmi -> cmjs"
  ~tags:["ocaml"; "js"]
  ~prod:"%.cmjs"
  ~deps:["%.mli"(* This one is inserted to force this rule to be skiped when
                   a .ml is provided without a .mli *); "%.ml"; "%.ml.depends"; "%.cmi"]
  (js_compile_ocaml_implem "%.ml" "%.cmjs");;

(*
  see above
*)
rule "ocaml: ml -> cmjs"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmjs"]
  ~deps:["%.ml"; "%.ml.depends"]
  (js_compile_ocaml_implem "%.ml" "%.cmjs");

(*
  rule "ocaml: ml -> cmjs & cmi"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmjs"; "%.cmi"]
  ~deps:["%.ml"; "%.ml.depends"]
  (js_compile_ocaml_implem "%.ml" "%.cmjs");
*)

rule "ocaml: cmjs* -> js"
  ~tags:["ocaml"; "js"; "program"]
  ~prod:"%.js"
  ~dep:"%.cmjs"
  (js_link "%.cmjs" "%.js");

rule "ocaml: mllib & cmjs* -> cmja"
  ~tags:["ocaml"; "js"; "library"]
  ~prod:"%.cmjsa"
  ~dep:"%.mllib"
  (js_library_link_mllib "%.mllib" "%.cmjsa");;

rule "ocaml: cmo* -> cmjsa"
  ~tags:["ocaml"; "js"; "library"]
  ~prod:"%.cmjsa"
  ~dep:"%.cmjs"
  (js_library_link "%.cmjs" "%.cmjsa");;

flag ["ocaml"; "js"; "link"] begin
  S (List.map (fun x -> A (x^".cmjsa")) !Options.ocaml_libs)
end;;

(* ocamlfind integration following http://www.nabble.com/forum/ViewPost.jtp?post=15979274 *)

(* these functions are not really officially exported *)
let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

(* this lists all supported packages *)
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
      run_and_read "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[Sh"OCAMLFIND_COMMANDS=ocamljs=../../../src/jscomp/_build/jsmain.byte"; A"../../../bin/ocamlfindjs"; x]

;;

dispatch begin function
  | Before_options ->

      (* override default commands by ocamlfind ones *)
       Options.ocamlc   := ocamlfind & A"ocamlc";
       Options.ocamlopt := ocamlfind & A"ocamlopt";
       Options.ocamldep := ocamlfind & A"ocamldep";
       Options.ocamldoc := ocamlfind & A"ocamldoc";
       ocamljs := ocamlfind & A"ocamljs";

  | After_rules ->

      flag ["ocaml"; "compile"; "dtypes"] & A"-dtypes";

      (* When one link an OCaml library/binary/package, one should use -linkpkg *)
       flag ["ocaml"; "byte"; "link"] & A"-linkpkg";
       flag ["ocaml"; "native"; "link"] & A"-linkpkg";
       flag ["ocaml"; "js"; "link"] & A"-linkpkg";

       (* For each ocamlfind package one inject the -package option when
        * compiling, computing dependencies, generating documentation and
        * linking. *)
       List.iter begin fun pkg ->
         flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
         flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
       end (find_packages ());

       (* Like -package but for extensions syntax. Morover -syntax is useless
        * when linking. *)
       List.iter begin fun syntax ->
         flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
         flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
       end (find_syntaxes ());

  | _ -> ()
end

;;
