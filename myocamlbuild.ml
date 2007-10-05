open Ocamlbuild_plugin
open Command
open Ocamlbuild_pack.Ocaml_compiler
open Ocamlbuild_pack.Ocaml_utils
open Ocamlbuild_pack.Tools

;;

let ocamljs = A"../bin/ocamljs"

let ocamljs_c tags arg out =
  let tags = tags++"ocaml"++"js" in
  Cmd (S [ocamljs; A"-c"; T(tags++"compile");
          ocaml_ppflags tags; flags_of_pathname arg;
          ocaml_include_flags arg; A"-o"; Px out; P arg])

let ocamljs_link flag tags deps out =
  Cmd (S [ocamljs; flag; T tags;
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
  Cmd (S [ocamljs; A"-pack"; T tags;
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

Pathname.define_context "src/ocamljs"    ["ocaml/bytecomp"; "ocaml/driver"; "ocaml/parsing"; "ocaml/typing"; "ocaml/utils"];
Pathname.define_context "ocaml/bytecomp" ["ocaml/parsing"; "ocaml/typing"; "ocaml/utils"];
Pathname.define_context "ocaml/driver"   ["ocaml/parsing"; "ocaml/utils"];
Pathname.define_context "ocaml/parsing"  ["ocaml/utils"];
Pathname.define_context "ocaml/typing"   ["ocaml/parsing"; "ocaml/utils"];
Pathname.define_context "ocaml/utils"    ["ocaml"];

Pathname.define_context "src/libs/javascript" ["src/libs/ocamljs"];
Pathname.define_context "src/libs/mozilla"    ["src/libs/ocamljs"];

(* from ocaml/myocamlbuild.ml *)
copy_rule "Temporary rule, waiting for a full usage of ocamlbuild" "%.mlbuild" "%.ml";;

(* The numeric opcodes *)
rule "The numeric opcodes"
  ~prod:"ocaml/bytecomp/opcodes.ml"
  ~dep:"ocaml/byterun/instruct.h"
  ~insert:`top
	begin fun _ _ ->
	  Cmd(Sh "sed -n -e '/^enum/p' -e 's/,//g' -e '/^  /p' ocaml/byterun/instruct.h | \
        awk -f ../ocaml/tools/make-opcodes > ocaml/bytecomp/opcodes.ml")
  end;;

(* The version number *)
rule "ocaml/stdlib/sys.ml"
  ~prod:"ocaml/stdlib/sys.ml"
  ~deps:["ocaml/stdlib/sys.mlp"; "ocaml/VERSION"]
  begin fun _ _ ->
    let version = with_input_file "ocaml/VERSION" input_line in
    Seq [rm_f "ocaml/stdlib/sys.ml";
         Cmd (S[A"sed"; A"-e";
                A(Printf.sprintf "s,%%%%VERSION%%%%,%s," version);
                Sh"<"; P"ocaml/stdlib/sys.mlp"; Sh">"; Px"ocaml/stdlib/sys.ml"]);
         chmod (A"-w") "ocaml/stdlib/sys.ml"]
  end;;

rule "ocaml/bytecomp/runtimedef.ml"
  ~prod:"ocaml/bytecomp/runtimedef.ml"
  ~deps:["ocaml/byterun/primitives"; "ocaml/byterun/fail.h"]
  begin fun _ _ ->
    Cmd(S[A"../build/mkruntimedef.sh";Sh">"; Px"ocaml/bytecomp/runtimedef.ml"])
  end;;
