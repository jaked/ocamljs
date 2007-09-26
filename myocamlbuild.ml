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

let js_compile_ocaml_implem ?tag ml cmj env build =
  let ml = env ml and cmj = env cmj in
  prepare_compile build ml;
  ocamljs_c (tags_of_pathname ml++"implem"+++tag) ml cmj

let js_link_gen = link_gen "cmj" "cmja" "cmja" ["cmj"; "cmi"]

let js_link = js_link_gen ocamljs_link_prog
  (fun tags -> tags++"ocaml"++"link"++"js"++"program")

let js_library_link = js_link_gen js_lib_linker js_lib_linker_tags

let js_library_link_modules =
  link_modules [("cmj",[]); ("cmi",[])] "cmj" "cmja" "cmja" js_lib_linker js_lib_linker_tags

(* from Ocaml_compiler *)
let link_from_file link modules_file cmX env build =
  let modules_file = env modules_file in
  let contents_list = string_list_of_file modules_file in
  link contents_list cmX env build

let js_library_link_mllib = link_from_file js_library_link_modules

let js_pack_modules =
  pack_modules [("cmj",["cmi"]); ("cmi",[])] "cmj" "cmja" "cmja" ocamljs_p
    (fun tags -> tags++"ocaml"++"pack"++"js")

let js_pack_mlpack = link_from_file js_pack_modules

;;

(* copied from the ocamlc versions in Ocaml_commpiler. *)
rule "ocaml: mlpack & cmj* & cmi -> cmj"
  ~tags:["ocaml"; "js"]
  ~prod:"%.cmj"
  ~deps:["%.mli"; "%.cmi"; "%.mlpack"]
  (js_pack_mlpack "%.mlpack" "%.cmj");;

(*
  this rule is a little hack; the right one is commented out below,
  but since apparently tags are not checked when picking a rule (??),
  the commented rule conflicts with the ocamlc one when trying to
  build a .cmi
*)

rule "ocaml: mlpack & cmj* -> cmj"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmj"]
  ~dep:"%.mlpack"
  (js_pack_mlpack "%.mlpack" "%.cmj");;

(*
rule "ocaml: mlpack & cmj* -> cmj & cmi"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmj"; "%.cmi"]
  ~dep:"%.mlpack"
  (js_pack_mlpack "%.mlpack" "%.cmj");;
*)

rule "ocaml: ml & cmi -> cmj"
  ~tags:["ocaml"; "js"]
  ~prod:"%.cmj"
  ~deps:["%.mli"(* This one is inserted to force this rule to be skiped when
                   a .ml is provided without a .mli *); "%.ml"; "%.ml.depends"; "%.cmi"]
  (js_compile_ocaml_implem "%.ml" "%.cmj");;

(*
  see above
*)
rule "ocaml: ml -> cmj"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmj"]
  ~deps:["%.ml"; "%.ml.depends"]
  (js_compile_ocaml_implem "%.ml" "%.cmj");

(*
  rule "ocaml: ml -> cmj & cmi"
  ~tags:["ocaml"; "js"]
  ~prods:["%.cmj"; "%.cmi"]
  ~deps:["%.ml"; "%.ml.depends"]
  (js_compile_ocaml_implem "%.ml" "%.cmj");
*)

rule "ocaml: cmj* -> js"
  ~tags:["ocaml"; "js"; "program"]
  ~prod:"%.js"
  ~dep:"%.cmj"
  (js_link "%.cmj" "%.js");

rule "ocaml: mllib & cmj* -> cmja"
  ~tags:["ocaml"; "js"; "library"]
  ~prod:"%.cmja"
  ~dep:"%.mllib"
  (js_library_link_mllib "%.mllib" "%.cmja");;

rule "ocaml: cmo* -> cmja"
  ~tags:["ocaml"; "js"; "library"]
  ~prod:"%.cmja"
  ~dep:"%.cmj"
  (js_library_link "%.cmj" "%.cmja");;

flag ["ocaml"; "js"; "link"] begin
  S (List.map (fun x -> A (x^".cmja")) !Options.ocaml_libs)
end;;

Pathname.define_context "src"            ["ocaml/bytecomp"; "ocaml/driver"; "ocaml/parsing"; "ocaml/typing"; "ocaml/utils"];
Pathname.define_context "ocaml/bytecomp" ["ocaml/parsing"; "ocaml/typing"; "ocaml/utils"];
Pathname.define_context "ocaml/driver"   ["ocaml/parsing"; "ocaml/utils"];
Pathname.define_context "ocaml/parsing"  ["ocaml/utils"];
Pathname.define_context "ocaml/typing"   ["ocaml/parsing"; "ocaml/utils"];
Pathname.define_context "ocaml/utils"    ["ocaml"];

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
