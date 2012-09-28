(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

open OcpLang
open BuildTypes
open BuildOCPTree
open BuildOCPTypes
open BuildOCPVariable

(* Under Windows, we cannot use dot-prefixed directories *)
let homedir = try Sys.getenv "HOME" with Not_found -> "."

let time_arg = ref false
(*
let byte_arg = ref false
let asm_arg = ref false
*)
let clean_arg = ref false

let distclean_arg = ref false
let fake_arg = ref false
let save_config_arg = ref false

let stop_on_error_arg = ref true
let cross_arg = ref (Some "X" : string option)
let verbosity_arg = ref (None : int option)
let targets_arg = ref ([]: string list)
let distrib_arg = ref false
let conf_arg = ref false
let global_arg = ref false
let no_global_arg = ref false
let autogen_arg = ref false
let list_ocp_files = ref false


let packages_by_name =
  ref (StringMap.empty : BuildTypes.package_info StringMap.t)
let (all_projects : (int, BuildTypes.package_info) Hashtbl.t) = Hashtbl.create 111

let new_id_generator () =
  let counter = ref 0 in
  fun () ->
  let id = !counter in
  incr counter;
  id

let new_rule_id = new_id_generator ()
let new_file_id = new_id_generator ()
let new_dir_id = new_id_generator ()
let new_package_id = new_id_generator ()


(* For all projects ...
let (build_rules : (int, build_rule) Hashtbl.t) = Hashtbl.create 1111
let (build_files : (int, build_file) Hashtbl.t) = Hashtbl.create 1111
let (build_directories : (int * int,   build_directory) Hashtbl.t) = Hashtbl.create 1111
*)
(* let build_byte_targets = ref ([] : build_file list)
let build_asm_targets = ref ([] : build_file list) *)
(*
let get_project name = StringMap.find name !projects
*)


let new_library b pj package_dirname src_dir dst_dir mut_dir =

  let lib =
    {
      lib_context = b;
      lib_id = pj.package_id;
      lib_name = pj.package_name;
      lib_dirname = File.of_string package_dirname;
      lib_provides = pj.package_provides ;
      lib_type = pj.package_type ;
      lib_tag = pj.package_tag;
      lib_filename = pj.package_filename;
      lib_node = pj.package_node;
      lib_missing_deps = pj.package_missing_deps;
      lib_deps = pj.package_deps_map;
      lib_requires = List.map (fun pd ->
        let pk = pd.dep_project in
        { pd with dep_project = Hashtbl.find all_projects pk.package_id }
      ) pj.package_requires;
      lib_added = pj.package_added;
      lib_options = pj.package_options;
      lib_installed = bool_option_true pj.package_options generated_option;

    (* lib_package = pj; *)
      lib_loc = (pj.package_filename, pj.package_loc, pj.package_name);
      lib_src_dir = src_dir;
      lib_dst_dir = dst_dir;
      lib_mut_dir = mut_dir;
      lib_byte_targets = [];
      lib_cmo_objects = [];
      lib_bytecomp_deps = [];
      lib_bytelink_deps = [];
      lib_asm_targets = [];
      lib_asm_cmx_objects = [];
      lib_asm_cmxo_objects = [];
      lib_asmcomp_deps = [];
      lib_asmlink_deps = [];
      lib_clink_deps = [];
      lib_modules = ref StringMap.empty;
      lib_internal_modules = StringsMap.empty;
      lib_dep_deps = IntMap.empty;
      lib_includes = None;
      lib_sources = List.map (fun (file, options) ->
        (file, BuildOCPInterp.translate_options pj.package_options options)
      ) pj.package_sources;
    }
  in
  Hashtbl.add all_projects lib.lib_id lib;
  packages_by_name := StringMap.add lib.lib_name lib !packages_by_name;
  lib


let absolute_filename dirname =
  if Filename.is_relative dirname then
    Filename.concat (Unix.getcwd ()) dirname
  else dirname

let installed_files = ref []
let register_installed (file : string) =
  installed_files := file :: !installed_files

(* TODO
let register_project pk =
  Hashtbl.add all_projects pk.lib_id pk
*)


