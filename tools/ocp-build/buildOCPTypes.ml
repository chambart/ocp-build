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
open BuildOCPTree


type dependency_flag =
| PREPROCESS
| COMPILE
| LINK

module DepFlagsSet = Set.Make(struct
    type t = dependency_flag
    let compare = compare
end)

type package = {
  package_name : string; (* basename of project *)
  mutable package_dirname : string; (* where the project files are *)

  mutable package_provides : string; (* what the project provides,
					default "" => same as name.
					if provides is specified, then
					the name of the object should
					be that one. TODO: it should
					be an option, since it should
					apply to modules too. *)
  mutable package_type : package_type; (* what it generates *)
  mutable package_tag : string; (* if tags are specified, then the
				   following rules apply when choosing
				   among several projects providing
				   the same name,

				   1/ if a tag is used in one of the
				   other projects, then the project
				   with that tag is preferred.

				   2/ If several tags are used in
				   other projects, and these tags are
				   present in different projects, then
				   a conflict is found and an error is
				   raised.

				   For example, if tags "debug" and
				   "threads" are used, then a conflict
				   is found if "stdlib" has two
				   versions, "debug" and "threads",
				   but the conflict is resolved if
				   "stdlib" also has a
				   "threads"+"debug" version.

				   Automatic tags: a "debug" version
				   is always compiled for bytecode and
				   native code, and a "profile"
				   version is always compiled for
				   native code. They are stored in
				   project+"+debug" and
				   project+"+profile", using the
				   interfaces from project. (how to do that ?)
				*)
  mutable package_version : string; (* unused: TODO *)
  mutable package_auto : string option; (* unused: TODO *)

  package_loc : int;
  package_filename : string;

  (* at the end of "load_project", we rename package_identifiers to be
     continuous *)
  mutable package_id : int;
  mutable package_validated : bool;
  package_node : LinearToposort.node;
  mutable package_missing_deps : int;

  mutable package_has_byte_debug : bool;   (* unused: TODO *)
  mutable package_has_asm_debug : bool;    (* unused: TODO *)
  mutable package_has_asm_profile : bool;  (* unused: TODO *)


  mutable package_sources : string_with_attributes list;
    (* the sources of the project, plus the flags to compile them. *)
  mutable package_files : (string * BuildOCPVariable.options) list;



  (* list of projects, on which compilation depends *)
  mutable package_deps_map : string package_dependency StringMap.t;
(*  mutable package_deps_sorted : string package_dependency list; *)
  (* bool = should the project be linked (true) or just a dependency (false) *)
  mutable package_requires : package package_dependency list;
  mutable package_added : bool;

  mutable package_options : BuildOCPVariable.options;
}

and 'a package_dependency =
    {
      dep_project : 'a;
      mutable dep_link : bool;
      mutable dep_syntax : bool;
      mutable dep_optional : bool;
    }

and project = {
  project_files : File.t list;
  mutable project_disabled : package array;
  mutable project_incomplete : package array;
  mutable project_sorted : package array;
  mutable project_missing : (string * package list) list;
}

(*
let disabled_projects = ref ([] : BuildTypes.package_info list)
let incomplete_projects = ref ([] : BuildTypes.package_info list)
*)












open BuildOCPVariable

let package_option = new_strings_option "package" ([] : string list)
let dirname_option = new_strings_option "dirname" ([] : string list)
let subdir_option = new_strings_option "subdir" ([] : string list)
let cclib_option = new_strings_option "cclib" ([] : string list)
let ccopt_option = new_strings_option "ccopt" ([] : string list)
let cflags_option = new_strings_option "cflags" ([] : string list)
let pp_option = new_strings_option "pp" []


let install_interface_option = new_bool_option "install_cmi" true
let generated_option = new_bool_option "generated" false
let enabled_option = new_bool_option "enabled" true
let byte_option = new_bool_option "has_byte" true
let asm_option = new_bool_option "has_asm" true
let pack_option = new_strings_option "pack" ([] : string list)
let packed_option = new_strings_option "packed" ([] : string list)
let ml_file_option = new_bool_option "ml" false
let sort_files_option = new_bool_option "sort" false
let mli_file_option = new_bool_option "mli" false
let pp_requires = new_strings_option "pp_requires" []
let no_mli_option = new_bool_option "no_mli" false

let syntax_option = new_strings_option "syntax" ([] : string list)
let syntaxes_option = new_strings_option "syntaxes" ([] : string list)

let bytecomp_option = new_strings_option "bytecomp" ([] : string list)
let bytelink_option = new_strings_option "bytelink" ([] : string list)
let asmcomp_option = new_strings_option "asmcomp" ([] : string list)
let asmlink_option = new_strings_option "asmlink" ([] : string list)
let dep_option = new_strings_option "dep" ([] : string list)

let rec string_of_condition cond =
  match cond with
    | IsEqualStringList (name, list) ->
      Printf.sprintf "%s = [ %s ]" name (String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) list))
    | IsTrue name ->
      Printf.sprintf "%s" name
    | NotCondition cond ->
      Printf.sprintf "not ( %s )" (string_of_condition cond)
    | AndConditions (cond1, cond2) ->
      Printf.sprintf "( %s ) && ( %s )" (string_of_condition cond1) (string_of_condition cond2)
    | OrConditions (cond1, cond2) ->
      Printf.sprintf "( %s ) || ( %s )" (string_of_condition cond1) (string_of_condition cond2)

let rec string_of_set_option option =
  match option with
    | OptionListSet (x, list) ->
      Printf.sprintf "%s = [ %s ]" x (String.concat " " (List.map (fun s ->
        Printf.sprintf "\"%s\"" (String.escaped s)) list))
  | OptionListAppend (x, list) ->
      Printf.sprintf "%s += [ %s ]" x (String.concat " " (List.map (fun s ->
        Printf.sprintf "\"%s\"" (String.escaped s)) list))
  | OptionListRemove (x, list) ->
    Printf.sprintf "%s -= [ %s ]" x (String.concat " " (List.map (fun s ->
      Printf.sprintf "\"%s\"" (String.escaped s)) list))
  | OptionBoolSet (x, y) -> Printf.sprintf "%s = %b" x y
  | OptionConfigSet c -> Printf.sprintf "config \"%s\"" c
  | OptionIfThenElse (cond, ifthen, ifelse) ->
    Printf.sprintf "if %s then %s%s"
      (string_of_condition cond)
      (string_of_one_option ifthen)
      (match ifelse with None -> ""
        | Some ifelse -> Printf.sprintf " else %s"
          (string_of_one_option ifelse))
  | OptionBlock options ->
    Printf.sprintf "begin\n%s\nend"
      (String.concat "\n" (List.map string_of_set_option options))

and string_of_one_option options =
  match options with
      [ option ] -> string_of_set_option option
    | options ->
      Printf.sprintf "{\n%s\n}"
        (String.concat "\n" (List.map string_of_set_option options))

