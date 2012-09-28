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

open BuildOCPVariable
open BuildOCPTree
open BuildOCPTypes

type config = {
  config_options : BuildOCPVariable.options;
  config_files : (string * set_option list) list;
  config_requires : string list;
  config_dirname : string;
  config_filename : string;
}

type state = {
  mutable packages : package IntMap.t;
  mutable npackages : int;
}

let initial_state () =
{ packages = IntMap.empty; npackages = 0; }

let final_state state =
  if state.npackages = 0 then [||] else
    Array.init state.npackages (fun i ->
      IntMap.find i state.packages
    )

let new_package pj name dirname filename =
  let package_id = pj.npackages in
  pj.npackages <- pj.npackages + 1;
  let pk = {
    package_id = package_id;
    package_tag = "";
    package_auto = None;
    package_version = "";
    package_loc = (-1);
    package_filename = filename;
    package_node = LinearToposort.new_node ();
    package_added = false;
    package_requires = [];
    package_name = name;
    package_missing_deps = 0;
    package_provides = name;
    package_type = ProjectLibrary;
(*    package_native = true; *)
(*    package_enabled = true; *)
    package_sources = [];
    package_files = [];
    package_dirname = dirname;
    package_deps_map = StringMap.empty;
    package_deps_sorted = [];
    package_options = new_options ();
(*    package_cflags = ""; *)
(*    package_cclib = ""; *)
(*    package_details = None; *)
(*    package_has_byte = true; *)
    package_has_byte_debug = false;
(*    package_has_asm = true; *)
    package_has_asm_debug = false;
    package_has_asm_profile = false;
  } in
  pj.packages <- IntMap.add pk.package_id pk pj.packages;
  pk

let empty_config set_defaults =
  let options = BuildOCPVariable.new_options () in
  let options =
    {  options_vars =
	List.fold_left (fun vars f -> f vars)
	  options.options_vars set_defaults }
  in
  { config_options = options;
    config_files = [];
    config_requires = [];
    config_dirname = "";
    config_filename = "";
  }

let configs = Hashtbl.create 17


let define_config config_name options = Hashtbl.add configs config_name options
let find_config config_name = Hashtbl.find configs config_name

let option_list_set options name list =
  { options_vars =
      StringMap.add name (OptionList list) options.options_vars }

let rec option_list_remove prev list =
  match list with
      [] -> prev
    | first_ele :: next_eles ->
      let rec iter prev before =
        match prev with
            [] -> List.rev before
          | x :: tail ->
            if x = first_ele then
              cut_after x tail tail next_eles before
            else
              iter tail (x :: before)

      and cut_after head tail list1 list2 before =
        match list1, list2 with
            _, [] -> iter list1 before
          | x1 :: tail1, x2 :: tail2 when x1 = x2 ->
            cut_after head tail tail1 tail2 before
          | _ -> iter tail (head :: before)
      in
      iter prev []

let option_list_remove options name list =
  try
    match StringMap.find name options.options_vars with
	OptionList prev ->
          let new_list = option_list_remove prev list in
          {               options_vars =
              StringMap.add name (OptionList new_list) options.options_vars }
      | _ ->
	failwith  (Printf.sprintf "OptionListRemove %s: incompatible values" name);
  with Not_found -> options



let option_list_append options name list =
  let list =
    try
      match StringMap.find name options.options_vars with
	  OptionList prev -> prev @ list
	| _ ->
	  failwith  (Printf.sprintf "OptionListAppend %s: incompatible values" name);
    with Not_found -> list
  in
  {
    options_vars =
      StringMap.add name (OptionList list) options.options_vars }

let options_list_append options names list =
  List.fold_left (fun options var ->
    option_list_append options var list
  ) options names

let options_list_remove options names list =
  List.fold_left (fun options var ->
    option_list_remove options var list
  ) options names

let options_list_set options names list =
  List.fold_left (fun options var ->
    option_list_set options var list
  ) options names

let meta_options = [
  "o",      [ "dep"; "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "oc",      [ "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "byte",   [        "bytecomp"; "bytelink"; ];
  "asm",   [        "asmcomp"; "asmlink"; ];
  "comp",   [        "bytecomp"; "asmcomp"; ];
  "link",   [        "bytelink"; "asmlink"; ];
]

let meta_options =
  let list = ref StringMap.empty in
  List.iter (fun (name, names) -> list := StringMap.add name names !list) meta_options;
  !list

let option_list_set options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_set options names list
  with Not_found -> option_list_set options name list

let option_list_append options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_append options names list
  with Not_found -> option_list_append options name list


let option_list_remove options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_remove options names list
  with Not_found -> option_list_remove options name list

let rec translate_condition options cond =
  match cond with
    | IsEqualStringList (name, list) ->
      let name_value = try
                         StringMap.find name options.options_vars
        with Not_found -> OptionList []
      in
      (*      Printf.fprintf stderr "translate_condition %s = %S = %S\n%!"
	      name (match name_value with
	      OptionList name_value -> String.concat ";"  name_value
	      | _ -> "???" )
	      (String.concat ";" list); *)
      name_value = OptionList list
    | IsTrue name ->
      let name_value = try
                         StringMap.find name options.options_vars
        with Not_found -> OptionBool false
      in
      name_value = OptionBool true
    | NotCondition cond -> not (translate_condition options cond)
    | AndConditions (cond1, cond2) ->
      (translate_condition options cond1) && (translate_condition options cond2)
    | OrConditions (cond1, cond2) ->
      (translate_condition options cond1) || (translate_condition options cond2)

let rec translate_options options list =
  match list with
      [] -> options
    | option :: list ->
      let options = translate_option options option in
      translate_options options list

and translate_option options op =
  match op with
    | OptionConfigSet config_name ->
      find_config config_name options

    | OptionListSet (name, list) -> option_list_set options name list

    | OptionListAppend (name, list) -> option_list_append options name list
    | OptionListRemove (name, list) -> option_list_remove options name list

    | OptionBoolSet (name, bool) ->
      {
	options_vars =
	  StringMap.add name (OptionBool bool) options.options_vars }

    | OptionIfThenElse (cond, ifthen, ifelse) ->
      begin
        if translate_condition options cond then
          translate_options options ifthen
        else
          match ifelse with
              None -> options
            | Some ifelse ->
              translate_options options ifelse
      end
    | OptionBlock list -> translate_options options list

(*
  module MakeParser(BuildGlobals : sig

  type project_info

  val new_project_id : unit -> int
  val register_project : project_info  BuildOCPTypes.project -> unit
  val new_project :
(* project name *) string ->
(* project dirname *) string ->
(* configuration filename *) string ->
  project_info BuildOCPTypes.project

  val register_installed : string -> unit

  end) = struct
*)


let add_project_dep pk link s =
  try
    let pd = StringMap.find s pk.package_deps_map in
    pd.dep_link <- pd.dep_link || link;
    pd
  with Not_found ->
    let pd = {
      dep_for = [];
      dep_project = s;
      dep_link = link;
    } in
    pk.package_deps_map <- StringMap.add s pd pk.package_deps_map;
    pd


let define_package pj name config kind =
  let pk = new_package pj name
    config.config_dirname config.config_filename
  in
  let project_options = config.config_options in
  pk.package_type <- kind;
  pk.package_provides <- name;
  pk.package_sources <-  config.config_files;
  pk.package_deps_sorted <- List.map (fun dep ->
    let dep_name, kind = OcpString.cut_at dep ':' in
    let link = match kind with
        "nolink" | "pp" -> false
      | "" -> true
      | _ ->
        Printf.eprintf "Warning: for package %s, require %S has unknown kind\n%!" name dep;
        false
    in
    add_project_dep pk link dep_name) config.config_requires;
  pk.package_options <- project_options

let rec translate_toplevel_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config = translate_toplevel_statement pj config stmt in
      translate_toplevel_statements pj config list

and translate_toplevel_statement pj config stmt =
  match stmt with
    | StmtDefineConfig (config_name, options) ->
      begin
        (*	      let options = translate_options config.config_options options in *)
	define_config config_name (fun old_options -> translate_options old_options options);
      end;
      config
    | StmtDefinePackage (package_type, library_name, simple_statements) ->
      begin
	let config = translate_statements pj config simple_statements in
	define_package pj library_name config package_type
      end;
      config
    | StmtBlock statements ->
      ignore (translate_toplevel_statements pj config statements);
      config
    | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config.config_options cond then
        translate_toplevel_statements pj config ifthen
      else
        match ifelse with
            None -> config
          | Some ifelse ->
            translate_toplevel_statements pj config ifelse
    end
    | _ -> translate_simple_statement pj config stmt

and translate_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config = translate_statement pj config stmt in
      translate_statements pj config list

and translate_statement pj config stmt =
  match stmt with
    | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config.config_options cond then
        translate_statements pj config ifthen
      else
        match ifelse with
            None -> config
          | Some ifelse ->
            translate_statements pj config ifelse
    end
    | _ -> translate_simple_statement pj config stmt

and translate_simple_statement pj config stmt =
  match stmt with
    | StmtRequiresSet requirements ->
      { config with config_requires = requirements }
    | StmtRequiresAppend requirements ->
      { config with config_requires = config.config_requires @ requirements }
    | StmtFilesSet files ->
      (*      let files = List.map (fun (file, options) ->
	      (file, translate_options config.config_options options)) files in *)
      { config with config_files = files }
    | StmtFilesAppend files ->
      (*      let files = List.map (fun (file, options) ->
	      (file, translate_options config.config_options options)) files in *)
      { config with config_files = config.config_files @ files }
    | StmtOption option ->
      { config with config_options = translate_option config.config_options option }
    | StmtSyntax (syntax_name, camlpN, extensions) -> config
    | StmtIfThenElse _
    | StmtBlock _
    | StmtDefinePackage _
    | StmtDefineConfig _ -> assert false

let read_ocamlconf pj config filename =
  let ast = BuildOCPParse.read_ocamlconf filename in
  translate_toplevel_statements pj
    { config
      with
	config_files = [];
	config_requires = [];
	config_dirname = Filename.dirname filename;
	config_filename = filename;
    } ast
