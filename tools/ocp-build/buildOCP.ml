(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open OcpLang
open BuildOCPTree
open BuildOCPTypes
open SimpleConfig
open BuildOCPVariable

let verbose = DebugVerbosity.verbose ["B";"BP"] "BuildOCP"

type state = {
  validated : (string * string, package) Hashtbl.t;
  missing : (string * string, package list ref) Hashtbl.t;
}

let rec validate_project s pk =
  if verbose 2 then
    Printf.eprintf "validate_project: %s, tag=%s, id=%d\n" pk.package_name pk.package_tag pk.package_id;
  if pk.package_missing_deps = 0 then begin
    let key =  (pk.package_name, pk.package_tag) in
    begin try
            let pk2 = Hashtbl.find s.validated key in
            Printf.eprintf "Error: two projects called %s\n" pk.package_name;
            Printf.eprintf "  One is defined in %s\n" pk.package_dirname;
            Printf.eprintf "  and one is defined in %s\n" pk2.package_dirname;
            Printf.eprintf "%!";
            exit 2
      with Not_found -> ()
    end;
    Hashtbl.add s.validated key pk;

    pk.package_requires <- List.map (fun pd_dep ->
	let pk = Hashtbl.find s.validated (pd_dep.dep_project, "") in
	{ pd_dep with dep_project = pk }
    ) pk.package_deps_sorted;

    try
      let list_ref = Hashtbl.find s.missing key in
      Hashtbl.remove s.missing key;
      List.iter (fun pk2 ->
	pk2.package_missing_deps <- pk2.package_missing_deps - 1;
	validate_project s pk2
      ) !list_ref;
    with Not_found -> ()
  end

let check_project s pk =
  if bool_option_true pk.package_options enabled_option then begin

    pk.package_missing_deps <- 0;
    StringMap.iter (fun name pkdep ->
      let key = (name, "") in (* TODO: we should use a datastructure that can handle
                                 dependencies by tag and by version *)
      if not (Hashtbl.mem s.validated key) then
	let list_ref =
	  try
	    Hashtbl.find s.missing key
	  with Not_found ->
	    let list_ref = ref [] in
	    Hashtbl.add s.missing key list_ref;
	    list_ref
	in
	list_ref := pk :: !list_ref;
	pk.package_missing_deps <- pk.package_missing_deps + 1
    ) pk.package_deps_map;
    validate_project s pk
  end



(*
val find_project : (File.t -> File.t)
*)
let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = File.add_basenames dirname basenames in
    if File.X.exists file then dirname else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if File.is_absolute root_dir then root_dir else
      File.concat (File.X.getcwd ()) root_dir
  in
  find root_dir basenames

(*
val open_project : (File.t -> project)
let open_project files =

(*
  let config_file = SimpleConfig.create_config_file file_t in
*)
(*  SimpleConfig.load config_file; *)
  pj
*)

module PackageDepSorter = LinearToposort.Make(struct
  type t = package  package_dependency
  let node pd = pd.dep_project.package_node
  let iter_edges f pd = List.iter f pd.dep_project.package_requires
  let name pd = pd.dep_project.package_name
end)

module PackageLinkSorter = LinearToposort.Make(struct
  type t = package  package_dependency
  let node pd = pd.dep_project.package_node
  let iter_edges f pd =
    List.iter (fun pd ->
      if pd.dep_link then
        f pd) pd.dep_project.package_requires
  let name pd = pd.dep_project.package_name
end)


(* Do a closure of all dependencies for this project *)
let update_deps pj =

  if verbose 2 then begin
    Printf.eprintf "BEFORE update_deps: Project %s depends on:\n%!" pj.package_name;
    List.iter (fun pd ->
      Printf.eprintf "\t%s%s\n%!" pd.dep_project.package_name
	(if pd.dep_link then "" else "(nolink)")
    ) pj.package_requires
  end;

  let deps = Hashtbl.create 111 in
  let list = ref
    (List.filter (fun pd ->
      if not (Hashtbl.mem deps pd.dep_project.package_id) then begin
        Hashtbl.add deps pd.dep_project.package_id pd;
        true
      end else false)
    (List.map (fun pd -> { pd with dep_link = false } ) pj.package_requires)) in
  let rec add_dep pd =
    let pj2 = pd.dep_project in
    let dep_link =
      match pj2.package_type with
	  ProjectLibrary
	| ProjectObjects -> pd.dep_link
	| ProjectProgram -> false
(*	| ProjectToplevel -> false *)
    in
    let add_more_deps =
      try
	let pd2 = Hashtbl.find deps pj2.package_id in
	if (not pd2.dep_link) && dep_link then begin
	  pd2.dep_link <- true;
	  true
	end else false
      with Not_found ->
	let pd2 = { pd with dep_link = dep_link } in
	list :=  pd2 :: !list;
	Hashtbl.add deps pj2.package_id pd;
	dep_link
    in
    if add_more_deps then
      List.iter add_dep pj2.package_requires
  in
  List.iter add_dep pj.package_requires;
  pj.package_requires <- !list;

(*  TODO: do better
List.iter (fun pd ->
    List.iter add_dep pj.package_requires
  ) pj.package_requires; *)

  if verbose 2 then begin
    Printf.eprintf "BEFORE update_deps SORT: Project %s depends on:\n%!" pj.package_name;
    List.iter (fun pd ->
      Printf.eprintf "\t%s%s\n%!" pd.dep_project.package_name
	(if pd.dep_link then "" else "(nolink)")
    ) pj.package_requires
  end;

  pj.package_requires <- PackageLinkSorter.sort (* sort_sorted *) !list;

  if verbose 2 then begin
    Printf.eprintf "AFTER update_deps: Project %s depends on:\n%!" pj.package_name;
    List.iter (fun pd ->
      Printf.eprintf "\t%s%s\n%!" pd.dep_project.package_name
	(if pd.dep_link then "" else "(nolink)")
    ) pj.package_requires
  end;

  ()


let reset_package_ids array =
  for i = 0 to Array.length array - 1 do
    array.(i).package_id <- i
  done

(*
val load_packages : (project -> int)
*)
(* Note that files should be sorted from the most internal one to
the deepest ones. *)

let load_project files =

  let state = {
    missing = Hashtbl.create 111;
    validated = Hashtbl.create 111;
  }
  in

(*
  let pj =
  {
   (*
 project_config = config_file;
    project_file = file_t;
    project_dir = File.dirname file_t;
   *)
    project_files = files;
    project_packages = IntMap.empty;
    project_npackages = 0;
    project_disabled = [];
    project_incomplete = [];
    project_sorted = [];
    project_missing = [];
  }
  in
*)


  let config = BuildOCPInterp.empty_config !BuildOCPVariable.options in

  let nerrors = ref 0 in

  let packages = BuildOCPInterp.initial_state () in

  let rec iter parents files =
    match files with
	[] -> ()
      | file :: next_files ->
	match parents with
	    [] -> assert false
	  | (parent, config) :: next_parents ->
            let file = File.to_string file in
	    if OcpString.starts_with file parent then
	      let dirname = Filename.dirname file in
	      if verbose 3 then
	        Printf.eprintf "Reading %s with context from %s\n%!" file parent;
	      let config =
		try
		  BuildOCPInterp.read_ocamlconf packages config file
		with BuildMisc.ParseError ->
		  incr nerrors;
		  config
	      in
	      iter ( (dirname, config) :: parents ) next_files
	    else
	      iter next_parents files
  in
  let _config = iter [ "", config ] files in

  let packages = BuildOCPInterp.final_state packages in

(* Before sorting the project, let's add syntaxes *)
  Array.iter (fun pk ->
    if bool_option_true pk.package_options enabled_option then begin
      List.iter (fun lib ->
        ignore (BuildOCPInterp.add_project_dep pk false lib
                  :  string package_dependency)
      ) (strings_option pk.package_options syntaxes_option)
    end
  ) packages;

  Array.iter (fun pk -> check_project state pk) packages;

  let project_incomplete = ref [] in
  let project_disabled = ref [] in

  Array.iter (fun pk ->
    if bool_option_true pk.package_options enabled_option then begin
      if pk.package_missing_deps > 0 then
	project_incomplete := pk :: !project_incomplete
    end else
      project_disabled := pk :: !project_disabled
  ) packages;

  let list = ref [] in
  Hashtbl.iter (fun _ pj ->
    list := { dep_project = pj; dep_for = []; dep_link = true } :: !list
  ) state.validated;

  let project_missing = ref [] in
  Hashtbl.iter (fun (name, _) list_ref ->
    project_missing := (name, !list_ref) :: !project_missing)
    state.missing;
(* Note that the result of this function can contain more elements
  as the initial list, as new dependencies are automatically added. *)
  let list = PackageDepSorter.sort !list in
  let project_sorted = List.map (fun pd -> pd.dep_project) list in
  List.iter update_deps project_sorted;

  let npackages = Array.length packages in

  let pj = {
    project_files = files;
    project_sorted = Array.of_list project_sorted;
    project_missing = !project_missing;
    project_disabled = Array.of_list !project_disabled;
    project_incomplete = Array.of_list !project_incomplete;
  } in
  assert (npackages =
      Array.length pj.project_sorted +
      Array.length pj.project_incomplete +
      Array.length pj.project_disabled);
  reset_package_ids pj.project_sorted;
  Array.iter (fun pk ->
    pk.package_requires <- List.sort (fun dep1 dep2 ->
      compare
        dep1.dep_project.package_id
        dep2.dep_project.package_id) pk.package_requires
  ) pj.project_sorted;

  reset_package_ids pj.project_incomplete;
  reset_package_ids pj.project_disabled;

  pj, !nerrors

(*
val save_project : (File.t -> (project -> unit))
let save_project file_t pj =
  save_with_help pj.project_config
*)


let scan_root root_dir =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string root_dir) ".ocp" (fun filename ->
    files := File.of_string filename :: !files);
  List.rev !files
(* files are reverted, so that the first in breadth are used first
(this is expected from [load_project] *)

let magic_head = "OCP-"
let magic_head_len = String.length magic_head
let magic_kind = "PROJ"
let magic_kind_len = String.length magic_kind
let magic_version = "20120928"
let magic_version_len = String.length magic_version
let magic = magic_head ^ magic_kind ^ magic_version
let magic_len = String.length magic

let save_project_state state filename =
  let oc = File.X.open_out filename in
  output_string oc magic;
  output_value oc (state : project);
  close_out oc

let load_project_state filename =
  let ic = File.X.open_in filename in
  let possible_magic = String.create magic_len in
  begin try
          really_input ic possible_magic 0 magic_len;
    with e ->
      close_in ic;
      failwith "load_project_state: truncated file"
  end;
  if possible_magic <> magic then begin
    close_in ic;
    if String.sub possible_magic 0 magic_head_len <> magic_head then
      failwith "load_project_state: not an OCP file";
    if String.sub possible_magic magic_head_len magic_kind_len
      <> magic_kind then
      failwith "load_project_state: not an OCP PROJECT file";
    if String.sub possible_magic (magic_head_len + magic_kind_len)
      magic_version_len <> magic_version then
      failwith "load_project_state: bad OCP PROJECT version";
  end;
  try
    let v = (input_value ic : project) in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

(*
val scan_project : (project -> unit)
let scan_project pj =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string pj.project_dir) ".ocp" (fun filename ->
    files := File.of_string filename :: !files);
  pj.project_files =:= List.rev !files;
  save_project pj.project_file pj;
  ()
*)

(*

  if !list_ocp_files || !verbosity_arg > 1 then begin
    Printf.eprintf "Found %d project files:\n%!" (List.length !files);
    List.iter (fun file ->
      Printf.eprintf "\t%s\n%!" file) !files;
  end;

*)

let find_package pj file =
  let list = ref [] in

  let st = File.X.lstat file in
(*
  let dir_t = pj.project_dir in
  let _dir = File.to_string dir_t in
*)
  let check_file pk filename =
    let file = File.of_string (Filename.concat pk.package_dirname filename) in
    try
      let st2 = File.X.lstat file in
      if
        st.Unix.st_ino = st2.Unix.st_ino &&
        st.Unix.st_dev = st2.Unix.st_dev then
        list := pk :: !list
    with _ -> ()
  in
  Array.iter (fun pk ->
    List.iter (fun (filename, _) ->
      check_file pk filename;
      let (kernel, extension) = File.cut_last_extension filename in
      match extension with
        | "ml" -> check_file pk (filename ^ ".mli")
        | "mli" -> ()
        | "mll" -> check_file pk (filename ^ ".ml")
        | "mly" ->
          check_file pk (filename ^ ".ml");
          check_file pk (filename ^ ".mli")
        | _ -> ()
    ) pk.package_sources
  ) pj.project_sorted;

  !list

let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir
