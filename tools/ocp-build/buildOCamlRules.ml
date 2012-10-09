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

(* TODO:

   Currently, there is no verification that all the dependencies
   appearing in pp_requires also appear in requires. Actually,
   pp_requires could be automatically added.

   We could simplify this by:

   ppflags = [ "" ]

   requires =

   syntax = [ "pa_dyntype" ]

   A syntax MUST either depend on another syntax (requires = [ "toto" ])
   where toto is a syntax, OR set the "pp_master" option. If we depend
   on a package, and this package depends on another syntax, it does
   not matter.



*)

open OcpLang
open OcpSystem


open BuildMisc

open BuildEngineTypes
open BuildEngineGlobals
open BuildEngineContext
open BuildEngineRules
open BuildEngineRules


open BuildOCPVariable
open BuildOCPTree
open BuildOCPTypes

open BuildTypes
open BuildGlobals
open BuildConfig

open BuildOCamlTypes
open BuildOCamlVariables
open BuildOCamlMisc

let mut_dir lib src_file =
  let rec iter mut_dir file_dir =
(*
    Printf.eprintf "src_dir = %S\n%!" lib.lib_src_dir.dir_fullname;
    Printf.eprintf "fil_dir = %S\n%!" file_dir.dir_fullname;
    Printf.eprintf "mut_dir = %S\n%!" lib.lib_mut_dir.dir_fullname;
*)
    if lib.lib_src_dir == file_dir
      || lib.lib_mut_dir == file_dir
    then mut_dir else
      let parent_dir = file_dir.dir_parent in
(*      Printf.eprintf "check parent\n"; *)
      assert (lib.lib_mut_dir.dir_fullname <> file_dir.dir_fullname);
      let mut_dir = iter mut_dir parent_dir in
      let subdir = Filename.concat mut_dir.dir_fullname file_dir.dir_basename
      in
      if not (Sys.file_exists subdir) then
        Unix.safe_mkdir subdir 0o755;
      add_directory lib.lib_context subdir
  in
  iter lib.lib_mut_dir  src_file.file_dir

let verbose = DebugVerbosity.verbose ["B"] "BuildOCamlRules"

let chop_prefix s prefix =
  let prefix_len = String.length prefix in
  String.sub s prefix_len (String.length s - prefix_len)

(*
module ProjectSorter = LinearToposort.Make(struct
  type t = BuildTypes.package_info  package_dependency
  let node pd = pd.dep_project.lib_node
  let iter_edges f pd = List.iter f pd.dep_project.lib_requires
  let name pd = pd.dep_project.lib_name
end)
*)



(* Within a project... *)
(* let cma_files = ref []
let cmxa_files = ref [] *)
let src_files = ref IntMap.empty
let dep_files = ref IntMap.empty
let cmi_files = ref []
let cmo_files = ref []
let cmx_files = ref []
let cmxo_files = ref [] (* .o files generated with .cmx files *)
let o_files = ref []

(* TODO: must do something more correct !! *)
let ocaml_version_greater_than version options =
  let ocaml_version = string_option options ocaml_config_version in
  ocaml_version >= version

let add_bin_annot_argument cmd options =
  if ocaml_version_greater_than "4" options then
    add_command_args cmd [S "-bin-annot" ]

let command_includes lib pack_for =
  let includes =
    match lib.lib_includes with
      | Some includes -> includes
      | None ->

	let added_dirs = ref IntMap.empty in
	let includes = ref [] in
	let add_include_dir dir =
	  if not (IntMap.mem dir.dir_id !added_dirs) then begin
	    added_dirs := IntMap.add dir.dir_id dir !added_dirs;
	    includes := !includes @ ["-I"; dir.dir_fullname];
	  end
	in

	add_include_dir lib.lib_dst_dir;
	add_include_dir lib.lib_src_dir;

      (* TODO: Fabrice: they should be reversed, no ?
         We should search directories in the
	 reverse order of the topological order. *)
	List.iter (fun dep ->
          let lib = dep.dep_project in
          match lib.lib_type with
              ProgramPackage (* | ProjectToplevel *) -> ()
            | LibraryPackage | ObjectsPackage ->
              if dep.dep_link then begin
	        add_include_dir lib.lib_dst_dir;
	        add_include_dir lib.lib_src_dir;
              end
	) lib.lib_requires;

      (* we put the source dir last in case there are some remaining objects files there, since
	 we don't do any hygienic cleaning before. We don't do it because we want to be able to
	 support object files that are built by other means. *)

	let includes = !includes in
	lib.lib_includes <- Some includes;
	includes
  in
  let rec add_internal_includes pack_for includes =
    match pack_for with
	[] -> includes
      | _ :: tail ->
	let includes = add_internal_includes tail includes in
	let includes = "-I" :: (Filename.concat lib.lib_dst_dir.dir_fullname (String.concat "/" (List.rev pack_for))) ::
	includes in
	includes
  in
  add_internal_includes (List.rev pack_for) includes

(*
let command_pp pj options =
  match string_option options pp_option with
    | "" -> []
    | pp -> ["-pp"; pp]
*)

let add_c2o_rule b lib pj seq src_file target_file options =
  let build_dir = add_directory b (Unix.getcwd ()) in
  let temp_file = BuildEngineContext.add_temp_file b build_dir target_file.file_basename in
  let r = new_rule b lib.lib_loc target_file
    [Execute (new_command

		(strings_option options ocamlcc_cmd
(*
		       (if bool_option_true pj.lib_options byte_option then ocamlcc_cmd
		       else ocamlopt_cmd) *)
                )
		[
                  S "-I"; S src_file.file_dir.dir_fullname;
		 S "-ccopt"; S
                   (String.concat " " (strings_option options cflags_option));
		 S "-ccopt"; S (String.concat " " (strings_option options ccopt_option));
		 S "-c"; S (file_filename src_file);
		]);
     Move (F temp_file.file_file, F target_file.file_file)
    ]
  in
(*
  StringMap.iter (fun option _ ->
    Printf.eprintf "OPTION %s\n" option
  ) options.options_vars;
  begin
    match options.options_inherit with
	None -> ()
      | Some options ->
	StringMap.iter (fun option _ ->
	  Printf.eprintf "OPTION INHERITED %s\n" option
	) options.options_vars;
  end;
*)
  add_rule_source r src_file;
  add_rule_sources r seq;
  List.iter (fun dep ->
    if dep.dep_link then
      let lib = dep.dep_project in
      add_rule_sources r lib.lib_bytecomp_deps
  ) pj.lib_requires;
  add_rule_temporary r temp_file

let add_mll2ml_rule b lib pj src_file target_file options =
(*
  let src_dir = lib.lib_src_dir in
  let dst_dir = lib.lib_dst_dir in
*)

  let r = new_rule b lib.lib_loc target_file
    [Execute (new_command (strings_option options ocamllex_cmd)
                [ S "-o"; BF target_file; BF src_file])
    ]
  in
  add_rule_source r src_file

let add_mly2ml_rule b lib pj src_file ml_target_file mli_target_file options =
(*
  let src_dir = lib.lib_src_dir in
  let dst_dir = lib.lib_dst_dir in
*)
  let src_dir = src_file.file_dir in
  let temp_ml = BuildEngineContext.add_temp_file b src_dir ml_target_file.file_basename in
  let temp_mli = BuildEngineContext.add_temp_file b src_dir mli_target_file.file_basename in
  let r = new_rule b lib.lib_loc ml_target_file
    [Execute (new_command (strings_option options ocamlyacc_cmd) [BF src_file]);
     Move (BF temp_ml, BF ml_target_file);
     Move (BF temp_mli, BF mli_target_file);
    ]
  in
  add_rule_source r src_file;
  add_rule_target r mli_target_file

(* reading dependencies is a bit complicated, as the format of make
   dependencies is not OK on Windows (because : is used in
   filenames). We should fix filenames in those cases.
  Note that ocamldep will escape any space in a filename, so that
  spaces are the only significant characters.
  Read the full file. Convert \\\n sequences into spaces.


   Instead, we should have a specail format, such as:

   CMO filename
   DEP dependency
   DEP dependency

  TODO: add a String.unescaped, the inverse of String.escaped.
*)

let bytelinkflags pj =
  List.map argument_of_string (strings_option pj.lib_options bytelink_option)
let asmlinkflags pj =
  List.map argument_of_string (strings_option pj.lib_options asmlink_option)
let depflags pj options =
    List.map argument_of_string (direct_strings_option options dep_option)
let bytecompflags pj options =
    List.map argument_of_string (direct_strings_option options bytecomp_option)
let asmcompflags pj options =
    List.map argument_of_string (direct_strings_option options asmcomp_option)


let add_ml2mldep_rule lib dst_dir pack_for force src_file target_file options =
  let b = lib.lib_context in
  let cmd = new_command (strings_option options ocamldep_cmd)
    (depflags lib options) in
  (match !cross_arg with
      None -> ()
    | Some _ -> add_command_string cmd "-modules");
  add_command_strings cmd (command_includes lib pack_for);
(*  add_command_strings cmd (command_pp lib options); *)
  if force = Force_IMPL || bool_option_true options ml_file_option then
    add_command_strings cmd [ "-impl" ]
  else
  if force = Force_INTF || bool_option_true options mli_file_option then
    add_command_strings cmd [ "-intf" ]
  ;
  add_command_strings cmd [file_filename src_file];
  add_command_pipe cmd (file_filename target_file);

  let r = new_rule lib.lib_context lib.lib_loc target_file [Execute cmd] in
  add_rule_source r src_file;
(* We don't need to have all the sources available ! Actually, the computation of
  dependencies is not done on the file-system, but on the virtual image of the
  file system, so files don't need to be present, they just need to be known to exist...

  List.iter (fun pd ->
    let lib = pd.dep_project in
    IntMap.iter (fun _ file -> add_rule_source r file) lib.lib_dep_deps
  ) lib.lib_requires;
*)

  let mldep_file_loaded = add_virtual_file b dst_dir (target_file.file_basename ^ " loaded") in
  let mldep_file_ok = add_virtual_file b dst_dir (target_file.file_basename ^ " ok") in

  let r_ok = new_rule b lib.lib_loc mldep_file_ok [] in
  r_ok.rule_forced <- true;  (* must be executed, even when no changes *)
  add_rule_source r_ok mldep_file_loaded;

  let loader =
    match !cross_arg with
	None -> BuildOCamldep.load_dependencies
      | Some _ -> BuildOCamldep.load_modules_dependencies lib options force dst_dir pack_for
  in
  let r_loaded = new_rule b lib.lib_loc mldep_file_loaded [ LoadDeps (loader, target_file, r_ok) ] in
  r_loaded.rule_forced <- true; (* must be executed, even when no changes *)
  add_rule_source r_loaded target_file;

  mldep_file_ok






type 'a to_sort =
    {
      to_sort_value : 'a;
      to_sort_node : LinearToposort.node;
      mutable to_sort_deps : 'a to_sort list;
    }

module FileSorter = LinearToposort.Make(struct
  type t = build_file to_sort
  let node to_sort = to_sort.to_sort_node
  let iter_edges f to_sort = List.iter f to_sort.to_sort_deps
  let name to_sort = file_filename to_sort.to_sort_value
end)

let sort_ocaml_files cmo_files =

  if verbose 2 then begin
    Printf.eprintf "Sorting:\n";
    List.iter (fun file ->
      Printf.eprintf "%s " file.file_basename;
    ) cmo_files;
  end;

  let map = ref StringMap.empty in
  let list = ref [] in
  let cmo_files = List.map (fun file ->
    let modname = Filename.chop_extension file.file_basename in
    modname.[0] <- Char.uppercase modname.[0];
    let to_sort =  {
      to_sort_value = file;
      to_sort_node = LinearToposort.new_node();
      to_sort_deps = [];
    } in
    map := StringMap.add modname to_sort !map;
    list := to_sort :: !list;
    (file, to_sort)
  ) cmo_files in

  (* reverse to keep original order *)
  let list = List.rev !list in
  List.iter (fun (file, to_sort) ->
    List.iter (fun r ->
      if r.rule_state <> RULE_INACTIVE then
	IntMap.iter (fun _ file2 ->
	  try
	    let modname = Filename.chop_extension file2.file_basename in
	    modname.[0] <- Char.uppercase modname.[0];
	    let to_sort2 = StringMap.find modname !map in
	    if to_sort2 != to_sort then
	      to_sort.to_sort_deps <- to_sort2 :: to_sort.to_sort_deps
	  with Not_found -> ()
	) r.rule_sources
    ) file.file_target_of

  ) cmo_files;

  let cmo_files =
    List.map (fun to_sort -> to_sort.to_sort_value) (FileSorter.sort list) in

  if verbose 2 then begin
    Printf.eprintf "\n";
    Printf.eprintf "Sorted:\n";
    List.iter (fun file ->
      Printf.eprintf "%s " file.file_basename;
    ) cmo_files;
    Printf.eprintf "\n";
  end;
  cmo_files




let add_files_to_link_to_command cmd options cmx_files =
  if bool_option_true options sort_files_option then begin
    DynamicAction (
      "sort for asm library",
      lazy (
	let cmx_files = sort_ocaml_files cmx_files in
	List.iter (fun cmx_file ->
	  add_command_args cmd [BF cmx_file]) cmx_files;
	[Execute cmd]
      )
    )
  end else begin
    List.iter (fun cmx_file ->
      add_command_args cmd [BF cmx_file]) cmx_files;
    Execute cmd
  end




let add_cmos2cma_rule b lib pj cclib cmo_files cma_file =
  if not lib.lib_installed then
    let options = pj.lib_options in


    let cmd = new_command (strings_option options ocamlc_cmd)
      (bytelinkflags pj) in
    add_command_args cmd  [S "-a"; S "-o"; BF cma_file];
    if cclib <> "" then
      add_command_strings cmd ["-custom"; "-cclib"; cclib ];

    let cmd = add_files_to_link_to_command cmd options cmo_files in
    let r = new_rule b lib.lib_loc cma_file [cmd] in
    add_rule_sources r cmo_files;
    add_rule_sources r !cmi_files





let cross_move r list =
  match !cross_arg with
      None -> ()
    | Some arch ->
      r.rule_commands <- r.rule_commands @
	(List.map (fun (f1, f2) ->
	  Move (f1, f2)
	 ) list)



let add_cmxs2cmxa_rule b lib pj cclib cmi_files cmx_files cmxo_files =
  let options = pj.lib_options in
  let src_dir = lib.lib_src_dir in
  let dst_dir = lib.lib_dst_dir in

  let basename_cmxa = pj.lib_name ^ ".cmxa" in
  let ext_lib = string_option options BuildConfig.ocaml_config_ext_lib in
  let basename_a = pj.lib_name ^ ext_lib in

  let cmxa_file = add_dst_file b dst_dir basename_cmxa in
  let a_file = add_dst_file b dst_dir basename_a in

  if not lib.lib_installed then begin
    let temp_cmxa = add_temp_file b src_dir basename_cmxa in
    let temp_a = add_temp_file b src_dir basename_a in

    let cmd = new_command (strings_option options ocamlopt_cmd) (asmlinkflags pj) in
    add_command_args cmd [S "-a"; S "-o"; BF temp_cmxa ];
    if cclib <> "" then
      add_command_strings cmd ["-cclib"; cclib];

    let cmd = add_files_to_link_to_command cmd options cmx_files in
    let r = new_rule b lib.lib_loc cmxa_file [cmd] in
    cross_move r [ F temp_cmxa.file_file, F cmxa_file.file_file;
		   F temp_a.file_file, F a_file.file_file];
    add_rule_sources r cmx_files;
    add_rule_sources r cmxo_files;
    add_rule_sources r cmi_files;
    add_rule_target r a_file;
    add_rule_temporaries r [temp_cmxa; temp_a];
  end;
  (cmxa_file, a_file)



let add_cmos2byte_rule b lib linkflags cclib cmo_files o_files byte_file =
  if not lib.lib_installed then
    let options = lib.lib_options in
  (*
    let src_dir = lib.lib_src_dir in
    let dst_dir = lib.lib_dst_dir in
  *)

    let cmd = new_command (strings_option options ocamlc_cmd) linkflags in
    add_command_args cmd [S "-o"; BF byte_file];
    let custom = ref false in
    List.iter (fun o_file ->
      custom := true;
      add_command_args cmd [BF o_file]) o_files;
    if cclib <> "" then
      add_command_args cmd [S "-cclib"; S cclib ];
    add_command_strings cmd (command_includes lib []);
    List.iter (fun pd ->
      if pd.dep_link then
        let pj = pd.dep_project in
	match pj.lib_type with
	| LibraryPackage ->
              (*
	        List.iter (fun a_file ->
		custom := true;
		add_command_args cmd [file_filename a_file]
	        ) lib.lib_clink_deps; *)
              add_command_args cmd (bytelinkflags pj);
	      add_command_args cmd [S (pj.lib_name ^ ".cma")]
          | ObjectsPackage ->
(*            Printf.eprintf "Depends on %s\n%!" pj.lib_name; *)
            add_command_args cmd (bytelinkflags pj);
	    List.iter (fun a_file ->
	      custom := true;
	      add_command_args cmd [BF a_file]
	    ) pj.lib_clink_deps;
            List.iter (fun cmo_file ->
(*              Printf.eprintf "Link with %s\n%!" (file_filename cmo_file); *)
	      add_command_arg cmd (BF cmo_file)
            ) pj.lib_cmo_objects;
	  | _ -> ()
    ) lib.lib_requires;
    if !custom then add_command_string cmd "-custom";

    let cmd = add_files_to_link_to_command cmd options cmo_files in
    let r = new_rule b lib.lib_loc byte_file [cmd] in
    add_rule_sources r cmo_files;
    add_rule_sources r !cmi_files;
    add_rule_sources r o_files;
    List.iter (fun pd ->
      if pd.dep_link then
        let lib = pd.dep_project in
        add_rule_sources r lib.lib_clink_deps;
        add_rule_sources r lib.lib_bytelink_deps;
    ) lib.lib_requires


let add_cmxs2asm_rule b lib linkflags cclib cmx_files cmxo_files o_files opt_file =
  if  not lib.lib_installed then
    let options = lib.lib_options in
  (*
    let src_dir = lib.lib_src_dir in
    let dst_dir = lib.lib_dst_dir in
  *)
    let cmd = new_command (strings_option options ocamlopt_cmd) linkflags in
    add_command_args cmd [S "-o"; BF opt_file];
    if cclib <> "" then
      add_command_args cmd [S "-cclib"; S cclib];
    List.iter (fun o_file ->
      add_command_arg cmd (BF o_file)) o_files;
    add_command_strings cmd (command_includes lib []);
    List.iter (fun pd ->
      if pd.dep_link then
        let pj = pd.dep_project in
	match pj.lib_type with
	  | LibraryPackage ->
            add_command_args cmd (asmlinkflags pj);
	    List.iter (fun a_file ->
	      add_command_arg cmd (BF a_file)
	    ) pj.lib_clink_deps;
	    add_command_string cmd (pj.lib_name ^ ".cmxa")
          | ObjectsPackage ->
            add_command_args cmd (asmlinkflags pj);
	    List.iter (fun a_file ->
	      add_command_arg cmd (BF a_file)
	    ) pj.lib_clink_deps;
	    List.iter (fun a_file ->
	      add_command_arg cmd (BF a_file)
	    ) pj.lib_asm_cmx_objects;
	  | ProgramPackage -> () (* dependency towards a preprocessor ? *)
    ) lib.lib_requires;

    let cmd = add_files_to_link_to_command cmd options cmx_files in
    let r = new_rule b lib.lib_loc opt_file [cmd] in
    add_rule_sources r cmx_files;
    add_rule_sources r cmxo_files;
    add_rule_sources r !cmi_files;
    add_rule_sources r o_files;
    List.iter (fun pd ->
      if pd.dep_link then
        let lib = pd.dep_project in
	add_rule_sources r lib.lib_clink_deps;
	add_rule_sources r lib.lib_asmlink_deps;
    ) lib.lib_requires


let add_os2a_rule b lib pj o_files a_file =
  (*
    let src_dir = lib.lib_src_dir in
    let dst_dir = lib.lib_dst_dir in
  *)
  if not lib.lib_installed then
    let target = a_file.file_basename in
    let ext_lib = string_option lib.lib_options BuildConfig.ocaml_config_ext_lib in
    let target_without_ext = Filename.chop_suffix target ext_lib in
    let target_without_prefix = chop_prefix target_without_ext "lib" in
    let target = File.add_basename a_file.file_dir.dir_file target_without_prefix in
    let cmd = new_command [ "ocamlmklib"  ]
      [S "-custom"; S "-o"; F target] in
    List.iter (fun o_file ->
      add_command_arg cmd (BF o_file)) o_files;
    let r = new_rule b lib.lib_loc a_file
      [Execute cmd] in
    add_rule_sources r o_files;
    ()

let add_c_source b lib pj c_file options =
(*
  let src_dir = lib.lib_src_dir in
*)
  let dst_dir = lib.lib_dst_dir in

  let basename = c_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".c" in
  let ext_obj  = string_option options BuildConfig.ocaml_config_ext_obj in
  let o_file = add_dst_file b dst_dir (kernel_name ^ ext_obj) in
  if not lib.lib_installed then
    add_c2o_rule b lib pj [] c_file o_file options;
  o_files := o_file :: !o_files

let add_command_pack_args cmd modnames =
  if modnames <> [] then
    add_command_args cmd [S "-for-pack";
                          S (String.concat "." modnames)]


let move_compilation_garbage r mut_dir temp_dir kernel_name lib =
  let b = r.rule_context in

  let move_to_sources dst_dir_virt exts =
    let dst_dir = dst_dir_virt.dir_file in
    List.iter (fun ext ->
      let basename = kernel_name ^ ext in
      let src_file = File.add_basename temp_dir basename in
      let dst_file = File.add_basename dst_dir basename in
      let _maybe_file = add_file b lib.lib_mut_dir basename in
      add_rule_command r (MoveIfExists (F src_file, F dst_file, None))
    ) exts
  in
  move_to_sources lib.lib_mut_dir [ ".annot"; ".s" ];

  let move_to_build exts =
    List.iter (fun ext ->
      let basename = kernel_name ^ ext in
      let src_file = File.add_basename temp_dir basename in
      let dst_file = add_file b lib.lib_dst_dir basename in
      let link_file = add_file b mut_dir basename in
      add_rule_command r (MoveIfExists
                            (F src_file, BF dst_file, Some (BF link_file)))
    ) exts
  in
  move_to_build [ ".cmt"; ".cmti"; ".spit"; ".spot" ]




let add_mli_source b lib pj mli_file options =

  if lib.lib_installed then () else
    let _ = () in


  if IntMap.mem mli_file.file_id !src_files then begin
    Printf.eprintf "Error: interface %s should be specified before its implementation in project %s\n%!"
      (file_filename mli_file) pj.lib_name;
    exit 2
  end;

  let dst_dir = lib.lib_dst_dir in
  let pack_for = strings_option options packed_option in
  let dst_dir = match pack_for with
      [] -> dst_dir
    | modnames ->
      let name = String.concat "/" modnames in (* TODO : should be Filename.concat *)
      let full_dirname = Filename.concat dst_dir.dir_fullname name in
      safe_mkdir full_dirname;
      add_directory b full_dirname
  in

  let basename = mli_file.file_basename in
  src_files := IntMap.add mli_file.file_id mli_file !src_files;

  let mut_dir = mut_dir lib mli_file in
  let ppv = BuildOCamlSyntaxes.get_pp lib options in
  let mli_file, force =
    match ppv.pp_option with
      [] -> mli_file, Force_not
    | pp ->
        (* TODO: we should create the new_ml_file in the same subdirectory
           as the source file, not at the toplevel !! *)

      let new_mli_file =
        add_file b mut_dir (mli_file.file_basename ^ "pp")
      in

      let cmd = new_command pp [ BF mli_file ]  in
      add_command_pipe cmd (File.to_string new_mli_file.file_file);

      let r = new_rule b lib.lib_loc new_mli_file [] in
      add_rule_command r (Execute cmd);
      BuildOCamlSyntaxes.add_pp_requires r ppv;
      add_rule_source r mli_file;

      new_mli_file, Force_INTF
  in

  let kernel_name = Filename.chop_extension basename in
  let mldep_file =
    match !cross_arg with
	None -> add_dst_file b dst_dir (kernel_name ^ ".mlidep")
      | Some _ -> add_dst_file b dst_dir (kernel_name ^ ".mlimods")
  in
  let mldep_file_ok = add_ml2mldep_rule lib dst_dir pack_for force mli_file mldep_file options in
  let seq_order = [mldep_file_ok] in

  let cmi_basename = kernel_name ^ ".cmi" in
  let cmi_temp = add_temp_file b  mli_file.file_dir cmi_basename in
  let cmi_file = add_dst_file b dst_dir cmi_basename in

  let cmd =
    if bool_option_true pj.lib_options byte_option then
      let cmd = new_command (strings_option options ocamlc_cmd) (bytecompflags pj options) in
      add_bin_annot_argument cmd options;
      add_command_args cmd [S "-c"; S "-o"; BF cmi_temp];
      add_command_strings cmd (command_includes lib pack_for);
(*      add_command_strings cmd (command_pp pj options); *)
      if force = Force_INTF || bool_option_true options mli_file_option then
        add_command_args cmd [S "-intf" ];
      add_command_args cmd [BF mli_file];
      cmd
  else
    let cmd = new_command (strings_option options ocamlopt_cmd) (asmcompflags pj options) in
    add_bin_annot_argument cmd options;
    add_command_args cmd [S "-c"; S "-o"; BF cmi_temp];
    add_command_strings cmd (command_includes lib pack_for);
    add_command_pack_args cmd pack_for;
(*    add_command_strings cmd (command_pp pj options); *)
      if force = Force_INTF || bool_option_true options mli_file_option then
        add_command_string cmd  "-intf" ;
    add_command_args cmd [BF mli_file];
    cmd
  in
  let r = new_rule b lib.lib_loc cmi_file [Execute cmd] in

  cross_move r [ BF cmi_temp, BF cmi_file ];
  move_compilation_garbage r mut_dir mli_file.file_dir.dir_file kernel_name lib;
  List.iter (fun pd ->
    if pd.dep_link then
      let lib = pd.dep_project in
      add_rule_sources r lib.lib_bytecomp_deps
  ) pj.lib_requires;
  add_rule_source r mli_file;
  add_rule_sources r seq_order;
  add_rule_temporary r cmi_temp;

(* TODO: we should actually rename all modules to fit their capitalized name in the _obuild directory *)
  let lib_modules = match pack_for with
      [] -> lib.lib_modules
    | _ ->
      let pack_for = List.rev pack_for in
      try
	let (_, map) = StringsMap.find pack_for lib.lib_internal_modules in
	map
      with Not_found ->
	let map = ref StringMap.empty in
	lib.lib_internal_modules <- StringsMap.add pack_for (dst_dir, map) lib.lib_internal_modules;
	map
  in

  begin
    let (is_ml, modname, basename) = BuildOCamldep.modname_of_file options force mli_file.file_basename in
    try
      let (kind, basename) = StringMap.find modname !lib_modules in
      match kind with
	  MLI -> ()
	| MLandMLI -> ()
	| ML ->
	  lib_modules := StringMap.add modname (MLandMLI, basename) !lib_modules
    with Not_found ->
      if verbose 3 then
	Printf.eprintf "Adding module %s to %s\n" modname lib.lib_name;
      lib_modules := StringMap.add modname (MLI, basename) !lib_modules
  end;

  cmi_files := cmi_file :: !cmi_files


let get_packed_objects r src_dir pack_of cmx_ext =
  let packed_cmx_files = ref [] in
  let b = r.rule_context in
  List.iter (fun basename ->
    let name, extension = File.cut_last_extension basename in
    let obj_extension = match String.lowercase extension with
	"ml" | "mll" | "mly" -> cmx_ext
      | "mli" -> ".cmi"
      | _ -> Printf.ksprintf failwith "Bad extension [%s] for filename [%s]" extension basename
    in
    let filename = name ^ obj_extension in
    let object_file = add_file b src_dir filename in
    packed_cmx_files := object_file :: !packed_cmx_files;

    add_rule_source r object_file;
  ) pack_of;

  let packed_cmx_files = List.rev !packed_cmx_files in
  packed_cmx_files


let copy_mli_if_needed b mut_dir mll_file kernel_name =
  try
    let mli_file = File.add_basename mll_file.file_dir.dir_file (kernel_name ^ ".mli") in
    if File.X.exists mli_file  then begin
      let mli_content = File.X.read_to_string mli_file in
      let tmp_mli = add_file b mut_dir (kernel_name ^ ".mli") in
      let tmp_mli_file = tmp_mli.file_file in
      if File.X.exists tmp_mli_file then
        let old_mli_content = File.X.read_to_string tmp_mli_file in
        if mli_content <> old_mli_content then begin
	  if verbose 1 then
	    Printf.fprintf stderr "cp %s %s [outdated]\n%!"
	      (File.to_string mli_file) (File.to_string tmp_mli_file);
            File.X.write_of_string tmp_mli_file mli_content
	end else
	  ()
      else begin
	if verbose 1 then
	  Printf.fprintf stderr "cp %s %s [unexisting] \n%!"
	    (File.to_string mli_file) (File.to_string tmp_mli_file);
        File.X.write_of_string tmp_mli_file mli_content;
        if not (Sys.file_exists "_obuild/_mutable_tree/libs/ocamlpro/system/simpleConfig.mli") then
          Printf.eprintf "not yet created\n%!";
      end
    end (* else
      Printf.eprintf "MLI FILE %S does not exist\n%!"
        (File.to_string mli_file); *)

  with e ->
    Printf.eprintf "copy_mli_if_needed error %s\n%!" (Printexc.to_string e);
    exit 2

(* Shall we infer the presence of the mli file ? We should probably ask the user
   to tell the build system that the mli does not exist. *)

let add_ml_source b lib pj ml_file options =
  let basename = ml_file.file_basename in
  (*  Printf.eprintf "basename = [%s]\n" basename; *)
  let kernel_name = Filename.chop_extension basename in

  let orig_ml_file = ml_file in
  let pack_for = strings_option options packed_option in
  if lib.lib_installed then begin

    if pack_for = [] then begin

(*
      Printf.eprintf "add_ml_source: %s is already installed in %s\n%!"
        basename (File.to_string dst_dir.dir_file);
      Printf.eprintf "ml_file %s\n%!" (file_filename ml_file);
*)

      let dst_dir = ml_file.file_dir in

      let cmo_basename = kernel_name ^ ".cmo" in
      let cmo_file = add_dst_file b dst_dir cmo_basename in

      let cmx_basename = kernel_name ^ ".cmx" in
      let cmx_file = add_dst_file b dst_dir cmx_basename in

      let ext_obj  = string_option options BuildConfig.ocaml_config_ext_obj in
      let o_basename = kernel_name ^ ext_obj in
      let o_file = add_dst_file b dst_dir o_basename in

      cmo_files := cmo_file :: !cmo_files;
      cmx_files := cmx_file :: !cmx_files;
      cmxo_files := o_file :: !cmxo_files
    end

  end else
    let mut_dir = mut_dir lib ml_file in
    let ppv = BuildOCamlSyntaxes.get_pp lib options in
    let ml_file, force =
      match ppv.pp_option with
        [] -> ml_file, Force_not
      | pp ->
        (* TODO: we should create the new_ml_file in the same subdirectory
           as the source file, not at the toplevel !! *)

        copy_mli_if_needed b mut_dir ml_file kernel_name;
        let new_ml_file =
          add_file b mut_dir (ml_file.file_basename ^ "pp")
        in

        let cmd = new_command pp [ BF ml_file ]  in
        add_command_pipe cmd (File.to_string new_ml_file.file_file);

        let r = new_rule b lib.lib_loc new_ml_file [] in
        add_rule_command r (Execute cmd);
       BuildOCamlSyntaxes.add_pp_requires r ppv;
        add_rule_source r ml_file;

        new_ml_file, Force_IMPL
    in


    let dst_dir = lib.lib_dst_dir in
    let dst_dir = match pack_for with
        [] -> dst_dir
      | modnames ->
        let name = String.concat "/" modnames in (* TODO : should be Filename.concat *)
        let full_dirname = Filename.concat dst_dir.dir_fullname name in
        safe_mkdir full_dirname;
        add_directory b full_dirname
    in
    let pack_of = strings_option options pack_option in

  (*
    if pack_of <> [] then
    List.iter (fun pack -> Printf.eprintf "pack %s\n" pack) pack_of;
  *)


  let modname = String.copy kernel_name in
  modname.[0] <- Char.uppercase modname.[0];
  let seq_order =
    if pack_of <> [] then
      [] (* don't compute dependencies when we already know them *)
    else
      let mldep_file = match !cross_arg with
	  None -> add_dst_file b dst_dir (kernel_name ^ ".mldep")
	| Some _ ->
	  add_dst_file b dst_dir (kernel_name ^ ".mlmods")
      in
      src_files := IntMap.add ml_file.file_id ml_file !src_files;
      let mldep_file_ok = add_ml2mldep_rule lib dst_dir pack_for force ml_file mldep_file options in
      dep_files := IntMap.add mldep_file.file_id mldep_file !dep_files;
      [mldep_file_ok]
  in
  let cmi_name = kernel_name ^ ".cmi" in
  let needs_cmi =
    try
      let cmi_file = find_dst_file dst_dir cmi_name in
      Some cmi_file
    with NoSuchFileInDir _ ->
      if not (bool_option_true options no_mli_option) &&
        (* do that before pp_option change it ! *)
        Sys.file_exists (file_filename orig_ml_file ^ "i")
      then begin
	(* MLI file does exist !!! We should probably put a warning, as we
	   have no information on how to compile this file !!*)
	ignore (add_mli_source b lib pj (add_file b orig_ml_file.file_dir (kernel_name ^ ".mli")) options);
	let cmi_file = find_dst_file dst_dir cmi_name in
	Some cmi_file
      end else
	None
  in
  let seq_order = match needs_cmi with
      None -> seq_order
    | Some cmi_file -> cmi_file :: seq_order in
  let gen_cmi = match needs_cmi with
      None -> [add_dst_file b dst_dir cmi_name ]
    | Some _ -> []
  in

  let lib_modules = match pack_for with
      [] -> lib.lib_modules
    | _ ->
      let pack_for = List.rev pack_for in
      try
	let (_, map) = StringsMap.find pack_for lib.lib_internal_modules in
	map
      with Not_found ->
	let map = ref StringMap.empty in
	lib.lib_internal_modules <- StringsMap.add pack_for (dst_dir, map) lib.lib_internal_modules;
	map
  in

  begin
    let (is_ml, modname, basename) = BuildOCamldep.modname_of_file options force ml_file.file_basename in
    try
      let (kind, basename) =  StringMap.find modname !lib_modules in
      let error filename =
        Printf.eprintf
          "ERROR: The file(s) %s appears more than once in %s\n%!"
          filename
          pj.lib_filename in
      match kind with
	  ML       -> error (basename ^ ".ml")
	| MLandMLI -> error (basename ^ ".ml and " ^ basename ^ ".mli")
	| MLI ->
	  lib_modules := StringMap.add modname (MLandMLI, basename) !lib_modules
    with Not_found ->
      if verbose 3 then
	Printf.eprintf "Adding module %s to %s\n" modname lib.lib_name;
      lib_modules := StringMap.add modname (ML, basename) !lib_modules
  end;


  let cmo_basename = kernel_name ^ ".cmo" in
  let cmo_file = add_dst_file b dst_dir cmo_basename in

  let cmi_basename = kernel_name ^ ".cmi" in
  let cmi_file = add_dst_file b dst_dir cmi_basename in

  let cmx_basename = kernel_name ^ ".cmx" in
  let cmx_file = add_dst_file b dst_dir cmx_basename in

  let ext_obj  = string_option options BuildConfig.ocaml_config_ext_obj in
  let o_basename = kernel_name ^ ext_obj in
  let o_file = add_dst_file b dst_dir o_basename in

  let (before_cmd, temp_ml_file) =
    if bool_option_true options no_mli_option then
      let temp_ml_file = T (kernel_name ^ ".ml") in
      ([ Copy (BF ml_file, temp_ml_file)], temp_ml_file)
    else
      ([], BF ml_file)
  in

  begin
    let cmd = new_command (strings_option options ocamlc_cmd) (bytecompflags pj options) in
    let r = new_rule b lib.lib_loc cmo_file before_cmd in

(*    let temp_dir = BuildEngineRules.rule_temp_dir r in
    let cmo_temp = File.add_basename temp_dir cmo_basename in
    let cmi_temp = File.add_basename temp_dir cmi_basename in *)

    add_bin_annot_argument cmd options;


    if pack_of = [] then begin
      add_command_args cmd [S "-c"; S "-o"; T cmo_basename];
      add_command_pack_args cmd pack_for;
      add_command_strings cmd (command_includes lib pack_for);
(*      add_command_strings cmd (command_pp pj options); *)
      if force = Force_IMPL || bool_option_true options ml_file_option then
        add_command_string cmd "-impl";
      add_command_arg cmd temp_ml_file;

      add_rule_command r (Execute cmd);
      add_rule_source r ml_file;
    end else begin
      add_command_args cmd [S "-pack"; S "-o"; T cmo_basename];
      add_command_pack_args cmd pack_for;

      let src_dir = Filename.concat dst_dir.dir_fullname modname in
      (*      Printf.eprintf "Pack in %s [%s]\n" src_dir modname; *)
      let src_dir = add_directory b src_dir in
      let cmo_files = get_packed_objects r src_dir pack_of ".cmo" in
      let cmd = add_files_to_link_to_command cmd options cmo_files in
      add_rule_command r cmd
    end;

    cross_move r (let moves = [ T cmo_basename, BF cmo_file ] in
(*		  add_rule_temporary r cmo_temp; *)
		  match needs_cmi with
		      None ->
(*			add_rule_temporary r cmi_temp; *)
			(T cmi_basename, BF cmi_file) :: moves
		    | _ -> moves);

    move_compilation_garbage r mut_dir (BuildEngineRules.rule_temp_dir r) kernel_name lib;

    add_rule_sources r seq_order;
    List.iter (fun pd ->
      if pd.dep_link then
        let lib = pd.dep_project in
        add_rule_sources r lib.lib_bytecomp_deps
    ) pj.lib_requires;
    add_rule_targets r gen_cmi;
  end;

  begin
    let cmd = new_command (strings_option options ocamlopt_cmd) (asmcompflags pj options) in
    let r = new_rule b lib.lib_loc cmx_file before_cmd in
    add_bin_annot_argument cmd options;
(*
    let temp_dir = BuildEngineRules.rule_temp_dir r in
    let o_temp = File.add_basename temp_dir o_basename in
    let cmx_temp = File.add_basename temp_dir cmx_basename in
    let cmi_temp = File.add_basename temp_dir cmi_basename in
*)

    if pack_of = [] then begin
      add_command_args cmd [S "-c"; S "-o"; T cmx_basename];
      add_command_pack_args cmd pack_for;
      add_command_strings cmd (command_includes lib pack_for);
(*      add_command_strings cmd (command_pp pj options); *)
      if force = Force_IMPL ||  bool_option_true options ml_file_option then
        add_command_string cmd "-impl" ;
      add_command_arg cmd temp_ml_file;

      add_rule_command r (Execute cmd);
      add_rule_source r ml_file;
    end else begin
      add_command_args cmd [S "-pack"; S "-o"; T cmx_basename];
      add_command_pack_args cmd pack_for;

      let src_dir = add_directory b (Filename.concat dst_dir.dir_fullname modname) in
      let cmx_files = get_packed_objects r src_dir pack_of ".cmx" in
      let cmd = add_files_to_link_to_command cmd options cmx_files in
      add_rule_command r cmd
    end;

    cross_move r (let moves = [ T cmx_basename, BF cmx_file;
			        T o_basename, BF o_file;
			      ] in
(*		  add_rule_temporaries r [cmx_temp; o_temp]; *)
		  match needs_cmi with
		      None ->
(*		        add_rule_temporary r cmi_temp; *)
		        (T cmi_basename, BF cmi_file) :: moves
		    | _ -> moves);
    List.iter (fun pd ->
      if pd.dep_link then
        let lib = pd.dep_project in
        add_rule_sources r lib.lib_asmcomp_deps
    ) pj.lib_requires;
    add_rule_sources r seq_order;
    add_rule_targets r (o_file :: gen_cmi);

    begin match needs_cmi with
        None ->
	(* If both ocamlc and ocamlopt build the cmi file, they should not execute
	   concurrently. For that, we create an artificial ordering between them, by
	   requesting the cmo file before the cmx file, if both have to be generated. *)

          (* TODO: is this still useful ? Now that we build in a
             temporary directory, there is no need for that, no ? *)
	  add_rule_time_dependency r cmo_file
      | _ -> ()
    end;
    move_compilation_garbage r mut_dir (BuildEngineRules.rule_temp_dir r) kernel_name lib;
  end;
  if pack_for = [] then begin
    cmo_files := cmo_file :: !cmo_files;
    cmx_files := cmx_file :: !cmx_files;
    cmxo_files := o_file :: !cmxo_files
  end

let add_mll_source b lib pj mll_file options =
  (*  let src_dir = lib.lib_src_dir in *)
  (*
    let dst_dir = lib.lib_dst_dir in
  *)
  let basename = mll_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".mll" in

  if not lib.lib_installed then
(*    let tmp_dirname =
      Filename.concat
        (Filename.concat b.build_dir_filename "_temp_tree")
        (File.to_string mll_file.file_dir.dir_file) in
    if not (Sys.file_exists tmp_dirname) then safe_mkdir tmp_dirname;
    let tmp_dir = add_directory b tmp_dirname in *)
    let mut_dir = mut_dir lib mll_file in

    copy_mli_if_needed b mut_dir mll_file kernel_name;


    let ml_file = add_file b mut_dir (kernel_name ^ ".ml") in
    add_mll2ml_rule b lib pj mll_file ml_file options;
    add_ml_source b lib pj ml_file options
  else
    let ml_file = add_file b lib.lib_src_dir (kernel_name ^ ".ml") in
    add_ml_source b lib pj ml_file options

let add_mly_source b lib pj mly_file options =
  (*  let src_dir = lib.lib_src_dir in *)
  (*
    let dst_dir = lib.lib_dst_dir in
  *)

  let basename = mly_file.file_basename in
  let kernel_name = Filename.chop_suffix basename ".mly" in

  if not lib.lib_installed then
    let tmp_dir = mut_dir lib mly_file in

    let ml_file = add_file b tmp_dir (kernel_name ^ ".ml") in
    let mli_filename = kernel_name ^ ".mli" in
    let mli_file = add_file b tmp_dir mli_filename in
    add_mli_source b lib pj mli_file options;
    add_mly2ml_rule b lib pj mly_file ml_file mli_file options;
    add_ml_source b lib pj ml_file options
  else
    let ml_file = add_file b mly_file.file_dir (kernel_name ^ ".ml") in
    add_ml_source b lib pj ml_file options

let rec process_source b lib src_dir (basename, options) =
  let (kernel_name, last_extension) = File.cut_last_extension basename in
  let src_file = add_filename b src_dir basename in
  match last_extension with
      "c" ->
	add_c_source b lib lib src_file options
    | "objects" ->
      let obj_lib =
        try
          StringMap.find kernel_name !packages_by_name
        with Not_found ->
          Printf.eprintf "Package %s: Could not find %s.objects in:\n%!"
            lib.lib_name kernel_name;
          StringMap.iter (fun s _ -> Printf.eprintf "%s " s) !packages_by_name;
          Printf.eprintf "\n%!";
          exit 2
      in
      cmo_files := (List.rev obj_lib.lib_cmo_objects) @ !cmo_files;
      cmx_files := (List.rev obj_lib.lib_asm_cmx_objects) @ !cmx_files;
      cmxo_files := (List.rev obj_lib.lib_asm_cmxo_objects) @ !cmxo_files;
      ()

    | "files" ->
      let obj_lib =
        try
          StringMap.find kernel_name !packages_by_name
        with Not_found ->
          Printf.eprintf "Package %s: Could not find %s.objects\n%!"
            lib.lib_name kernel_name;
          exit 2
      in
      let src_dir = obj_lib.lib_src_dir in
      List.iter (process_source b lib src_dir) obj_lib.lib_sources

    | "ml" ->
      add_ml_source b lib lib src_file options
    | "mll" ->
      add_mll_source b lib lib src_file options
    | "mly" ->
      add_mly_source b lib lib src_file options
    | "mli" ->
      add_mli_source b lib lib src_file options
    (* other ones: .ml4, mli4, .ml5, .mli5, .mly4, .mly5, .mll4, .mll5 *)
    | _ ->
      if bool_option_true options ml_file_option then
	add_ml_source b lib lib src_file options
      else
        if bool_option_true options mli_file_option then
          add_mli_source b lib lib src_file options
        else begin

	  Printf.eprintf "Don't know what to do with [%s]\n%!" (String.escaped basename);
	  Printf.eprintf "\tfrom project %s in dir %s\n%!" lib.lib_name src_dir.dir_fullname;
	  exit 2;
        end

let process_source b lib src_dir (basename, options) =
  let src_dir =
    let package = string_option options package_option in
    if package = "" then src_dir else
      let obj_lib =
        try
          StringMap.find package !packages_by_name
        with Not_found ->
          Printf.eprintf "Package %s: Could not find package %s\n%!"
            lib.lib_name package;
          exit 2
      in
      let src_dir = obj_lib.lib_src_dir in
      src_dir
  in
  let basename =
    let subdir = strings_option options subdir_option in
    match subdir with
        [] -> basename
      | subdir ->
(* Since basename can be a relative filename, we use both
  File.t and strings. Clearly, it is not good, and we should
  convert basename to File.t earlier *)
        let subdir = File.add_basenames (File.of_string "") subdir in
        Filename.concat (File.to_string subdir) basename
  in
  process_source b lib src_dir (basename, options)

let reset_object_files () =
  src_files := IntMap.empty;
  dep_files := IntMap.empty;
  o_files := [];
  cmo_files := [];
  cmx_files := [];
  cmxo_files := [];
  cmi_files := [];
  ()

let revert_object_files () =
  cmo_files := List.rev !cmo_files;
  cmx_files := List.rev !cmx_files;
  cmxo_files := List.rev !cmxo_files;
  cmi_files := List.rev !cmi_files;
  o_files := List.rev !o_files;
  ()


let process_sources b lib =
  let src_dir = lib.lib_src_dir in
  let _dst_dir = lib.lib_dst_dir in
  reset_object_files ();
  List.iter (process_source b lib src_dir) lib.lib_sources;
  revert_object_files ();

  (* src_files have to be present, but modifications should not trigger new computations
  let src_files = IntMap.map (fun src_file ->
    let src_file_ok = add_virtual_file b dst_dir (src_file.file_basename ^ " ok") in

    let r_ok = new_rule b lib.lib_loc src_file_ok [] in
    r_ok.rule_forced <- true;  (* must be executed, even when no changes *)
    add_rule_source r_ok src_file;
    src_file_ok
  ) !src_files in

  lib.lib_dep_deps <- src_files; *)
(*
  IntMap.iter (fun _ dep_file ->
    List.iter (fun r ->
      IntMap.iter (fun _ file -> add_rule_source r file) src_files;
    )  dep_file.file_target_of
  ) !dep_files;
*)
  ()

let add_library b lib =
(*  let src_dir = lib.lib_src_dir in *)
  let dst_dir = lib.lib_dst_dir in
(*   let options = lib.lib_options in *)
  process_sources b lib;


  let cclib = string_option lib.lib_options cclib_option in
  let cclib =
    if !o_files <> [] then begin
      let ext_lib = string_option lib.lib_options BuildConfig.ocaml_config_ext_lib in
      let a_file = add_dst_file b dst_dir (Printf.sprintf "libml%s%s" lib.lib_name ext_lib) in
      add_os2a_rule b lib lib !o_files a_file;
      if bool_option_true lib.lib_options byte_option then
	lib.lib_byte_targets <- a_file :: lib.lib_byte_targets;
      if bool_option_true lib.lib_options asm_option then
	lib.lib_asm_targets <- a_file :: lib.lib_asm_targets;
      lib.lib_clink_deps <- a_file :: lib.lib_clink_deps;
      lib.lib_bytelink_deps <- lib.lib_bytelink_deps;
      lib.lib_asmlink_deps <- lib.lib_asmlink_deps;
      lib.lib_cmo_objects <- lib.lib_cmo_objects;
      lib.lib_asm_cmx_objects <- lib.lib_asm_cmx_objects;
      lib.lib_asm_cmxo_objects <- lib.lib_asm_cmxo_objects;
      Printf.sprintf "%s -lml%s -I %s" cclib lib.lib_name dst_dir.dir_fullname
    end
    else cclib
  in

  if bool_option_true lib.lib_options byte_option then begin
    let cma_file = add_dst_file b dst_dir (lib.lib_name ^ ".cma") in
    add_cmos2cma_rule b lib lib cclib !cmo_files cma_file;
    lib.lib_byte_targets <- cma_file :: !cmi_files @ lib.lib_byte_targets;
    lib.lib_bytelink_deps <- cma_file :: lib.lib_bytelink_deps;
    lib.lib_cmo_objects <- !cmo_files @ lib.lib_cmo_objects;
  end;

  if bool_option_true lib.lib_options asm_option then begin

    let (cmxa_file, a_file) = add_cmxs2cmxa_rule b lib lib cclib !cmi_files !cmx_files !cmxo_files in
    lib.lib_asm_targets <- cmxa_file :: a_file :: !cmi_files @ lib.lib_asm_targets;
    lib.lib_asmlink_deps <- cmxa_file :: a_file :: lib.lib_asmlink_deps;
    lib.lib_asm_cmx_objects <- !cmx_files @ lib.lib_asm_cmx_objects;
    lib.lib_asm_cmxo_objects <- !cmxo_files @ lib.lib_asm_cmxo_objects;
  end;
  ()


let add_objects b lib =
(*  let src_dir = lib.lib_src_dir in *)
(*  let dst_dir = lib.lib_dst_dir in *)
  process_sources b lib;


  if bool_option_true lib.lib_options byte_option then begin
    lib.lib_byte_targets <- !cmo_files @ !cmi_files @ lib.lib_byte_targets;
    lib.lib_bytelink_deps <- !cmo_files @ lib.lib_bytelink_deps;
    lib.lib_bytecomp_deps <- !cmo_files @ !cmi_files @ lib.lib_bytecomp_deps;
    lib.lib_cmo_objects <- !cmo_files @ lib.lib_cmo_objects;
  end;
  if bool_option_true lib.lib_options asm_option then begin
    lib.lib_asm_targets <- !cmx_files @ !cmi_files @ lib.lib_asm_targets;
    lib.lib_asmlink_deps <- !cmx_files @ !cmxo_files @ lib.lib_asmlink_deps;
    lib.lib_asmcomp_deps <- !cmx_files @ !cmi_files @ lib.lib_asmcomp_deps;
    lib.lib_asm_cmx_objects <- !cmx_files @ lib.lib_asm_cmx_objects;
    lib.lib_asm_cmxo_objects <- !cmxo_files @ lib.lib_asm_cmxo_objects;
  end;
  ()


let add_program b lib =
(*  let src_dir = lib.lib_src_dir in *)
  let dst_dir = lib.lib_dst_dir in
  process_sources b lib;

  begin (* Fast check of libraries modules *)
    let map = ref StringMap.empty in
    List.iter (fun dep ->
      let lib1 = dep.dep_project in
      match lib1.lib_type with
          ProgramPackage
(*        | ProjectToplevel *)
        | ObjectsPackage
          -> ()
        | LibraryPackage ->
          let modules = lib1.lib_modules in
          let modules = !modules in
          StringMap.iter (fun modname _ ->
            try
              let lib2 = StringMap.find modname !map in
              Printf.eprintf
                "Warning: program %s, requirements %s and %s both\n" lib.lib_name lib2.lib_name lib1.lib_name;
              Printf.eprintf "\tdefine a module name %s.\n" modname;
              ()
            with Not_found ->
              map := StringMap.add modname lib1 !map
          ) modules
    ) lib.lib_requires
  end;

  let cclib = string_option lib.lib_options cclib_option in
  begin
    let linkflags = bytelinkflags lib in
    let linkflags =
      if !cmo_files <> [] then linkflags else S "-linkall" :: linkflags in
    let byte_file = add_dst_file b dst_dir (lib.lib_name ^ byte_exe) in
    add_cmos2byte_rule b lib linkflags cclib !cmo_files !o_files byte_file;
    if bool_option_true lib.lib_options byte_option then begin
      lib.lib_byte_targets <- byte_file :: lib.lib_byte_targets;
      lib.lib_bytelink_deps <- byte_file :: lib.lib_bytelink_deps;
      lib.lib_bytecomp_deps <- byte_file :: lib.lib_bytecomp_deps;
      lib.lib_cmo_objects <- !cmo_files @ lib.lib_cmo_objects;
    end
  end;

  if !cmx_files <> [] then begin
    let linkflags = asmlinkflags lib in
    let linkflags =
      if !cmx_files <> [] then linkflags else S "-linkall" :: linkflags in
    let asm_file = add_dst_file b dst_dir (lib.lib_name ^ asm_exe) in
    add_cmxs2asm_rule b lib linkflags cclib !cmx_files !cmxo_files !o_files asm_file;
    if bool_option_true lib.lib_options asm_option then begin
      lib.lib_asm_targets <- asm_file :: lib.lib_asm_targets;
      lib.lib_asmlink_deps <- asm_file :: lib.lib_asmlink_deps;
      lib.lib_asmcomp_deps <- asm_file :: lib.lib_asmcomp_deps;
      lib.lib_asm_cmx_objects <- !cmx_files @ lib.lib_asm_cmx_objects;
      lib.lib_asm_cmxo_objects <- !cmxo_files @ lib.lib_asm_cmxo_objects;
    end
  end;
  ()

let add_package b pk =
  if verbose 4 then Printf.eprintf "Adding %s\n" pk.package_name;

  let package_dirname =
    try
	  match StringMap.find dirname_option.option_name pk.package_options with
	      OptionList list ->
                BuildSubst.subst (String.concat Filename.dir_sep list)
	    | _ -> raise Not_found
    with Not_found ->
      pk.package_dirname
  in


  let src_dir = add_directory b (absolute_filename package_dirname) in
  if verbose 4 then Printf.eprintf "\tfrom %s\n" src_dir.dir_fullname;

  let dst_dir =
    if bool_option_true pk.package_options generated_option then src_dir else
      match !cross_arg with
	  None -> src_dir
        | Some arch ->
	  let dirname =
	    Filename.concat b.build_dir_filename pk.package_name
        (*	  Filename.concat src_dir.dir_fullname build_dir_basename *)
	  in
	  if not (Sys.file_exists dirname) then safe_mkdir dirname;
	  add_directory b dirname
  in
  if verbose 4 then Printf.eprintf "\tto %s\n" dst_dir.dir_fullname;

  BuildSubst.putenv (Printf.sprintf "%s_SRC_DIR" pk.package_name) src_dir.dir_fullname;
  BuildSubst.putenv (Printf.sprintf "%s_DST_DIR" pk.package_name) dst_dir.dir_fullname;

  let mut_dir =
    let src_dirname = File.to_string src_dir.dir_file in
    let mut_dirname =
      Filename.concat
        (Filename.concat b.build_dir_filename "_mutable_tree") src_dirname
    in
    if not (Sys.file_exists mut_dirname) then safe_mkdir mut_dirname;
    add_directory b mut_dirname
  in

  let lib = BuildGlobals.new_library b pk package_dirname src_dir dst_dir mut_dir in

  (match !cross_arg with
      None -> ()
      | Some _ ->
	safe_mkdir dst_dir.dir_fullname);
  match lib.lib_type with
      LibraryPackage -> add_library b  lib
      | ProgramPackage -> add_program b  lib
      | ObjectsPackage -> add_objects b  lib
(*      | _ -> Printf.eprintf "\tWarning: Don't know what to do with 'add_project %s'\n" lib.lib_name *)

let create pj b =
  Array.iter (add_package b) pj.project_sorted
