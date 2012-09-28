(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

open OcpLang
open SimpleConfig

open BuildConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions

let _ = DebugVerbosity.add_submodules "B" [ "BuildMain" ]

let version = "2012-07-05 09:11 Fabrice"

let print_version () =
  Printf.fprintf stderr "%s\n%!" version;
  exit 0

let set_verbosity v =
  DebugVerbosity.increase_verbosity "B" v

let t0 = Unix.gettimeofday ()

let time s f x =
  if !time_arg then
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let t1 = Unix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x

let time2 s f x1 x2 =
  if !time_arg then
    let t0 = Unix.gettimeofday () in
    let y = f x1 x2 in
    let t1 = Unix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x1 x2

let save_project = ref false
let save_arguments_arg = ref false
let delete_orphans_arg = ref KeepOrphans
let list_projects_arg = ref false
let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false
let build_dir_basename_arg = ref "_obuild"
let no_stdlib_arg = ref None
let other_dirs_arg = ref []
type arch_arg = ArchAuto | ArchNone | Arch of string
let arch_arg = ref ArchNone

let init_arg = ref false

let arg_list = BuildOptions.arg_list ()

let arg_list = [
  "-init", Arg.Unit (fun () ->
    init_arg := true;
    save_arguments_arg := true;
    save_project := true), " : create the ocp-build.root file";

(*
  "-byte", Arg.Set byte_arg, " : build only bytecode version";
  "-asm", Arg.Set asm_arg, " : build only native code version";
*)
  "-clean", Arg.Set clean_arg, " : clean all compiled files and exit";
  "-distclean", Arg.Set distclean_arg, " : clean all generated files and exit";

  "-obuild", Arg.String (fun s -> build_dir_basename_arg := s), " <dir> : change _obuild directory";
  "-arch", Arg.String (fun s -> arch_arg := Arch s), " <arch> : set arch";
  "-auto-arch", Arg.Unit (fun () -> arch_arg := ArchAuto), ": set arch automatically";
  "-I", Arg.String (fun s -> other_dirs_arg := s :: !other_dirs_arg),
  " DIRECTORY : add DIRECTORY to project";
  "-no-stdlib", Arg.Unit (fun _ -> no_stdlib_arg := Some true), " : do not scan standard directory";
  "-sanitize", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFiles), " : remove orphan objects from _obuild";
  "-sanitize-dirs", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFilesAndDirectories), " : remove orphan directories from _obuild";

  "-list-ocp-files", Arg.Set list_ocp_files, " : list all .ocp files found";
  "-cross", Arg.String (fun arch -> cross_arg := Some arch), " : use a cross-compilation directory";
  "-k", Arg.Clear stop_on_error_arg, " : continue after errors";
  "-fake", Arg.Set fake_arg, " : fake actions, do not execute them";

  "-list-projects", Arg.Set list_projects_arg, " : list projects";
  "-list-targets", Arg.Unit (fun _ ->
    list_byte_targets_arg := true;
    list_asm_targets_arg := true), " : list all targets";
  "-list-byte-targets", Arg.Set list_byte_targets_arg, " : list bytecode targets";
  "-list-asm-targets", Arg.Set list_asm_targets_arg, " : list native targets";

  "-time", Arg.Set time_arg, " : print timing";

  "-library-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProjectLibrary
      (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

  "-program-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProjectProgram
      (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

]
  @ [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      " : useless argument";
      "";
      "Options under this line will be saved if you either use";
      "\"-save-global\" or \"-save-local\":";
      ""
    ];
    BuildOptions.shortcut_arg "-asm" "-native" arg_list;
    BuildOptions.shortcut_arg "-byte" "-bytecode" arg_list;
    BuildOptions.shortcut_arg "-scan" "-autoscan" arg_list;
    BuildOptions.shortcut_arg "-v" "-verbosity" arg_list;
  ]
  @ arg_list

let arg_usage =
  String.concat "\n"
    ["ocp-build [options] targets : build OCaml targets";
     "";
     "A project is composed of packages, described by .ocp files";
     "";
     "The following options are available:"
    ]


let build root_file =
  let root_dir = File.dirname root_file in

  let build_dir_basename = !build_dir_basename_arg in


  let build_dir_filename = (* absolute_filename *) build_dir_basename in

  targets_arg := List.rev !targets_arg;

  if verbose 2 then Printf.eprintf "Arguments parsed\n%!";


  let root_config, pjo = BuildOptions.load_local root_file in

  if pjo.option_ocamllib <> "" then begin
(* do this before check_config *)
    Unix.putenv "OCAMLLIB" pjo.option_ocamllib
  end;

(*  time "Config time: %.2fs\n%!" BuildConfig.load_config local_config_file; *)
  let cfg =
    time "Check config: %.2fs\n%!" BuildConfig.check_config pjo in

  let host = Printf.sprintf "%s-%s-%s"
    cfg.ocaml_system cfg.ocaml_architecture cfg.ocaml_version in

  let build_dir_filename =
    match !arch_arg with
      ArchAuto -> Filename.concat build_dir_filename host
    | Arch host -> Filename.concat build_dir_filename host
    | ArchNone -> build_dir_filename
  in
(*  Printf.fprintf stderr "build_dir_filename = %s\n%!" build_dir_filename; *)
  let b = BuildEngineContext.create (File.to_string root_dir) build_dir_filename in

  b.stop_on_error_arg <- !stop_on_error_arg;

  if !conf_arg || !distrib_arg || !autogen_arg then exit 0;

  let _project_ocpbuild_version = create_option root_config
    [ "ocpbuild_version" ]
    ["The version of ocp-build used to save this file"]
    SimpleConfig.string_option version
  in

  let _project_other_dirs_option = create_option root_config
    [ "other_dirs" ]
    [ "Other directories to be scanned" ]
    (SimpleConfig.list_option SimpleConfig.string_option) []
  in

  let force_scan = pjo.option_autoscan in
  let use_digests = pjo.option_digest in

  set_verbosity pjo.option_verbosity;

  let ncores = pjo.option_ncores in
  let _usestdlib = pjo.option_usestdlib in

  let root_files = create_option root_config [ "files" ]
   [ "List of configuration files for this project"]
    (list_option file_option) []
  in

  if use_digests then BuildEngineMtime.use_digests true;
  if force_scan then begin
    save_project := true;
    let files = BuildOCP.scan_root root_dir in
    root_files =:= files (* !!project_other_dirs *)
  end;

  if !save_project then begin
    Printf.fprintf stderr "Updating ocp-build.root\n%!";
    BuildOptions.save_local root_config
  end;
  BuildOptions.maybe_save_local root_config;

    b.cross_arg <- !cross_arg;

  (* Don't modify default values from now on, since they have been included
   in the default configuration ! *)
  let pj, nerrors =
    time "Loading time: %.2fs\n%!" BuildOCP.load_project !!root_files
  in
  if nerrors > 0 then exit 2;
 BuildOCP.save_project_state pj
   (File.add_basenames root_dir ["_obuild"; "ocp.ocpx"]);

  let print_package pj = Printf.eprintf "\t%s in %s (%s)\n" pj.package_name pj.package_dirname
	(match pj.package_type with
	    ProjectProgram -> "program"
	  | ProjectLibrary -> "library"
(*	  | ProjectToplevel -> "toplevel" *)
	  | ProjectObjects -> "objects")
      in

    if verbose 3 || !list_projects_arg then begin

      Printf.eprintf "Disabled packages:\n";
      Array.iter print_package pj.project_disabled;

      Printf.eprintf "Validated packages:\n";
      Array.iter print_package pj.project_sorted;
    end;

  begin
    let incomplete_projects = Hashtbl.create  13 in
    if pj.project_incomplete <> [||] then begin
      Printf.eprintf "Warning: %d incomplete projects:\n" (Array.length pj.project_incomplete);
      Array.iter (fun pk ->
        Hashtbl.add incomplete_projects pk.package_name pk;
        print_package pk) pj.project_incomplete;
    end;

  List.iter (fun (name, list) ->
    Printf.eprintf "   %s \"%s\" missed by %d projects\n"
      (if Hashtbl.mem incomplete_projects name then "INCOMPLETE" else "ABSENT")
      name
      (List.length list);
    List.iter print_package list;
  ) pj.project_missing;
  end;

  time "Context time: %.2fs\n%!" BuildOCamlRules.create pj b;

  if !distclean_arg then begin
    BuildActions.do_distclean ();
    exit 0
  end;

  if !clean_arg then begin
    BuildActions.do_clean b;
    exit 0;
  end;

  if !list_byte_targets_arg then begin
    Printf.eprintf "Bytecode targets:\n";
    StringMap.iter (fun _ lib ->
      if lib.lib_byte_targets <> [] then begin
	List.iter (fun target -> Printf.eprintf "\t%s\t->\t%s\n" lib.lib_name target.file_basename) lib.lib_byte_targets;
      end) !packages_by_name;
	Printf.eprintf "%!"
  end;

  let targets = ref [] in
  let add_project_targets lib =
    if pjo.option_bytecode then
      targets := lib.lib_byte_targets @ !targets;
    if pjo.option_native then
      targets := lib.lib_asm_targets @ !targets;
  in
  begin
    match !targets_arg with
	[] ->
	  StringMap.iter (fun _ pj -> add_project_targets pj) !packages_by_name
      | list ->
	List.iter (fun name ->
	  try
	    let pj = StringMap.find name !packages_by_name in
	    add_project_targets pj
	  with Not_found ->
	    Printf.eprintf "Error: Could not find target project %s\n%!" name;
	    exit 2
	) list
  end;
  if !targets <> [] then begin
    begin
      try
	time2 "Build init time: %.2f\n%!" BuildEngine.init b !targets
      with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
	let (rule_filename, rule_loc, rule_name) = r.rule_loc in
	BuildMisc.print_loc rule_filename rule_loc;
	Printf.eprintf "Error: in project \"%s\", the source filename\n"
	  rule_name;
	Printf.eprintf "\t\"%s\" does not exist\n" filename;
	BuildRules.print_rule r;
	exit 2
    end;
    let orphans = time2 "Sanitizing time: %.2fs\n%!" BuildEngine.sanitize b !delete_orphans_arg in
    if orphans > 0 then begin
      Printf.fprintf stderr "Error: found %d orphan files in _obuild. You must remove them.\n" orphans;
      Printf.fprintf stderr "\n";
      Printf.fprintf stderr "   You can add the -sanitize argument to automatically remove\n";
      Printf.fprintf stderr "   orphan files\n";
      Printf.fprintf stderr "\n";
      exit 2;
    end else
      if orphans < 0 then
        Printf.fprintf stderr "Warning: deleted %d orphan files in _obuild\n" (-orphans);
    Printf.fprintf stderr "Building using %d cores\n%!" ncores;
    time2  "Building time: %.2fs\n%!" BuildEngine.parallel_loop b ncores;
    let errors = BuildEngine.fatal_errors() @ BuildEngine.errors() in
    if verbose 1 || errors <> [] then begin
      Printf.eprintf "%s. %d commands executed, %d files generated.\n%!"
	(if errors = [] then "No error" else
	    Printf.sprintf "%d errors" (List.length errors))
	!BuildEngine.stats_command_executed
	!BuildEngine.stats_files_generated;
    end;
    if errors <> [] then begin
      List.iter (fun lines ->
	Printf.eprintf "Error:\n";
	List.iter (fun line ->
	  Printf.eprintf "%s\n" line
	) lines

      ) errors;
      exit 2
    end;
  end;
  Printf.eprintf "%!";
  let t1 = Unix.gettimeofday () in
  if !time_arg then
    Printf.printf "Total time: %.2fs\n%!" (t1 -. t0)

let _ =
  Printexc.record_backtrace true;

  BuildOptions.load_global ();
  set_verbosity !!BuildOptions.GlobalOptions.verbosity_option;
  Arg.parse arg_list (fun s -> targets_arg := s :: !targets_arg) arg_usage;
  BuildOptions.maybe_save_global ();

  if !init_arg && not (Sys.file_exists "ocp-build.root") then begin
    let oc = open_out "ocp-build.root" in
    close_out oc
  end;

  let project_basenames = [  "ocp-build.root" ] in
  let project_dir =
    try
      BuildOCP.find_root (File.X.getcwd ()) project_basenames
    with Not_found ->
      Printf.fprintf stderr "Fatal error: no ocp-build.root file found.\n%!";
      Printf.fprintf stderr "\tYou can use the -init option at the root of the project\n";
      Printf.fprintf stderr "\tto create the initial file.\n%!";
      exit 2
  in

  let project_filename = File.add_basenames project_dir project_basenames in
  Unix.chdir (File.to_string project_dir);
  Printf.fprintf stdout "ocp-build: Entering directory `%s'\n%!"  (File.to_string project_dir);
  try
    build project_filename;
    Printf.fprintf stdout "ocp-build: Leaving directory `%s'\n%!" (File.to_string project_dir)

  with e ->
    let backtrace = Printexc.get_backtrace () in
    Printf.fprintf stdout "ocp-build: Leaving directory `%s'\n%!" (File.to_string project_dir);
    Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!" (Printexc.to_string e) backtrace;
    raise e


