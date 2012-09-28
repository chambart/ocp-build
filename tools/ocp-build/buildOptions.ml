(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

(* Options are stored in:
   - in the project directory, in a file called 'ocp-build.root' that is
   also used as to discover the root of the project.
   - in the user HOME directory, in a file called '~/.ocp/ocp-build.conf'

   As a consequence, we have two sets of options:
   - project options, that are usually optional options
   - global options, that have the default value

   Finally, we must handle also arguments, that can be used to change the
   final value of the option.

*)

(*
TODO: we might need a mechanism at some point to declare options in
other files. For now, it is better to declare all options here.
*)

open SimpleConfig

  type project_options = {
    mutable option_ncores : int;
    mutable option_autoscan : bool;
    mutable option_verbosity : int;
    mutable option_usestdlib : bool;
    mutable option_digest : bool;
    mutable option_bytecode : bool;
    mutable option_native : bool;

    mutable option_ocamlbin : string;
    mutable option_ocamllib : string;

    mutable option_ocamlc : string list;
    mutable option_ocamlopt : string list;
    mutable option_ocamldep : string list;
    mutable option_ocamllex : string list;
    mutable option_ocamlyacc : string list;
  }

let must_save_local = ref false
let must_save_global = ref false
let arg_list = ref [
  "-save-global", Arg.Set must_save_global,
  " : save arguments in global config";
  "-save-local", Arg.Set must_save_local,
  " : save arguments in local (project) config";
]

let ncores_descr =
  [ "njobs" ],
  ["Number of jobs to use in parallel on this computer"],
  int_option, 1

let verbosity_descr =
  [ "verbosity" ],
  ["verbosity for debugging"],  int_option, 1

let autoscan_descr =
  [ "autoscan" ],
  ["If set, scan for .ocp files";
   "in project sub-directories"],
  bool_option, false

let usestdlib_descr =
  [ "usestdlib" ],
  ["If set, also scan for packages the";
   "OCaml install directory"],
    bool_option, true

let digest_descr =
  [ "digest" ],
  [ "If set, use content digest change instead of";
    "modification to trigger recompilation"],
  bool_option, false



let bytecode_descr =
  [ "bytecode" ], ["If set, compile in bytecode" ],
  bool_option, true
let native_descr =
  [ "native" ], ["If set, compile in native code" ],
  bool_option, true


let ocamlbin_descr =
  [ "ocamlbin" ],  ["directory containing ocaml compilers"],
  string_option, ""

let ocamllib_descr =
  [ "ocamllib" ],  ["directory containing ocaml libraries"],
  string_option, ""

let ocamlc_descr =
  [ "ocamlc" ],  ["executable names to use in preference order"],
  list_option string_option, [ "ocamlc.opt"; "ocamlc" ]

let ocamlopt_descr =
  [ "ocamlopt" ],  ["executable names to use in preference order"],
  list_option string_option, [ "ocamlopt.opt"; "ocamlopt" ]

let ocamldep_descr =
  [ "ocamldep" ],  ["executable names to use in preference order"],
  list_option string_option, [ "ocamldep.opt"; "ocamldep" ]

let ocamllex_descr =
  [ "ocamllex" ],  ["executable names to use in preference order"],
  list_option string_option, [ "ocamllex.opt"; "ocamllex" ]

let ocamlyacc_descr =
  [ "ocamlyacc" ],  ["executable names to use in preference order"],
  list_option string_option, [ "ocamlyacc" ]

module GlobalOptions = struct

  let homedir = try Sys.getenv "HOME" with Not_found -> "."

  let global_config_dir =
    try
      Sys.getenv "OCP_HOME"
    with Not_found -> Filename.concat homedir ".ocp"

  let global_config_dir = File.of_string global_config_dir

  let config_file_basename =  "ocp-build.conf"

  let global_config_file =
    File.add_basename global_config_dir config_file_basename

  let config_file = create_config_file global_config_file

  let global_option (option_names, option_help, option_type, option_default) =
    create_option config_file option_names option_help option_type
      option_default

  let global_int_option option_descr =
    let option = global_option option_descr in
    let (option_names, option_help, _, _ ) = option_descr in
    let arg_set = ref false in
    arg_list :=
      (Printf.sprintf "-%s" (String.concat ":" option_names),
       Arg.Int (fun n ->
         option =:= n;
         arg_set := true),
       Printf.sprintf " NUM : %s" (String.concat "\n     " option_help)) ::
      !arg_list;
    (arg_set, option, option_descr)

  let global_string_option option_descr =
    let option = global_option option_descr in
    let (option_names, option_help, _, _ ) = option_descr in
    let arg_set = ref false in
    arg_list :=
      (Printf.sprintf "-%s" (String.concat ":" option_names),
       Arg.String (fun n ->
         option =:= n;
         arg_set := true),
       Printf.sprintf " STRING : %s" (String.concat "\n     " option_help)) ::
      !arg_list;
    (arg_set, option, option_descr)

  let global_stringlist_option option_descr =
    let option = global_option option_descr in
    let (option_names, option_help, _, _ ) = option_descr in
    let arg_set = ref false in
    arg_list :=
      (Printf.sprintf "-%s" (String.concat ":" option_names),
       Arg.String (fun n ->
         option =:= OcpString.split_simplify n ':';
         arg_set := true),
       Printf.sprintf " LIST_OF_STRINGS : %s\n     (LIST_OF_STRINGS is a set of strings separated by :)" (String.concat "\n     " option_help)) ::
      !arg_list;
    (arg_set, option, option_descr)

  let global_bool_option option_descr =
    let option = global_option option_descr in
    let (option_names, option_help, _, _ ) = option_descr in
    let arg_set = ref false in
    arg_list :=
      (Printf.sprintf "-no-%s" (String.concat ":" option_names),
       Arg.Unit (fun n ->
         option =:= false;
         arg_set := true),
       Printf.sprintf " : clear option, %s"
         (String.concat "\n     " option_help)) ::
      (Printf.sprintf "-%s" (String.concat ":" option_names),
       Arg.Unit (fun n ->
         option =:= true;
         arg_set := true),
       Printf.sprintf " : set option, %s"
         (String.concat "\n     " option_help)) ::
      !arg_list;
    (arg_set, option, option_descr)

  let ncores_setter = global_int_option ncores_descr
  let verbosity_setter = global_int_option verbosity_descr
  let autoscan_setter = global_bool_option autoscan_descr
  let usestdlib_setter = global_bool_option usestdlib_descr
  let digest_setter = global_bool_option digest_descr
  let ocamlbin_setter = global_string_option ocamlbin_descr
  let ocamllib_setter = global_string_option ocamllib_descr
  let bytecode_setter = global_bool_option bytecode_descr
  let native_setter = global_bool_option native_descr

  let ocamlc_setter = global_stringlist_option ocamlc_descr
  let ocamlopt_setter = global_stringlist_option ocamlopt_descr
  let ocamldep_setter = global_stringlist_option ocamldep_descr
  let ocamllex_setter = global_stringlist_option ocamllex_descr
  let ocamlyacc_setter = global_stringlist_option ocamlyacc_descr

  let load_or_create () =
    if File.X.exists global_config_file then begin
      try
        SimpleConfig.load config_file
      with e ->
        Printf.fprintf stderr "Error while loading %s\n"
          (File.to_string global_config_file);
        Printf.fprintf stderr "\tException %s\n%!"
          (Printexc.to_string e);
        exit 2
    end else begin
      Printf.eprintf "Warning: file %S does not exist. Creating with default values.\n%!" (File.to_string global_config_file);
      File.Dir.make_all global_config_dir;
      SimpleConfig.save_with_help config_file
    end

  let save () =
      must_save_global := false;
    SimpleConfig.save_with_help config_file
  let maybe_save () =
    if !must_save_global then save ()

  let (_, verbosity_option, _) = verbosity_setter

end


module LocalOptions = struct

  open BuildTypes

  let load config_filename =

    let config_file = SimpleConfig.create_config_file config_filename in

    begin
      try
        SimpleConfig.load config_file
      with e ->
        Printf.fprintf stderr "Error while loading %s\n"
          (File.to_string (SimpleConfig.config_file config_file));
        Printf.fprintf stderr "\tException %s\n%!"
          (Printexc.to_string e);
        exit 2
    end;

    let local_option
        (option_set, global_option,
         (option_names, option_help, option_type, option_default)
        ) =
      let local_option = create_option config_file option_names
        option_help (option_option option_type) None in
      if !option_set then begin
        if !must_save_local then
          local_option =:= Some !!global_option;
        !!global_option
      end else
        match !!local_option with
            None -> !!global_option
          | Some v -> v
    in

    let option_ncores = local_option GlobalOptions.ncores_setter in
    let option_autoscan = local_option GlobalOptions.autoscan_setter in
    let option_verbosity = local_option GlobalOptions.verbosity_setter in
    let option_usestdlib = local_option GlobalOptions.usestdlib_setter in
    let option_digest = local_option GlobalOptions.digest_setter in
    let option_ocamlbin = local_option GlobalOptions.ocamlbin_setter in
    let option_ocamllib = local_option GlobalOptions.ocamllib_setter in
    let option_bytecode = local_option GlobalOptions.bytecode_setter in
    let option_native = local_option GlobalOptions.native_setter in

    let option_ocamlc = local_option GlobalOptions.ocamlc_setter in
    let option_ocamlopt = local_option GlobalOptions.ocamlopt_setter in
    let option_ocamldep = local_option GlobalOptions.ocamldep_setter in
    let option_ocamllex = local_option GlobalOptions.ocamllex_setter in
    let option_ocamlyacc = local_option GlobalOptions.ocamlyacc_setter in
    config_file,
    {
      option_ncores;
      option_autoscan;
      option_verbosity;
      option_usestdlib;
      option_digest;
      option_ocamlbin;
      option_ocamllib;
      option_bytecode;
      option_native;
      option_ocamlc;
      option_ocamlopt;
      option_ocamldep;
      option_ocamllex;
      option_ocamlyacc;
    }

  let save root_config =
    must_save_local := false;
    SimpleConfig.save_with_help root_config

  let maybe_save root_config =
    if !must_save_local then save root_config

end

let arg_list () = List.rev !arg_list
let rec shortcut_arg new_name old_name list =
  match list with
      [] -> assert false
    | (name, f, help) :: list ->
      if name = old_name then
        (new_name, f,
         Printf.sprintf "%s\n    (shortcut for %s)" help old_name)
      else shortcut_arg new_name old_name list

let load_local = LocalOptions.load
let save_local = LocalOptions.save
let maybe_save_local = LocalOptions.maybe_save

let load_global = GlobalOptions.load_or_create
let save_global = GlobalOptions.save
let maybe_save_global = GlobalOptions.maybe_save

