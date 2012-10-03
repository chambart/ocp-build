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

(* Génération et lecture du fichier de configuration *)

open OcpLang

open BuildOptions
open Genlex
open BuildOCPTypes
open BuildGlobals
open BuildTypes
open BuildMisc
open BuildOCPVariable



let sep_PATH =
  match Sys.os_type with
      "Win32" -> ';'
    | _ -> ':'

let get_PATH () =
  try
    let path = Sys.getenv "PATH" in
    String.split path sep_PATH
  with Not_found ->
    failwith "Env variable PATH not defined"

let b = Buffer.create 10000
let get_stdout_lines cmd args =
  let temp_file = Filename.temp_file "ocp-build-" ".out" in
  let pid = BuildMisc.create_process (cmd@args) (Some temp_file) None in
  let status = BuildMisc.wait_command pid in
  let lines = ref [] in
  begin try
	  let ic = open_in temp_file in
	  begin

	    try
	      while true do
		lines := (input_line ic) :: !lines
	      done
	    with _ -> ()
	  end;
	  close_in ic;
	  Sys.remove temp_file;
    with _ -> ()
  end;
  (status, List.rev !lines)

let check_command_exists filename =
  let st = Unix.stat filename in
  match st.Unix.st_kind with
      Unix.S_REG ->
	begin
	  try
	    Unix.access filename [Unix.X_OK];
	    filename
	  with e ->
	    Printf.eprintf "Warning: %s in PATH has not executable permission\n%!"
	      filename;
	    raise e
	end
    | _ ->
      Printf.eprintf "Warning: %s in PATH is not a regular command\n%!" filename;
      raise Not_found

let rec find_in_PATH command path =
  match path with
      [] -> raise Not_found
    | dirname :: path ->
      let filename = Filename.concat dirname command in
      try
	check_command_exists filename
      with _ ->
	find_in_PATH command path


let verbose = DebugVerbosity.verbose [ "B" ] "BuildConfig"

let lexer = Genlex.make_lexer  [ "=" ; "["; "]" ]

let ocamldep_cmd = new_initial_strings_option "ocamldep" [ "ocamldep.opt" ]
let ocamlc_cmd = new_initial_strings_option "ocamlc" [ "ocamlc.opt" ]
let ocamlcc_cmd = new_initial_strings_option "ocamlcc" [ "ocamlc.opt" ]
let ocamlopt_cmd = new_initial_strings_option "ocamlopt" [ "ocamlopt.opt" ]
let ocamllex_cmd = new_initial_strings_option "ocamllex" [ "ocamllex.opt" ]
let ocamlyacc_cmd = new_initial_strings_option "ocamlyacc" [ "ocamlyacc" ]


let ocaml_config_version = new_initial_strings_option "ocaml_version" []
let ocaml_config_system = new_initial_strings_option "system" []
let ocaml_config_architecture = new_initial_strings_option "architecture" []
let ocaml_config_ext_obj = new_initial_strings_option "ext_obj" [ ".o" ]
let ocaml_config_ext_lib = new_initial_strings_option "ext_lib" [ ".a" ]
let ocaml_config_ext_dll = new_initial_strings_option "ext_dll" [ ".so" ]
let ocaml_config_os_type = new_initial_strings_option "os_type" [ ]


module TYPES = struct

  type ocaml_config = {
    ocaml_version : string;
    ocaml_ocamllib : string;
    ocaml_system : string;
    ocaml_architecture : string;
    ocaml_ext_obj : string;
    ocaml_ext_lib : string;
    ocaml_ext_dll : string;
    ocaml_os_type : string;
  }

end

open TYPES

let get_config cmd =
  let _, c = get_stdout_lines [ cmd ] [ "-config" ] in
  let ocaml_version = ref "NOVERSION" in
  let ocaml_system = ref "NOSYSTEM" in
  let ocaml_architecture = ref "NOARCH" in
  let ocaml_ext_obj = ref ".o" in
  let ocaml_ext_lib = ref ".a" in
  let ocaml_ext_dll = ref ".so" in
  let ocaml_os_type = ref "NOOSTYPE" in
  let ocaml_ocamllib = ref "" in
  List.iter (fun line ->
    let (name, v) = OcpString.cut_at line ':' in
    let v = String.sub v 1 (String.length v - 1) in
    match name with
      | "version" -> ocaml_version := v
      | "system" -> ocaml_system := v
      | "architecture" -> ocaml_architecture := v
      | "ext_obj" -> ocaml_ext_obj := v
      | "ext_lib" -> ocaml_ext_lib := v
      | "ext_dll" -> ocaml_ext_dll := v
      | "os_type" -> ocaml_os_type := v
      | "standard_library" -> ocaml_ocamllib := v
      | _ -> ()
  ) c;

  { ocaml_version = !ocaml_version;
    ocaml_architecture = !ocaml_architecture;
    ocaml_system = !ocaml_system;
    ocaml_ext_obj = !ocaml_ext_obj;
    ocaml_ext_lib = !ocaml_ext_lib;
    ocaml_ext_dll = !ocaml_ext_dll;
    ocaml_os_type = !ocaml_os_type;
    ocaml_ocamllib = !ocaml_ocamllib;
  }

let rec find_first_in_path path filter list =
  match list with
      [] -> None
    | basename :: others ->
      try
        let binary = find_in_PATH basename path in
        if filter binary then Some binary else raise Not_found
      with Not_found ->
        find_first_in_path path filter others

let check_is_compiler ocamlc_prefixes args ocamlc =
  let status, lines =
    try get_stdout_lines [ ocamlc ] args
    with e ->
      Printf.eprintf "Warning: could not execute %S\n%!" ocamlc;
      Printf.eprintf "\texception %S\n%!" (Printexc.to_string e);
      (-1, []) in
  if status = 0 then
    try
      match lines with
          first_line :: _ ->
            let prefix =
              let pos = String.index first_line ',' in
              String.sub first_line 0 pos
            in
            List.mem prefix ocamlc_prefixes
        | _ -> false
 with _ -> false
 else false

let ocamlc_prefixes = [
  "The Objective Caml compiler"; "The OCaml compiler"]
let ocamlopt_prefixes = [
  "The Objective Caml native-code compiler";
  "The OCaml native-code compiler"
]
let ocamldep_prefixes = [ "ocamldep" ]
let ocamllex_prefixes = [
  "The Objective Caml lexer generator";
  "The OCaml lexer generator" ]
let ocamlyacc_prefixes = [
  "The Objective Caml parser generator";
  "The OCaml parser generator" ]

let check_is_ocamlc = check_is_compiler ocamlc_prefixes  [ "-v" ]
let check_is_ocamlopt = check_is_compiler ocamlopt_prefixes  [ "-v" ]
let check_is_ocamllex = check_is_compiler ocamllex_prefixes  [ "-version" ]
let check_is_ocamldep = check_is_compiler ocamldep_prefixes [ "-version" ]
let check_is_ocamlyacc = check_is_compiler ocamlyacc_prefixes [ "-version" ]

let check_config pjo =

  if not (pjo.option_native || pjo.option_bytecode) then begin
    pjo.option_native <- true;
    pjo.option_bytecode <- true;
  end;

  if pjo.option_ocamlc = [] then
    pjo.option_ocamlc <- [ "ocamlc.opt" ; "ocamlc"];

  let path =
    if pjo.option_ocamlbin = "" then
      get_PATH ()
    else [ pjo.option_ocamlbin ]
  in

  let ocamlc = find_first_in_path path check_is_ocamlc pjo.option_ocamlc in
  let ocamlopt = find_first_in_path path check_is_ocamlopt pjo.option_ocamlopt in

  let cfg = match ocamlc, ocamlopt with
    | None, None ->
      Printf.eprintf "Error: could not find an OCaml compiler.\n";
      exit 2

    | Some ocamlc, None ->
      if pjo.option_native then begin
        if pjo.option_bytecode then begin
          Printf.eprintf "Warning: could not find an OCaml native code compiler\n";
          Printf.eprintf "\tDeactivating native code generation.\n";
        end
        else begin
          Printf.eprintf "Error: could not find an OCaml native code compiler\n";
          Printf.eprintf "\tbut you ask for native code only.\n";
          exit 2
        end;
      end;
      pjo.option_native <- false;
      set_strings_option ocamlc_cmd [ocamlc];
      set_strings_option ocamlcc_cmd [ocamlc];
      get_config ocamlc

    | None, Some ocamlopt ->
      if pjo.option_bytecode then begin
        if pjo.option_native then begin
          Printf.eprintf "Warning: could not find an OCaml bytecode compiler\n";
          Printf.eprintf "\tDesactivating bytecode generation.\n";
          exit 2
        end
        else begin
          Printf.eprintf "Error: could not find an OCaml bytecode compiler\n";
          Printf.eprintf "\tbut you ask for bytecode only.\n";
          exit 2
        end;
      end;
      pjo.option_bytecode <- false;
      set_strings_option ocamlopt_cmd [ocamlopt];
      set_strings_option ocamlcc_cmd [ocamlopt];
      get_config ocamlopt

    | Some ocamlc, Some ocamlopt ->
      let byte_config = get_config ocamlc in
      let native_config = get_config ocamlopt in
      if byte_config <> native_config then begin
        Printf.fprintf stderr "Warning: bytecode and native code compilers disagree on configuration%!\n";
        if pjo.option_bytecode && pjo.option_native then begin
          Printf.eprintf "\tDesactivating bytecode generation.\n";
          pjo.option_native <- false;
        end
      end;
      set_strings_option ocamlc_cmd [ocamlc];
      set_strings_option ocamlcc_cmd [ocamlc];
      set_strings_option ocamlopt_cmd [ocamlopt];
      byte_config
  in

  let ocamldep = find_first_in_path path
    check_is_ocamldep pjo.option_ocamldep in
  begin match ocamldep with
      None ->
        Printf.eprintf "Error: Could not find OCaml ocamldep tool.\n";
        exit 2
    | Some ocamldep ->
      set_strings_option ocamldep_cmd [ocamldep];
  end;

  let ocamllex = find_first_in_path path
    check_is_ocamllex pjo.option_ocamllex in
  begin match ocamllex with
      None ->
        Printf.eprintf "Error: Could not find OCaml ocamllex tool.\n";
        exit 2
    | Some ocamllex ->
      set_strings_option ocamllex_cmd [ocamllex];
  end;

  let ocamlyacc = find_first_in_path path
    check_is_ocamlyacc pjo.option_ocamlyacc in
  begin match ocamlyacc with
      None ->
        Printf.eprintf "Error: Could not find OCaml ocamlyacc tool.\n";
        exit 2
    | Some ocamlyacc ->
      set_strings_option ocamlyacc_cmd [ocamlyacc];
  end;

  set_strings_option ocaml_config_ext_lib [cfg.ocaml_ext_lib];
  set_strings_option ocaml_config_ext_obj [cfg.ocaml_ext_obj];
  set_strings_option ocaml_config_version [cfg.ocaml_version];
  set_strings_option ocaml_config_system [cfg.ocaml_system];
  set_strings_option ocaml_config_architecture [cfg.ocaml_architecture];
  set_strings_option ocaml_config_os_type [cfg.ocaml_os_type];
  set_strings_option ocaml_config_ext_dll [cfg.ocaml_ext_dll];

  BuildSubst.add_to_subst "OCAMLLIB" cfg.ocaml_ocamllib;

(*  Printf.fprintf stderr "SYSTEM = %s\n%!" cfg.ocaml_system; *)
  cfg


