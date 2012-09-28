(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

val add_dependency_loader :
  string -> (string -> ( string * string list list) list) -> unit
val find_dependency_loader :
  string -> (string -> ( string * string list list) list)


val new_dir_id : BuildEngineTypes.build_context -> int
val new_file_id : BuildEngineTypes.build_context -> int
val new_rule_id : BuildEngineTypes.build_context -> int
val new_process_id : BuildEngineTypes.build_context -> int

val file_filename : BuildEngineTypes.build_file -> string
(* val print_indented_command : BuildEngineTypes.build_action -> unit *)


