(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

exception MissingSourceWithNoBuildingRule of BuildEngineTypes.build_rule * string

val stats_command_executed : int ref
val stats_files_generated : int ref

(* [init targets] Initialize the build engine, by checking activating all the rules
 needed for the creation of the files [targets].
   raise MissingSourceWithNoBuildingRule (rule, filename) if a file is needed as
      a source and no rule is available for generating it.
*)
val init :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_file list -> unit

val fatal_errors : unit -> string list list
val errors : unit -> string list list

(* [parallel_loop ncores] Start the build process on [ncores] cores. *)
val parallel_loop :
  BuildEngineTypes.build_context -> int -> unit


val sanitize : BuildEngineTypes.build_context -> BuildEngineTypes.delete_orphans -> int
