(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open BuildOCPTypes

type config
type state

val initial_state : unit -> state
val final_state : state -> package array

val empty_config :(BuildOCPVariable.option_value StringMap.t ->
            BuildOCPVariable.option_value StringMap.t)
    list -> config

val translate_options :
  BuildOCPVariable.options ->
  BuildOCPTree.set_option list -> BuildOCPVariable.options

val add_project_dep: BuildOCPTypes.package -> bool -> string ->  string package_dependency

(*

module MakeParser(S : sig
  type project_info

  val new_project_id : unit -> int
  val register_project : project_info BuildOCPTypes.project -> unit
  val new_project :
    string ->
    string -> string -> project_info  BuildOCPTypes.project
  val register_installed : string -> unit

end) : sig


end

*)

val read_ocamlconf :  state -> config -> string -> config

