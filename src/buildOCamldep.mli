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


(* [load_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_dependencies : string -> (string * string list list) list

(* [load_modules_dependencies filename] returns a list of association between
 a target and a list of filenames, its dependencies. *)
val load_modules_dependencies : BuildTypes.package_info -> BuildOCPVariable.options ->           BuildOCamlTypes.force_kind ->

  BuildEngineTypes.build_directory -> string list -> string -> (string * string list list) list


val modname_of_file : BuildOCPVariable.options ->            BuildOCamlTypes.force_kind ->
string -> bool * string * string
