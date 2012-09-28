(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)

open BuildOCPTypes

val load_project : File.t list -> project * int

(* returns the number of errors while reading the files *)

val find_root : File.t -> string list -> File.t

(*
val save_project : File.t -> project -> unit
*)

val scan_root : File.t -> File.t list


(* [find_package pj file] returns the list of packages in
   project [pj] containing [file] as a source.
*)
val find_package : project -> File.t -> package list


val save_project_state : project -> File.t -> unit
val load_project_state : File.t -> project

val find_obuild : (string -> unit) -> string -> unit

