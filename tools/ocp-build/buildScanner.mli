(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)


val scan_directory_for_suffix :
 (* directory *) string -> (* extension *) string ->
  (string -> unit) -> unit

val scan_directory_for_files :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

val scan_directory_for_extensions :
 (* directory *) string ->
 (* extensions handlers *)
  (string -> unit) StringMap.t ->
  unit

(*
val scan_directory_for_extensions2 :
 (* directory *) string ->
 (* extensions handlers *)
  (string ->  (* relative filename *)
   string ->  (* full filename *)
   unit) StringMap.t ->
  unit
*)

