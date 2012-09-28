(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)



(* [find_installed dirname] generates a file 'ocp-installed.ocp' in
directory [dirname] with a list of already installed files, that
should not be regenerated. *)
val find_installed : string -> unit

(* [gen_from_distrib dirname] generates a list of files of the form
 'ocp-installed-*.ocp' in ~/.ocp/ corresponding to the descriptions
  of all the libraries in the current distribution. *)
val gen_from_distrib : string -> unit

(* [autogen dirname target]: generate file 'build.ocp' in [dirname]
  describing a project [target], built from all the source files inside
  the directory [dirname]. *)
val autogen : string -> string -> unit
*)

