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

open BuildOCPTypes
open BuildOCPVariable


(* These values are modified by reading the configuration *)
val ocamlc_cmd : string list source_option
val ocamlcc_cmd : string list source_option
val ocamllex_cmd : string list source_option
val ocamlyacc_cmd : string list source_option
val ocamldep_cmd : string list source_option
val ocamlopt_cmd : string list source_option

(*
val mklib_cmd : BuildTypes.mklib_kind ref
val ar_cmd : string ref
val ranlib_cmd : string ref
val libdirs : (string * string) list ref
*)

(* These values are global, but could be set per project, as we can
  change the compiler depending on that.

   TODO: Maybe we could even attach
  these values to a particular compiler, and cache them so that we
   can load them each time that compiler is used.. *)
val ocaml_config_version : string list source_option
val ocaml_config_system : string list source_option
val ocaml_config_architecture :  string list source_option
val ocaml_config_ext_obj :  string list source_option
val ocaml_config_ext_lib :  string list source_option
val ocaml_config_ext_dll :  string list source_option



(* Misc *)
val get_stdout_lines : string list -> string list -> int * string list

module TYPES : sig
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

val check_config : BuildOptions.project_options -> ocaml_config
