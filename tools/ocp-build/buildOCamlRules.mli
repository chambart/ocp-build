(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)


(* From the [validated_projects] table, fill the other
   tables *)
val create : BuildOCPTypes.project -> BuildEngineTypes.build_context -> unit
