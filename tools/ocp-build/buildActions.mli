(***********************************************************************)
(*                                                                     *)
(*                             ocp-build                               *)
(*                                                                     *)
(*  Copyright 2011-2012 OCamlPro SAS                                   *)
(*  Developed by OCamlPro, supervised by Fabrice LE FESSANT (INRIA)    *)
(*                                                                     *)
(***********************************************************************)


(* clean all generated object files *)
val do_clean : BuildEngineTypes.build_context -> unit

(* clean all generated/modified files *)
val do_distclean : unit -> unit
