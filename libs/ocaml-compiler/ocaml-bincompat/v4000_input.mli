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

val input_cmi : in_channel -> Cmi_format.cmi_infos
val input_ast_intf :
  in_channel -> string * V4000_types.Parsetree.signature
val input_ast_impl :
  in_channel -> string * V4000_types.Parsetree.structure
