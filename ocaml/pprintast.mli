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

(* Printing code expressions *)

val print_structure : Format.formatter -> Parsetree.structure -> unit
val print_signature : Format.formatter -> Parsetree.signature -> unit

val string_of_expression : Parsetree.expression -> string
val signature_item :  Format.formatter -> Parsetree.signature_item -> unit

val value_description : Format.formatter -> Parsetree.value_description -> unit
val module_type : Format.formatter -> Parsetree.module_type -> unit

